open Cards
open Util
open Stdlib

type player = { pos : int * int; top : bool; grabbing : card list }
[@@deriving show]

type state = { top : card list array; stacks : card list array; p : player }
[@@deriving show]

let arrange_stacks (cs : card list) : card list array * card list =
  let ns = List.init 7 (fun x -> x + 1) in
  let fn (stacks, cs') n =
    let ch = List.take n cs' in
    let ct = List.drop n cs' in
    (ch :: stacks, ct)
  in
  List.fold_left fn ([], cs) ns
  |> Pair.map_fst @@ Fun.compose Array.of_list (List.map open_first_card)

let init_state : state =
  let all_cards = shuffle all_cards in
  let stacks, unseen = arrange_stacks all_cards in
  {
    top = [| unseen; []; []; []; []; [] |];
    stacks;
    p = { pos = (0, 6); top = false; grabbing = [] };
  }

let set_cur_stack (s : state) (n : card list) : state =
  let { p; stacks; top; _ } = s in
  let x, _ = p.pos in
  let curstacks = if p.top then top else stacks in
  curstacks.(x) <- n;
  s

let add_to_cur_stack (s : state) (n : card list) : state =
  let { p; stacks; top; _ } = s in
  let x, _ = p.pos in
  let curstacks = if p.top then top else stacks in
  curstacks.(x) <- curstacks.(x) @ n;
  s

let cur_stack ({ p; stacks; top; _ } : state) : card list =
  let x, _ = p.pos in
  let curstacks = if p.top then top else stacks in
  curstacks.(x)

let cur_card (s : state) : card option =
  Pair.snd s.p.pos |> List.nth_opt @@ cur_stack s

let is_cur_card_last (s : state) : bool =
  let len = List.length @@ cur_stack s in
  Pair.snd s.p.pos + 1 == len || len == 0

let inside_bounds ({ p; stacks; top; _ } : state) : bool =
  let curstacks = if p.top then top else stacks in
  let x, y = p.pos in
  x >= 0 && y >= 0
  && Array.length curstacks > x
  && (List.length curstacks.(x) > y || y == 0)

let is_pos_valid (s : state) : bool =
  let x, y = s.p.pos in
  let is_grabbing = not @@ List.is_empty s.p.grabbing in
  ((not s.p.top) || x > if is_grabbing then 1 else 0)
  &&
  if is_grabbing then is_cur_card_last s
  else is_cur_card_last s || Util.none_or Cards.is_seen @@ cur_card s

let set_pos s x' y' = { s with p = { s.p with pos = (x', y') } }

let rec adjust_y_pos (s : state) : state =
  let x, y = s.p.pos in
  let set_y = set_pos s x in
  let curstacks = if s.p.top then s.top else s.stacks in
  let max_y = List.length curstacks.(x) - 1 in
  if not @@ inside_bounds s then max 0 y - 1 |> set_y |> adjust_y_pos
  else if not @@ is_pos_valid s then min max_y y + 1 |> set_y |> adjust_y_pos
  else s

let rec move (c : char) (s : state) : state =
  let { p; stacks; top; _ } = s in
  let x, y = p.pos in
  let h = c == 'h' || c == 'l' in
  let is_grabbing = not @@ List.is_empty s.p.grabbing in
  let nx, ny =
    match c with
    | 'h' -> (x - 1, y)
    | 'j' -> (x, y + 1)
    | 'k' -> (x, y - 1)
    | 'l' -> (x + 1, y)
    | _ -> (x, y)
  in
  let setp = set_pos s in

  if h then
    let curstacks = if p.top then top else stacks in
    let max_x = Array.length curstacks - 1 in
    let min_x = if p.top then if is_grabbing then 2 else 1 else 0 in
    let nx' = if nx > max_x then min_x else if nx < min_x then max_x else nx in
    adjust_y_pos @@ setp nx' ny
  else if
    let s' = setp nx ny in
    (not @@ inside_bounds s') || (not @@ is_pos_valid s')
  then
    let nx' = if nx > 1 then nx + if p.top then 1 else -1 else nx in
    let ny' = if ny < 0 then 0 else ny in
    let top = x > 0 && not p.top in
    let s' = setp nx' ny' in
    adjust_y_pos @@ { s' with p = { s'.p with top } }
  else setp nx ny

let is_valid_ungrab (s : state) : bool =
  let x, _ = s.p.pos in
  (not @@ List.is_empty s.p.grabbing)
  && (x > 1 || not s.p.top)
  &&
  let gt, gv, _ = List.nth s.p.grabbing 0 in
  let is_valid_top (t, v, s') =
    List.length s.p.grabbing == 1
    && gt == t
    && card_val_to_enum gv - card_val_to_enum v == 1
    && card_type_to_enum t == x - 2
  in
  let is_valid_stack (t, v, s') =
    Cards.is_red gt != Cards.is_red t
    && card_val_to_enum v - card_val_to_enum gv == 1
  in
  match cur_card s with
  | None -> (not s.p.top) || (card_type_to_enum gt == x - 2 && gv == Ace)
  | Some c ->
      (not @@ Cards.is_seen c)
      || (if s.p.top then is_valid_top else is_valid_stack) c

(* TODO: let player ungrab card taken from seen *)
let ungrab_cards (s : state) : state =
  move 'k'
    { (add_to_cur_stack s s.p.grabbing) with p = { s.p with grabbing = [] } }

let is_valid_grab (s : state) : bool =
  List.is_empty s.p.grabbing
  && (Util.some_and Cards.is_seen @@ cur_card s)
  && ((not s.p.top) || is_cur_card_last s || Pair.fst s.p.pos == 1)

let grab_cards (s : state) : state =
  let curstack = cur_stack s in
  let x, y = s.p.pos in
  let takes_seen = s.p.top && x == 1 in
  let rest, grabbing = Util.split curstack @@ if takes_seen then 1 else y in
  let rest, grabbing =
    if takes_seen then (grabbing, rest) else (rest, grabbing)
  in
  { (set_cur_stack s rest) with p = { s.p with grabbing } }

let cycle_grab (s : state) : state =
  if is_valid_grab s then grab_cards s
  else if is_valid_ungrab s then ungrab_cards s
  else s

let cycle_next_cards (s : state) : state =
  let unseen, seen = Util.fsttwo s.top in
  let unseen, seen =
    if List.is_empty unseen then (seen, unseen) else Util.move_n unseen seen 1
  in
  (* who cares about performance anyway *)
  let seen = List.map (fun (t, v, _) -> (t, v, true)) seen in
  let unseen = List.map (fun (t, v, _) -> (t, v, false)) unseen in
  { s with top = Util.setfsttwo s.top unseen seen }

let open_card (s : state) : state =
  match cur_card s with
  | None -> s
  | Some (t, v, s') ->
      if s' then s else set_cur_stack s @@ open_first_card @@ cur_stack s
