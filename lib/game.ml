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

let rec move (c : char) (s : state) : state =
  let { p; stacks; top; _ } = s in
  let x, y = p.pos in
  let h = c == 'h' || c == 'l' in
  let nx, ny =
    match c with
    | 'h' -> (x - 1, y)
    | 'j' -> (x, y + 1)
    | 'k' -> (x, y - 1)
    | 'l' -> (x + 1, y)
    | _ -> (x, y)
  in

  let oob v t = v < 0 || t <= v in
  let ubc ?(is_y = false) v t =
    let topmost = if t > 0 then t - 1 else 0 in
    if v < 0 then topmost
    else if t <= v then if h && is_y then topmost else 0
    else v
  in

  let curstacks = if p.top then top else stacks in
  let cur_max_x = Array.length curstacks in
  let nx' = ubc nx cur_max_x in
  let cur_max_y = if p.top && nx' < 2 then 0 else List.length curstacks.(nx') in
  let ny' = ubc ~is_y:true ny cur_max_y in

  let top, px, py =
    if h then (p.top, nx', ny')
    else if oob ny cur_max_y then
      let nstacks = if p.top then stacks else top in
      let n_max_x = Array.length nstacks in
      let top = not p.top in
      let nx' = if nx > 1 then if top then nx - 1 else nx + 1 else nx in
      (* not necessary i guess *)
      let nx' = ubc nx' n_max_x in
      let n_max_y = List.length nstacks.(nx') in
      let ny' = ubc y n_max_y in
      (top, nx', if nx' < 2 && top then 0 else ny')
    else (p.top, x, if x < 2 && p.top then cur_max_y else ny')
  in

  let p = { p with pos = (px, py); top } in
  let s = { s with p } in

  let valid_grab =
    List.is_empty p.grabbing || (((not p.top) || px != 1) && is_cur_card_last s)
  in
  let valid_pos =
    is_cur_card_last s || (Util.none_or (fun (_, _, s') -> s') @@ cur_card s)
  in
  if valid_grab && valid_pos then s else move 'j' s

let is_valid_ungrab (s : state) : bool =
  let x, _ = s.p.pos in
  let gt, gv, _ = List.nth s.p.grabbing 0 in
  let is_valid_top (t, v, s') =
    List.length s.p.grabbing == 1
    && gt == t
    && card_val_to_enum v - card_val_to_enum gv == 1
    && card_type_to_enum t == x - 2
  in
  let is_valid_stack (t, v, s') =
    Cards.is_red gt != Cards.is_red t
    && card_val_to_enum v - card_val_to_enum gv == 1
  in
  (not @@ List.is_empty s.p.grabbing)
  && (x > 1 || not s.p.top)
  &&
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
