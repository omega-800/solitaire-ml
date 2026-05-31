open Cards
open Util

type player = { pos : int * int; top : bool; grabbing : bool } [@@deriving show]

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
    p = { pos = (0, 6); top = false; grabbing = false };
  }

let set_cur_stack (s : state) (n : card list) : state =
  let { p; stacks; top; _ } = s in
  let x, _ = p.pos in
  let curstacks = if p.top then top else stacks in
  curstacks.(x) <- n;
  s

let cur_stack ({ p; stacks; top; _ } : state) : card list =
  let x, _ = p.pos in
  let curstacks = if p.top then top else stacks in
  curstacks.(x)

let cur_card (s : state) : card option =
  Pair.snd s.p.pos |> List.nth_opt @@ cur_stack s

let move_cards (s : state) : state = s

let cycle_grab (s : state) : state =
  (* TODO: *)
  let c = cur_card s in
  let x, y = s.p.pos in
  (* let curstack = cur_stack s in *)
  (* let rest, grabbing = Util.split curstack @@ y in *)
  let valid_grab =
    Stdlib.not s.p.grabbing
    && Util.some_and (fun (_, _, s) -> s) c
    && (x > 0 || Stdlib.not s.p.top)
  in
  let valid_ungrab =
    s.p.grabbing
    && Util.some_and (fun (_, _, s) -> s) c
    && (x > 1 || Stdlib.not s.p.top)
  in
  let grab_cards =
    { s (* (set_cur_stack s rest) *) with p = { s.p with grabbing = true } }
  in
  let ungrab_cards = { s with p = { s.p with grabbing = false } } in
  if valid_grab then grab_cards else if valid_ungrab then ungrab_cards else s

(* TODO: state *)
let move (s : state) (c : char) : state =
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
      let nx' = ubc nx n_max_x in
      let n_max_y = List.length nstacks.(nx') in
      let top = Stdlib.not p.top in
      let ny' = ubc y n_max_y in
      (top, nx', if nx' < 2 && top then 0 else ny')
    else (p.top, x, if x < 2 && p.top then cur_max_y else ny')
  in

  let p = { p with pos = (px, py); top } in
  let s = { s with p } in
  if p.grabbing then move_cards s else s

let cycle_next_cards (s : state) : state =
  let unseen, seen = Util.fsttwo s.top in
  let unseen, seen =
    if List.is_empty unseen then (seen, unseen) else Util.move_n unseen seen 1
  in
  (* who cares about performance anyway *)
  let seen = List.map (fun (t, v, _) -> (t, v, true)) seen in
  let unseen = List.map (fun (t, v, _) -> (t, v, false)) unseen in
  { s with top = Util.setfsttwo s.top unseen seen }
