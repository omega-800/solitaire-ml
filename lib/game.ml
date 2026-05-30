open Cards
open Util

type player = { pos : int * int; top : bool; grabbing : bool } [@@deriving show]

type state = {
  seen : card list;
  unseen : card list;
  stacks : card list array;
  top : card list array;
  p : player;
}
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
  |> Pair.map_snd open_first_card

let init_state : state =
  let all_cards = shuffle all_cards in
  let stacks, unseen = arrange_stacks all_cards in
  {
    seen = [];
    unseen;
    stacks;
    top = [| []; []; []; [] |];
    p = { pos = (0, 0); top = false; grabbing = false };
  }

(* TODO: state *)
let move (s : state) (c : char) : state =
  let { p; stacks; top; _ } = s in
  let x, y = p.pos in
  let x, y =
    match c with
    | 'h' -> (x - 1, y)
    | 'j' -> (x, y + 1)
    | 'k' -> (x, y - 1)
    | 'l' -> (x + 1, y)
    | _ -> (x, y)
  in
  let curstacks = if p.top then top else stacks in
  let nstacks = if p.top then stacks else top in
  let ubc t v =
    if v < 0 then if t > 0 then t - 1 else 0 else if t <= v then 0 else v
  in
  let x = x |> ubc @@ Array.length curstacks in
  let curstack = curstacks.(x) in
  let ny = y |> ubc @@ List.length curstack in
  let top, y, x =
    if y != ny then
      ( Stdlib.not p.top,
        0,
        Stdlib.min x @@ Stdlib.max 0 @@ (Array.length nstacks - 1) )
    else (p.top, y, x)
  in
  let p = { p with pos = (x, y); top } in
  { s with p }
