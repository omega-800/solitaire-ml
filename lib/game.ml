open Cards
open Util

type player = {
  pos : int * int;
  grabbing : bool;
} [@@deriving show]

type state = {
  seen : card list;
  unseen : card list;
  stacks : card list array;
  top : card list array;
  p : player;
} [@@deriving show]

let arrange_stacks (cs : card list) : (card list array * card list) = 
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
  let (stacks, unseen) = arrange_stacks all_cards in
  {
    seen = [];
    unseen = unseen;
    stacks = stacks;
    top = [|[];[];[];[]|];
    p = {
      pos = (0,0);
      grabbing = false;
    }
  }
