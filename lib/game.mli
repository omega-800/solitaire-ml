open Cards

(* TODO: do i really have to duplicate this? *)
type player = { pos : int * int; top: bool; grabbing : bool } [@@deriving show]

type state = {
  seen : card list;
  unseen : card list;
  stacks : card list array;
  top : card list array;
  p : player;
}
[@@deriving show]

val init_state : state
val move : state -> char -> state
