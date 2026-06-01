open Cards

(* TODO: do i really have to duplicate this? *)
type player = { pos : int * int; top: bool; grabbing : card list } [@@deriving show]

type state = {
  top : card list array;
  stacks : card list array;
  p : player;
}
[@@deriving show]

val init_state : state
val move : char -> state -> state
val cycle_grab : state -> state
val cycle_next_cards : state -> state
val open_card : state -> state
