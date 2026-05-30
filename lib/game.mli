open Cards

(* TODO: do i really have to duplicate this? *)
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

val init_state : state 
val arrange_stacks : card list -> card list array * card list
