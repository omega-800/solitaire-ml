(* TODO: do i really have to duplicate this? *)
type card_type =  Spade | Heart | Club | Diamond [@@deriving enum, show]

type card_val =
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | N10
  | Jack
  | Queen
  | King
  | Ace
[@@deriving enum, show]

type card = card_type * card_val * bool [@@deriving show]

val show_card_val : card_val -> string
val show_card_type : card_type -> string
val show_card : card -> string * string
val all_cards : card list
val open_first_card : card list -> card list
val is_seen : card -> bool
val is_red : card_type -> bool
