open Util

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

let show_card_val (cval : card_val) : string =
  (if cval == N10 then "" else " ")
  ^
  match cval with
  | Ace -> "A"
  | King -> "K"
  | Queen -> "Q"
  | Jack -> "J"
  | N10 -> "10"
  | N9 -> "9"
  | N8 -> "8"
  | N7 -> "7"
  | N6 -> "6"
  | N5 -> "5"
  | N4 -> "4"
  | N3 -> "3"
  | N2 -> "2"

let show_card_type (ctype : card_type) : string =
  match ctype with
  | Diamond -> Color.red "D"
  | Heart -> Color.red "H"
  | Spade -> Color.blue "S"
  | Club -> Color.blue "C"

let show_card ((t, v, s) : card) : string * string =
  if s then (show_card_type t, show_card_val v) else ("?", " ?")

let all_of_card_type : card_type list =
  List.init (max_card_type + 1) (fun c -> card_type_of_enum c |> Option.get)

let all_of_card_val : card_val list =
  List.init (max_card_val + 1) (fun c -> card_val_of_enum c |> Option.get)

let all_cards : card list =
  cartesian_product all_of_card_type all_of_card_val
  |> List.map (fun (t, v) -> (t, v, false))

let open_first_card (cs : card list) : card list =
  match cs with
  | [] -> []
  | _ ->
      let i = List.length cs - 1 in
      let t, v, _ = List.nth cs i in
      Util.insert cs i (t, v, true)

let is_seen ((_, _, s) : card) : bool = s

let is_red (t: card_type) : bool = t == Diamond || t == Heart
