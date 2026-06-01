open Cards
open Game
open Color
open Stdlib

type marked_stack = (card * color) list

let init_stack_marks = List.map (fun c -> (c, White))

let cprint c s =
  match c with
  | Red -> red s
  | Green -> green s
  | Blue -> blue s
  | Yellow -> yellow s
  | _ -> s

let sepstr = "+--------+"
let spcstr = "          "
let crdstr = "|        |"

let show_card_stack_v (cs : marked_stack) (emptyclr : color) : string list =
  (if List.length cs == 0 then [ cprint emptyclr sepstr ]
   else
     List.concat_map
       (fun (c, clr) ->
         let type_str, val_str = show_card c in
         let cp = cprint clr in
         [ cp sepstr; cp "|" ^ " (" ^ type_str ^ ") " ^ val_str ^ cp " |" ])
       cs)
  @
  let cp =
    match Util.last_opt cs with
    | Some (_, clr) -> cprint clr
    | None -> cprint emptyclr
  in
  [ cp crdstr; cp sepstr ]

let pcolor p g = (*if g then Green else*) if p then Yellow else White

let marked_stacks ({ stacks; top; p; _ } : state) :
    marked_stack array * marked_stack array =
  let stacks = Array.map init_stack_marks stacks in
  let top = Array.map init_stack_marks top in

  let x, y = p.pos in
  let curstacks = if p.top then top else stacks in
  let curstack = curstacks.(x) in
  let grabbing = List.map (fun c -> (c, Green)) p.grabbing in

  let nstack =
    (match List.nth_opt curstack y with
      | Some (curcard, _) ->
          Util.insert curstack y
            (curcard, pcolor true @@ not @@ List.is_empty p.grabbing)
      | None -> curstack)
    @ grabbing
  in

  curstacks.(x) <- nstack;
  if p.top then (stacks, curstacks) else (curstacks, top)

(* U | S |   | S | H | C | D *)
(* 1 | 2 | 3 | 4 | 5 | 6 | 7 *)

let print_state_v (s : state) : unit =
  let maxlen xs = Array.fold_left max 0 @@ Array.map List.length xs in
  let concat_rows xs =
    String.concat "\n"
    @@ List.mapi
         (fun i _ ->
           Array.fold_left (fun a cs -> a ^ " " ^ List.nth cs i) "" xs)
         xs.(0)
  in
  let pad_stack len cs =
    cs @ List.init (len - List.length cs) (Fun.const spcstr)
  in
  let pad_stacks len = Array.map (pad_stack len) in

  let stacks, top = marked_stacks s in
  let unseen, seen = Util.fsttwo top in

  let hacky_clr t x =
    if s.p.top == t && x == Pair.fst s.p.pos then
      pcolor true @@ not @@ List.is_empty s.p.grabbing
    else White
  in

  let top =
    Array.mapi (fun i cs ->
        let clr = hacky_clr true @@ (i + 2) in
        let cp = cprint clr in
        if List.is_empty cs then
          [
            cp sepstr;
            cp "|  >>="
            ^ show_card_type (Option.get @@ card_type_of_enum i)
            ^ cp "  |";
            cp sepstr;
          ]
        else show_card_stack_v cs clr)
    @@ Array.sub top 2 4
  in
  let ltop = maxlen top in
  let top_padded = pad_stacks ltop top in

  let single_card c emptyclr =
    match c with
    | Some (c, clr) ->
        let cp = cprint clr in
        let t, v = show_card c in
        [ cp sepstr; cp "|" ^ " (" ^ t ^ ") " ^ v ^ cp " |"; cp sepstr ]
    | None -> List.map (cprint emptyclr) [ sepstr; crdstr; sepstr ]
  in
  let top_placeholder =
    Array.map (pad_stack ltop)
      [|
        hacky_clr true 0 |> single_card @@ List.nth_opt unseen 0;
        hacky_clr true 1 |> single_card @@ List.nth_opt seen 0;
        [];
      |]
  in
  let top_combined = Array.append top_placeholder top_padded in

  let stacks =
    Array.mapi (fun i cs -> show_card_stack_v cs @@ hacky_clr false i) stacks
  in
  let lstacks = maxlen stacks in
  let stacks_padded = pad_stacks lstacks stacks in

  print_endline @@ concat_rows top_combined ^ "\n\n" ^ concat_rows stacks_padded
(* print_endline @@ concat_rows top_combined ^ concat_rows stacks *)
(* print_endline @@ String.concat "\n" @@ Array.to_list @@ Array.map (Fun.compose Int.to_string List.length) top_combined *)
(* print_endline @@ String.concat "\n" @@ Array.to_list @@ Array.map (String.concat "@") stacks_padded *)
(* print_endline @@ show_state s *)

(*
let rec show_card_stack_h (cs : card list) : string list =
  let open List in
  let l = length cs in
  let ts, vs = split @@ map show_card cs in
  [
    " +" ^ (String.concat "+" @@ init l (fun _ -> "-")) ^ "-+";
    " |" ^ String.concat "|" ts ^ " |";
    " |" ^ String.concat "|" vs ^ " |";
    " +" ^ (String.concat "+" @@ init l (fun _ -> "-")) ^ "-+";
  ]

let print_state_h (s : state) : unit =
  let maxlen xs =
    xs
    |> Array.map @@ Fun.compose String.length @@ Fun.flip List.nth 0
    |> Array.fold_left max 0
  in

  let stacks, top = marked_stacks s in 
  let top = Array.map show_card_stack_h s.top in
  let ltop = maxlen top in
  let top =
    top
    |> Array.map
       @@ List.map (fun s -> s ^ String.make (ltop - String.length s) ' ')
  in
  let pad = Array.make 3 @@ List.init 4 @@ Fun.const @@ String.make ltop ' ' in
  let stacks = Array.map show_card_stack_h s.stacks in
  let concat_rows xs ys =
    Array.combine xs ys
    |> Array.fold_left
         (fun a (x, y) ->
           List.combine x y
           |> List.fold_left (fun a' (x', y') -> a' ^ x' ^ y' ^ "\n") a)
         ""
  in
  print_endline @@ concat_rows (Array.append pad top) stacks
  *)
