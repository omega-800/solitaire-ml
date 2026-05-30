open Cards
open Game
open Color

type marked_stack = (card * color) list

let init_stack_marks = List.map (fun c -> (c, White))

let cprint c s =
  match c with
  | Red -> red s
  | Green -> green s
  | Blue -> blue s
  | Yellow -> yellow s
  | _ -> s

let show_card_stack_v (cs : marked_stack) : string list =
  (if List.length cs == 0 then [ "+-------+" ]
   else
     List.concat_map
       (fun (c, clr) ->
         let type_str, val_str = show_card c in
         let cp = cprint clr in
         [ cp "+-------+"; cp "| (" ^ type_str ^ cp ") " ^ val_str ^ cp " |" ])
       cs)
  @
  let cp =
    match Util.last_opt cs with
    | Some (_, clr) -> cprint clr
    | None -> cprint White
  in
  [ cp "|       |"; cp "+-------+" ]

let marked_stacks ({ stacks; top; p; _ } : state) :
    marked_stack array * marked_stack array =
  let stacks = Array.map init_stack_marks stacks in
  let top = Array.map init_stack_marks top in

  let x, y = p.pos in
  let curstacks = if p.top then top else stacks in
  let curstack = curstacks.(x) in
  let nstack =
    match List.nth_opt curstack y with
    | Some (curcard, _) -> Util.insert curstack y (curcard, Green)
    | None -> curstack
  in

  curstacks.(x) <- nstack;
  if p.top then (stacks, curstacks) else (curstacks, top)

(* U | S |   | S | H | C | D *)
(* 1 | 2 | 3 | 4 | 5 | 6 | 7 *)

let print_state_v (s : state) : unit =
  let maxlen xs = Array.fold_left Stdlib.max 0 @@ Array.map List.length xs in
  let concat_rows xs =
    String.concat "\n"
    @@ List.mapi
         (fun i _ ->
           Array.fold_left (fun a cs -> a ^ " " ^ List.nth cs i) "" xs)
         xs.(0)
  in
  let pad_stacks len =
    Array.map (fun cs ->
        cs @ List.init (len - List.length cs) (Fun.const "         "))
  in

  let stacks, top = marked_stacks s in
  let top = Array.map show_card_stack_v top in
  let ltop = maxlen top in
  let top_placeholder =
    Array.make 3 @@ List.init ltop @@ Fun.const "         "
  in
  let top_padded = pad_stacks ltop top in
  let top_combined = Array.append top_placeholder top_padded in

  let stacks = Array.map show_card_stack_v stacks in
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
    |> Array.fold_left Stdlib.max 0
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
