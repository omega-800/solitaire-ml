open Solitaire_ml

let has_won ({ top; stacks; _ } : Game.state) : bool =
  List.is_empty top.(0)
  && List.is_empty top.(1)
  && Array.for_all List.is_empty stacks

let rec loopedy_loop (s : Game.state) : unit =
  print_string "\027[2J";
  print_string "\027[H";
  flush stdout;

  if has_won s then (
    print_endline "\n\nYou won! Play again? (y/n)\n\n";
    Print.print_state_v s;
    let c = Util.get_char () in
    match c with
    | 'q' | 'n' -> ()
    | 'y' -> loopedy_loop Game.init_state
    | _ -> loopedy_loop s)
  else (
    print_endline " [h,j,k,l] to move     | [g] to grab/ungrab     | [n] for next card";
    print_endline " [q] to quit           | [u] to undo grab       | [o] to open hidden card";

    Print.print_state_v s;
    let c = Util.get_char () in
    if c == 'q' then ()
    else
      loopedy_loop
      @@ (match c with
         | 'g' -> Game.cycle_grab
         | 'u' -> Game.undo
         | 'o' -> Game.open_card
         | 'n' -> Game.cycle_next_cards
         | c -> Game.move c)
           s)

let () = loopedy_loop Game.init_state
