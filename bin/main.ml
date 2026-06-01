open Solitaire_ml

let rec loopedy_loop (s : Game.state) : unit =
  print_string "\027[2J";
  print_string "\027[H";
  flush stdout;
  print_endline "h,j,k,l to move";
  print_endline "g to grab/ungrab";
  print_endline "n for next card";
  print_endline "o to open hidden card";
  print_endline "q to quit";
  (s.p.pos
  |> Util.uncurry @@ Printf.printf "position: (%d, %d) t[%b] s@[%d] g@[%d]\n")
    s.p.top
    (List.length s.top.(1))
  @@ List.length s.p.grabbing;

  Print.print_state_v s;
  let c = Util.get_char () in
  if c == 'q' then ()
  else
    loopedy_loop
    @@
    (match c with
    | 'g' -> Game.cycle_grab
    | 'o' -> Game.open_card
    | 'n' -> Game.cycle_next_cards
    | c -> Game.move c) s

let () = loopedy_loop Game.init_state
