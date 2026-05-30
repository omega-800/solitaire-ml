open Solitaire_ml

let rec loopedy_loop (s : Game.state) : unit =
  print_string "\027[2J";
  print_string "\027[H";
  flush stdout;
  print_endline "h,j,k,l to move";
  print_endline "g to grab/ungrab";
  print_endline "n for next card";
  print_endline "q to quit";
  (s.p.pos |> Util.uncurry @@ Printf.printf "position: (%d, %d) t[%b]\n") s.p.top;
  Print.print_state_v s;
  let c = Util.get_char () in
  if c == 'q' then ()
  else loopedy_loop @@ match c with 'g' -> s | 'n' -> s | c -> Game.move s c

let () =
  (* TODO: *)
  Random.self_init ();
  loopedy_loop Game.init_state
