let cartesian_product (xs : 'a list) (ys : 'b list) : ('a * 'b) list = 
  let open List in concat (map (fun x -> map (fun y -> (x,y)) ys) xs)

let shuffle (d : 'a list) : 'a list =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let insert (xs : 'a list) (i : int) (x : 'a) : 'a list = 
  let open List in 
  let xh = take i xs in 
  let xt = drop (length xs - i) xs in 
  append xh @@ append [x] xt

let get_char () : char =
    let open Unix in
    let termio = tcgetattr stdin in
    tcsetattr stdin TCSAFLUSH
      { termio with c_icanon = false; c_echo = false };
    let c = input_char (in_channel_of_descr stdin) in
    tcsetattr stdin TCSAFLUSH termio;
    c;;
