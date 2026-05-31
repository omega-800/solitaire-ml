let cartesian_product (xs : 'a list) (ys : 'b list) : ('a * 'b) list =
  let open List in
  concat (map (fun x -> map (fun y -> (x, y)) ys) xs)

let shuffle (d : 'a list) : 'a list =
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let split (xs : 'a list) (i : int) : 'a list * 'a list =
  let open List in
  (take i xs, drop i xs)

let insert (xs : 'a list) (i : int) (x : 'a) : 'a list =
  let open List in
  let xh = take i xs in
  let xt = drop (i + 1) xs in
  xh @ [ x ] @ xt

let last_opt (xs : 'a list) : 'a option =
  List.nth_opt xs @@ Stdlib.max 0 @@ (List.length xs - 1)

let last (xs : 'a list) : 'a =
  List.nth xs @@ Stdlib.max 0 @@ (List.length xs - 1)

let move_n (f : 'a list) (t : 'a list) (n : int) : 'a list * 'a list =
  let e, f = split f n in
  (f, e @ t)

let get_char () : char =
  let open Unix in
  let termio = tcgetattr stdin in
  tcsetattr stdin TCSAFLUSH { termio with c_icanon = false; c_echo = false };
  let c = input_char (in_channel_of_descr stdin) in
  tcsetattr stdin TCSAFLUSH termio;
  c

let uncurry f (x, y) = f x y
let curry f x y = f (x, y)

let fsttwo (xs : 'a list array) : 'a list * 'a list = (xs.(0), xs.(1))
let setfsttwo (xs : 'a list array) (ys : 'a list) (zs : 'a list) : 'a list array
    =
  xs.(0) <- ys;
  xs.(1) <- zs;
  xs

let some_and f o = 
  match o with 
    | Some v -> f v
    | None -> false

let none_or f o = 
  match o with 
    | Some v -> f v
    | None -> true
