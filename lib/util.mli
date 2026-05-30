val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
val shuffle : 'a list -> 'a list
val insert : 'a list -> int -> 'a -> 'a list
val get_char : unit -> char
val last_opt : 'a list -> 'a option
val last : 'a list -> 'a
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
