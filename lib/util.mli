val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
val shuffle : 'a list -> 'a list
val insert : 'a list -> int -> 'a -> 'a list
val get_char : unit -> char
val split : 'a list -> int -> 'a list * 'a list
val last_opt : 'a list -> 'a option
val last : 'a list -> 'a
val move_n : 'a list -> 'a list -> int -> 'a list * 'a list
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val fsttwo : 'a list array -> 'a list * 'a list
val setfsttwo : 'a list array -> 'a list -> 'a list -> 'a list array
val some_and : ('a -> bool) -> 'a option -> bool
val none_or : ('a -> bool) -> 'a option -> bool
