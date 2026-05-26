type t

val compare : t -> t -> int
val of_bytes : bytes -> int -> int -> t
val of_string : string -> int -> int -> t
val get : t -> int -> char
