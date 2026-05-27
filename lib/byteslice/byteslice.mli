type t

val empty : t
val of_bytes : bytes -> int -> int -> t
val of_string : string -> int -> int -> t
val length : t -> int
val get : t -> int -> char
val sub : t -> int -> int -> t
val to_bytes : t -> bytes
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
