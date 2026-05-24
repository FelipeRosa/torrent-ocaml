module StringMap : Map.S with type key = string

type t

val of_int : int64 -> t
val of_bytes : bytes -> t
val of_string : string -> t
val of_list : t list -> t
val of_string_map : t StringMap.t -> t
val get_int : t -> int64
val get_bytestring : t -> bytes
val get_list : t -> t list
val get_dict : t -> t StringMap.t
