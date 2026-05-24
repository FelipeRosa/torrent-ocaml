module Dict : Map.S with type key = string

type t

val get_int : t -> int64
val get_bytestring : t -> bytes
val get_list : t -> t list
val get_dict : t -> t Dict.t
