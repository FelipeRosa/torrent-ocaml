module StringMap = Map.Make (String)

type t =
  | Int of int64
  | Bytestring of bytes
  | List of t list
  | Dict of t StringMap.t

let dict_bindings = function
  | Dict m -> StringMap.bindings m
  | _ -> raise (Invalid_argument "not a dict")
;;

let bytestring_bytes = function
  | Bytestring b -> b
  | _ -> raise (Invalid_argument "not a bytestring")
;;
