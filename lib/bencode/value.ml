module Dict = Map.Make (String)

type t =
  | Int of int64
  | Bytestring of bytes
  | List of t list
  | Dict of t Dict.t

let get_int = function
  | Int i -> i
  | _ -> raise (Invalid_argument "not an int")
;;

let get_bytestring = function
  | Bytestring bs -> bs
  | _ -> raise (Invalid_argument "not a bytestring")
;;

let get_list = function
  | List l -> l
  | _ -> raise (Invalid_argument "not a list")
;;

let get_dict = function
  | Dict d -> d
  | _ -> raise (Invalid_argument "not a dict")
;;
