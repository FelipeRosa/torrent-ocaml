module Parsing = Parsing
module Value = Value

let ( let* ) = Option.bind

let parse_int bs =
  let open Parsing in
  let* _, bs' = parse_char 'i' bs in
  let* minus_sign, bs' = optional (parse_char '-') bs' in
  let* digits, bs' = parse_bytes_many parse_bytes_digit bs' in
  let* _, bs' = parse_char 'e' bs' in
  let* int_v = digits |> String.of_bytes |> Int64.of_string_opt in
  Some (Value.of_int (if Option.is_some minus_sign then Int64.neg int_v else int_v), bs')
;;

let parse_bytestring bs =
  let open Parsing in
  let* len_digits, bs' = parse_bytes_many parse_bytes_digit bs in
  let* _, bs' = parse_char ':' bs' in
  let* bytestring, bs' =
    parse_bytes_exact
      (len_digits |> String.of_bytes |> int_of_string)
      (parse_bytes_one (Fun.const true))
      bs'
  in
  Some (Value.of_bytes bytestring, bs')
;;

let rec parse_value bs =
  let parsers = [ parse_int; parse_bytestring; parse_list; parse_dict ] in
  let rec loop = function
    | [] -> None
    | parser_fn :: ps ->
      (match parser_fn bs with
       | None -> loop ps
       | res -> res)
  in
  loop parsers

and parse_list bs =
  let open Parsing in
  let* _, bs' = parse_char 'l' bs in
  let* val_list, bs' = parse_many parse_value bs' in
  let* _, bs' = parse_char 'e' bs' in
  Some (Value.of_list val_list, bs')

and parse_dict bs =
  let open Parsing in
  let parse_kv bs =
    let* k, bs' = parse_bytestring bs in
    let* v, bs' = parse_value bs' in
    Some ((String.of_bytes (Value.get_bytestring k), v), bs')
  in
  let* _, bs' = parse_char 'd' bs in
  let* kvs, bs' = parse_many parse_kv bs' in
  let* _, bs' = parse_char 'e' bs' in
  Some (Value.of_string_map (Value.StringMap.of_list kvs), bs')
;;

(* TESTS *)
let%test "parse_value int" =
  let bs = Parsing.ByteSource.of_string "i45e1:a" in
  match parse_value bs with
  | Some (v, _) -> Value.get_int v = Int64.of_int 45
  | _ -> false
;;

let%test "parse_value bytestring" =
  let bs = Parsing.ByteSource.of_string "3:abci32e" in
  match parse_value bs with
  | Some (v, _) -> Value.get_bytestring v = Bytes.of_string "abc"
  | _ -> false
;;

let%test "parse_value list" =
  let bs = Parsing.ByteSource.of_string "li1ei2e3:abce" in
  let verify_list l =
    let* e0 = List.nth_opt l 0 in
    let* e1 = List.nth_opt l 1 in
    let* e2 = List.nth_opt l 2 in
    Some
      (Value.get_int e0 = Int64.of_int 1
       && Value.get_int e1 = Int64.of_int 2
       && Bytes.equal (Value.get_bytestring e2) (Bytes.of_string "abc"))
  in
  match parse_value bs with
  | Some (l, _) -> verify_list (Value.get_list l) = Some true
  | _ -> false
;;

let%test "parse_value dict" =
  let bs = Parsing.ByteSource.of_string "d1:ai1e1:b3:defe" in
  let verify_dict d =
    let* e0 = Value.StringMap.find_opt "a" d in
    let* e1 = Value.StringMap.find_opt "b" d in
    Some
      (Value.get_int e0 = Int64.one
       && Bytes.equal (Value.get_bytestring e1) (Bytes.of_string "def"))
  in
  match parse_value bs with
  | Some (d, _) -> verify_dict (Value.get_dict d) = Some true
  | _ -> false
;;
