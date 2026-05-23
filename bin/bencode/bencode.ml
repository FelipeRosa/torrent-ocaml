module Value = Value
module Parse = Parse
module ByteSource = Parse.ByteSource

let parse_int bs =
  let open Parse in
  let ( let* ) = Option.bind in
  let* _, bs = parse_byte 'i' bs in
  let* minus_sign, bs = parse_optional (parse_byte '-') bs in
  let* digits, bs = parse_digits bs in
  let* _, bs = parse_byte 'e' bs in
  let* parsed_int = Int64.of_string_opt (Bytes.to_string digits) in
  Some (Value.Int parsed_int, bs)
;;

let parse_bytestring bs =
  let open Parse in
  let ( let* ) = Option.bind in
  let* digits, bs = parse_digits bs in
  let* bytestring_len = int_of_string_opt (Bytes.to_string digits) in
  let* _, bs = parse_byte ':' bs in
  let* bytestring, bs =
    parse_exact
      (module ByteSource)
      bytestring_len
      (parse_one (module ByteSource) (Fun.const true))
      bs
  in
  Some (Value.Bytestring bytestring, bs)
;;

let rec parse_value bs =
  let parsers = [ parse_int; parse_bytestring; parse_list; parse_dict ] in
  let rec loop = function
    | [] -> None
    | p :: ps ->
      (match p bs with
       | None -> loop ps
       | Some res -> Some res)
  in
  loop parsers

and parse_list bs =
  let rec parse_many_values values bs =
    match parse_value bs with
    | None -> Some (values, bs)
    | Some (value, next_bs) -> parse_many_values (values @ [ value ]) next_bs
  in
  let open Parse in
  let ( let* ) = Option.bind in
  let* _, bs = parse_byte 'l' bs in
  let* values, bs = parse_many_values [] bs in
  let* _, bs = parse_byte 'e' bs in
  Some (Value.List values, bs)

and parse_dict bs =
  let ( let* ) = Option.bind in
  let parse_key_value bs =
    let* k, bs = parse_bytestring bs in
    let* v, bs = parse_value bs in
    Some (Bytes.to_string (Value.bytestring_bytes k), v, bs)
  in
  let rec parse_many_key_value kvs bs =
    match parse_key_value bs with
    | None -> Some (kvs, bs)
    | Some (k, v, bs) -> parse_many_key_value (Value.StringMap.add k v kvs) bs
  in
  let open Parse in
  let* _, bs = parse_byte 'd' bs in
  let* m, bs = parse_many_key_value Value.StringMap.empty bs in
  let* _, bs = parse_byte 'e' bs in
  Some (Value.Dict m, bs)
;;
