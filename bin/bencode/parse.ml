module type Source = sig
  type t
  type c

  val empty_c : c
  val cat_c : c -> c -> c
  val consume_n : int -> t -> (c * t) option
end

module ByteSource = struct
  type t =
    { bs : bytes
    ; ptr : int
    }

  type c = bytes

  let of_string s = { bs = Bytes.of_string s; ptr = 0 }
  let empty_c = Bytes.empty
  let cat_c = Bytes.cat

  let consume_n n src =
    let next_ptr = src.ptr + n in
    if next_ptr <= Bytes.length src.bs
    then Some (Bytes.sub src.bs src.ptr n, { src with ptr = next_ptr })
    else None
  ;;
end

let parse_one
      (type a)
      (type b)
      (module S : Source with type t = a and type c = b)
      (pred : b -> bool)
      (src : a)
  =
  let opt_pred res = if pred (fst res) then Some res else None in
  let consume_opt = S.consume_n 1 src in
  Option.bind consume_opt opt_pred
;;

let parse_many
      (type a)
      (type b)
      (module S : Source with type t = a and type c = b)
      parser_fn
      (src : a)
  =
  let rec loop ok acc current_src =
    match parser_fn current_src with
    | Some (consumed, updated_src) -> loop true (S.cat_c acc consumed) updated_src
    | None -> ok, (acc, current_src)
  in
  match loop false S.empty_c src with
  | true, res -> Some res
  | false, _ -> None
;;

let parse_optional parser_fn src =
  let res =
    match parser_fn src with
    | None -> None, src
    | Some (consumed, updated_src) -> Some consumed, updated_src
  in
  Some res
;;

let parse_exact
      (type a)
      (type b)
      (module S : Source with type t = a and type c = b)
      n
      parser_fn
      src
  =
  let rec loop i acc current_src =
    if i < n
    then (
      match parser_fn current_src with
      | Some (consumed, updated_src) -> loop (i + 1) (S.cat_c acc consumed) updated_src
      | None -> None)
    else Some (acc, current_src)
  in
  loop 0 S.empty_c src
;;

let parse_byte b = parse_one (module ByteSource) (fun bs -> b = Bytes.get bs 0)

let parse_digit =
  parse_one (module ByteSource) (fun bs -> Char.Ascii.is_digit (Bytes.get bs 0))
;;

let parse_digits = parse_many (module ByteSource) parse_digit

(* TESTS *)
let%test "parse_one" =
  let src = ByteSource.of_string "i123" in
  let expected_bs = Bytes.of_string "i" in
  match parse_one (module ByteSource) (fun bs -> Bytes.get bs 0 = 'i') src with
  | Some (bs, updated_state) -> Bytes.equal expected_bs bs && updated_state.ptr = 1
  | None -> false
;;

let%test "parse_many" =
  let src = ByteSource.of_string "1234e" in
  let expected_bs = Bytes.of_string "1234" in
  match
    parse_many
      (module ByteSource)
      (parse_one (module ByteSource) (fun bs -> Char.Ascii.is_digit (Bytes.get bs 0)))
      src
  with
  | Some (bs, updated_src) -> Bytes.equal expected_bs bs && updated_src.ptr = 4
  | None -> false
;;
