module BS = Byteslice

module ByteSource = struct
  type t =
    { buf : bytes
    ; ptr : int
    }

  let of_string s = { buf = Bytes.of_string s; ptr = 0 }

  let peek bs =
    if bs.ptr < Bytes.length bs.buf then Some (Bytes.get bs.buf bs.ptr) else None
  ;;

  let consume_slice n bs =
    if bs.ptr + n <= Bytes.length bs.buf
    then BS.of_bytes bs.buf bs.ptr n, { bs with ptr = bs.ptr + n }
    else raise (Invalid_argument "source already fully consumed")
  ;;

  let advance n bs =
    let end_ptr = bs.ptr + n in
    if end_ptr <= Bytes.length bs.buf
    then { bs with ptr = bs.ptr + n }
    else raise (Invalid_argument "source already fully advanced")
  ;;

  (* TESTS *)
  let%test "peek success" = of_string "abc" |> peek |> Option.equal ( = ) (Some 'a')
  let%test "peek fail" = of_string "" |> peek |> Option.is_none

  let%test "consume_slice" =
    let bs = of_string "abcde" in
    let sl, bs' = consume_slice 3 bs in
    BS.to_string sl = "abc" && bs'.ptr = 3
  ;;

  let%test "advance" = of_string "abc" |> advance 1 |> peek |> ( = ) (Some 'b')
end

type 'a parser_fn = ByteSource.t -> ('a * ByteSource.t) option

let ( let* ) = Option.bind

let parse_one pred bs =
  let* b = ByteSource.peek bs in
  if pred b then Some (b, ByteSource.advance 1 bs) else None
;;

let parse_bytes_one pred bs =
  let* b = ByteSource.peek bs in
  if pred b
  then (
    let sl, bs' = ByteSource.consume_slice 1 bs in
    Some (sl, bs'))
  else None
;;

let parse_many_gen
      (type a)
      (type b)
      (empty : b)
      (cat_fn : a -> b -> b)
      (pf : a parser_fn)
      bs
  =
  let rec loop acc cur_bs ok =
    match pf cur_bs with
    | None -> acc, cur_bs, ok
    | Some (accepted, updated_bs) -> loop (cat_fn accepted acc) updated_bs true
  in
  match loop empty bs false with
  | acc, updated_bs, true -> Some (acc, updated_bs)
  | _, _, _ -> None
;;

let parse_many pf bs = parse_many_gen [] (fun a acc -> acc @ [ a ]) pf bs

let parse_bytes_many pf bs =
  let start_ptr = bs.ByteSource.ptr in
  let rec loop cur_bs ok =
    match pf cur_bs with
    | None -> cur_bs, ok
    | Some (_, updated_bs) -> loop updated_bs true
  in
  match loop bs false with
  | _, false -> None
  | end_bs, true ->
    let n = end_bs.ByteSource.ptr - start_ptr in
    Some (BS.of_bytes bs.ByteSource.buf start_ptr n, end_bs)
;;

let parse_exact_gen empty cat_fn n pf bs =
  let rec loop i acc cur_bs =
    if i = n
    then Some (acc, cur_bs)
    else (
      let cont_loop_fn (accepted, updated_bs) =
        loop (i + 1) (cat_fn accepted acc) updated_bs
      in
      Option.bind (pf cur_bs) cont_loop_fn)
  in
  loop 0 empty bs
;;

let parse_exact n pf bs = parse_exact_gen [] (fun a acc -> acc @ [ a ]) n pf bs

let parse_bytes_exact n pf bs =
  let start_ptr = bs.ByteSource.ptr in
  let rec loop i cur_bs =
    if i = n
    then Some (BS.of_bytes bs.ByteSource.buf start_ptr n, cur_bs)
    else
      Option.bind (pf cur_bs) (fun (_, updated_bs) -> loop (i + 1) updated_bs)
  in
  loop 0 bs
;;

let optional pf bs =
  match pf bs with
  | None -> Some (None, bs)
  | Some (accepted, updated_bs) -> Some (Some accepted, updated_bs)
;;

let parse_char c bs =
  let ignore_accepted (_, updated_bs) = (), updated_bs in
  parse_one (( = ) c) bs |> Option.map ignore_accepted
;;

let parse_digit = parse_one Char.Ascii.is_digit
let parse_bytes_digit = parse_bytes_one Char.Ascii.is_digit

(* TESTS *)
let%test "parse_one match" =
  let expected_b = 'a' in
  let pred b = b = expected_b in
  let bs = ByteSource.of_string "abc" in
  match parse_one pred bs with
  | Some (accepted, updated_bs) ->
    accepted = expected_b && updated_bs = { bs with ptr = 1 }
  | None -> false
;;

let%test "parse_one no match" =
  let bs = ByteSource.of_string "abc" in
  parse_one (Fun.const false) bs |> Option.is_none
;;

let%test "parse_many success" =
  let bs = ByteSource.of_string "123abc" in
  match parse_many parse_digit bs with
  | Some (cs, updated_bs) -> cs = [ '1'; '2'; '3' ] && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "parse_many end of source" =
  let bs = ByteSource.of_string "123" in
  match parse_many parse_digit bs with
  | Some (cs, updated_bs) -> cs = [ '1'; '2'; '3' ] && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "parse_many failure" =
  let bs = ByteSource.of_string "123" in
  parse_many (parse_one (Fun.const false)) bs |> Option.is_none
;;

let%test "parse_bytes_many success" =
  let bs = ByteSource.of_string "123abc" in
  match parse_bytes_many parse_bytes_digit bs with
  | Some (sl, updated_bs) ->
    BS.to_string sl = "123" && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "parse_bytes_exact success" =
  let bs = ByteSource.of_string "abcde" in
  match parse_bytes_exact 3 (parse_bytes_one (Fun.const true)) bs with
  | Some (sl, updated_bs) ->
    BS.to_string sl = "abc" && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "optional match" =
  let bs = ByteSource.of_string "-1" in
  let test_case =
    let* minus_sign, bs = optional (parse_char '-') bs in
    let* digit, bs = parse_digit bs in
    Some (minus_sign, digit, bs)
  in
  match test_case with
  | Some (Some _, '1', updated_bs) -> updated_bs = { bs with ptr = 2 }
  | _ -> false
;;

let%test "optional no match" =
  let bs = ByteSource.of_string "1" in
  let test_case =
    let* minus_sign, bs = optional (parse_char '-') bs in
    let* digit, bs = parse_digit bs in
    Some (minus_sign, digit, bs)
  in
  match test_case with
  | Some (None, '1', updated_bs) -> updated_bs = { bs with ptr = 1 }
  | _ -> false
;;

let%test "parse_exact success" =
  let bs = ByteSource.of_string "1234" in
  match parse_exact 2 parse_digit bs with
  | Some ([ '1'; '2' ], updated_bs) -> updated_bs = { bs with ptr = 2 }
  | _ -> false
;;

let%test "parse_exact failure" =
  let bs = ByteSource.of_string "1234" in
  parse_exact 5 parse_digit bs |> Option.is_none
;;
