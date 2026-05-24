module ByteSource = struct
  type t =
    { buf : bytes
    ; ptr : int
    }

  let of_string s = { buf = Bytes.of_string s; ptr = 0 }

  let peek bs =
    if bs.ptr < Bytes.length bs.buf then Some (Bytes.get bs.buf bs.ptr) else None
  ;;

  let advance n bs =
    let end_ptr = bs.ptr + n in
    if end_ptr <= Bytes.length bs.buf
    then { bs with ptr = bs.ptr + n }
    else raise (Invalid_argument "source already fully consumed")
  ;;

  (* TESTS *)
  let%test "peek success" = of_string "abc" |> peek |> Option.equal ( = ) (Some 'a')
  let%test "peek fail" = of_string "" |> peek |> Option.is_none

  let%test "advance" =
    of_string "abc" |> advance 1 |> peek |> Option.equal ( = ) (Some 'b')
  ;;
end

type 'a parser_fn = ByteSource.t -> ('a * ByteSource.t) option

let ( let* ) = Option.bind

let parse_one pred bs =
  let* b = ByteSource.peek bs in
  if pred b then Some (b, ByteSource.advance 1 bs) else None
;;

let parse_many pf bs =
  let rec loop acc cur_bs =
    match pf cur_bs with
    | None -> acc, cur_bs
    | Some (accepted, updated_bs) -> loop (acc @ [ accepted ]) updated_bs
  in
  match loop [] bs with
  | [], _ -> None
  | res -> Some res
;;

let parse_exact n pf bs =
  let rec loop i acc cur_bs =
    if i = n
    then Some (acc, cur_bs)
    else (
      let cont_loop_fn (accepted, updated_bs) =
        loop (i + 1) (acc @ [ accepted ]) updated_bs
      in
      Option.bind (pf cur_bs) cont_loop_fn)
  in
  loop 0 [] bs
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

let parse_alpha = parse_one Char.Ascii.is_letter
let parse_digit = parse_one Char.Ascii.is_digit

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
  let bs = ByteSource.of_string "abc123" in
  match parse_many parse_alpha bs with
  | Some (cs, updated_bs) -> cs = [ 'a'; 'b'; 'c' ] && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "parse_many end of source" =
  let bs = ByteSource.of_string "abc" in
  match parse_many parse_alpha bs with
  | Some (cs, updated_bs) -> cs = [ 'a'; 'b'; 'c' ] && updated_bs = { bs with ptr = 3 }
  | None -> false
;;

let%test "parse_many failure" =
  let bs = ByteSource.of_string "123" in
  parse_many parse_alpha bs |> Option.is_none
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
  let bs = ByteSource.of_string "abcd" in
  match parse_exact 2 parse_alpha bs with
  | Some ([ 'a'; 'b' ], updated_bs) -> updated_bs = { bs with ptr = 2 }
  | _ -> false
;;

let%test "parse_exact failure" =
  let bs = ByteSource.of_string "abcd" in
  parse_exact 5 parse_alpha bs |> Option.is_none
;;
