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
    if end_ptr <= Bytes.length bs.buf then Some { bs with ptr = bs.ptr + n } else None
  ;;

  (* TODO: TESTS *)
end

let ( let* ) = Option.bind

let parse_one pred bs =
  let* b = ByteSource.peek bs in
  if pred b then Some (b, Option.get (ByteSource.advance 1 bs)) else None
;;

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
