type t =
  { bs : bytes
  ; start : int
  ; len : int
  }

let empty = { bs = Bytes.empty; start = 0; len = 0 }
let of_bytes bs start len = { bs; start; len }
let of_string s start len = { bs = Bytes.of_string s; start; len }
let length b = b.len
let get b i = Bytes.get b.bs (b.start + i)

let sub b pos len =
  if pos < 0 || len < 0 || pos + len > b.len
  then raise (Invalid_argument "Byteslice.sub")
  else { b with start = b.start + pos; len }
;;

let to_bytes b = Bytes.sub b.bs b.start b.len
let to_string b = Bytes.sub_string b.bs b.start b.len

let equal b0 b1 =
  b0.len = b1.len
  &&
  let rec loop i =
    if i = b0.len then true
    else if Bytes.get b0.bs (b0.start + i) = Bytes.get b1.bs (b1.start + i)
    then loop (i + 1)
    else false
  in
  loop 0
;;

let compare b0 b1 =
  let max_i = min b0.len b1.len in
  let rec loop i cmp =
    if i = max_i
    then cmp
    else (
      let cmp' = Stdlib.compare (get b0 i) (get b1 i) in
      if cmp' = 0 then loop (i + 1) cmp' else cmp')
  in
  loop 0 0
;;

(* TESTS *)
let%test "get" =
  let b = of_bytes (Bytes.of_string "1234") 2 2 in
  get b 1 = '4'
;;

let%test "length" =
  let b = of_string "hello" 1 3 in
  length b = 3
;;

let%test "sub" =
  let b = of_string "abcdef" 0 6 in
  let s = sub b 2 3 in
  to_string s = "cde"
;;

let%test "to_bytes" =
  let b = of_string "abcdef" 2 3 in
  to_bytes b = Bytes.of_string "cde"
;;

let%test "to_string" =
  let b = of_string "abcdef" 2 3 in
  to_string b = "cde"
;;

let%test "equal true" =
  let b0 = of_string "xabcy" 1 3 in
  let b1 = of_string "abc" 0 3 in
  equal b0 b1
;;

let%test "equal false length" =
  let b0 = of_string "abc" 0 3 in
  let b1 = of_string "ab" 0 2 in
  not (equal b0 b1)
;;

let%test "compare" =
  let b0 = of_bytes (Bytes.of_string "1abc") 1 3 in
  let b1 = of_bytes (Bytes.of_string "2bcd") 1 2 in
  compare b0 b1 = -1
;;
