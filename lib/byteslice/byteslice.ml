type t =
  { bs : bytes
  ; start : int
  ; len : int
  }

let of_bytes bs start len = { bs; start; len }
let of_string s start len = { bs = Bytes.of_string s; start; len }
let get b i = Bytes.get b.bs (b.start + i)

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

let%test "compare" =
  let b0 = of_bytes (Bytes.of_string "1abc") 1 3 in
  let b1 = of_bytes (Bytes.of_string "2bcd") 1 2 in
  compare b0 b1 = -1
;;
