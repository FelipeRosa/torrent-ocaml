module ByteSource : sig
  type t

  val of_string : string -> t
  val peek : t -> char option
  val advance : int -> t -> t
end

type 'a parser_fn = ByteSource.t -> ('a * ByteSource.t) option

val parse_one : (char -> bool) -> char parser_fn
val parse_many : 'a parser_fn -> 'a list parser_fn
val parse_exact : int -> 'a parser_fn -> 'a list parser_fn
val optional : 'a parser_fn -> 'a option parser_fn
val parse_char : char -> unit parser_fn
val parse_alpha : char parser_fn
val parse_digit : char parser_fn
