open Base

type t =
  | Int of Int.t
  | Float of Float.t
  | Atom of String.t

val compare : t -> t -> Int.t

val to_string : t -> String.t
