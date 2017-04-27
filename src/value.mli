type t =
  | Int of int
  | Float of float
  | Atom of string

val to_string : t -> string
