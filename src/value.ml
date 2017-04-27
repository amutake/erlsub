type t =
  | Int of int
  | Float of float
  | Atom of string

let to_string = function
  | Int n -> BatInt.to_string n
  | Float n -> BatFloat.to_string n
  | Atom a -> "'" ^ a ^ "'"
