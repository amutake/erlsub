open Base
module Pervasives = Caml.Pervasives (* to avoid ppx_compare warnings *)

type t =
  | Int of Int.t
  | Float of Float.t
  | Atom of String.t
[@@deriving compare]

let to_string = function
  | Int n -> Int.to_string n
  | Float n -> Float.to_string n
  | Atom a -> "'" ^ a ^ "'"
