type line = int

(* 7.2 Atomic Literals *)
module Literal = struct
  type t =
    | Atom of line * string
    | Char of line * char
    | Float of line * float
    | Integer of line * int
    | String of line * char list
end

(* 7.3 Patterns *)
module Pattern = struct
  type t =
  | Literal of Literal.t
  | Compound of t * t
  | Nil
end
