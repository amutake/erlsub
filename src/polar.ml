open Base
module Pervasives = Caml.Pervasives (* to avoid ppx_compare warnings *)

module rec Positive : sig
  type t =
    | Int
    | Float
    | Atom
    | Fun of Negative.t list * t
    | Tuple of t List.t
    | Var of String.t
    | Union of t * t
    | Bottom
    | Rec of String.t * t
  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t
  val to_string : t -> String.t
end = struct
  type t =
    | Int
    | Float
    | Atom
    | Fun of Negative.t list * t
    | Tuple of t List.t
    | Var of String.t
    | Union of t * t
    | Bottom
    | Rec of String.t * t
  [@@deriving compare]
  let equal = [%compare.equal: t]
  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Atom -> "atom"
    | Fun (args, body) ->
       "((" ^ String.concat ~sep:", " (List.map args ~f:Negative.to_string) ^ ") -> " ^ to_string body ^ ")"
    | Tuple ts ->
       "{" ^ String.concat ~sep:", " (List.map ts ~f:to_string) ^ "}"
    | Var var -> var
    | Union (t1, t2) -> "(" ^ to_string t1 ^ " \\/ " ^ to_string t2 ^ ")"
    | Bottom -> "\bot"
    | Rec (r, t) -> "rec " ^ r ^ ". " ^ to_string t
end
and Negative : sig
  type t =
    | Int
    | Float
    | Atom
    | Fun of Positive.t list * t
    | Tuple of t List.t
    | Var of String.t
    | Intersection of t * t
    | Top
    | Rec of String.t * t
  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t
  val to_string : t -> String.t
end = struct
  type t =
    | Int
    | Float
    | Atom
    | Fun of Positive.t list * t
    | Tuple of t List.t
    | Var of String.t
    | Intersection of t * t
    | Top
    | Rec of String.t * t
  [@@deriving compare]
  let equal = [%compare.equal: t]
  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Atom -> "atom"
    | Fun (args, body) ->
       "((" ^ String.concat ~sep:", " (List.map args ~f:Positive.to_string) ^ ") -> " ^ to_string body ^ ")"
    | Tuple ts ->
       "{" ^ String.concat ~sep:", " (List.map ts ~f:to_string) ^ "}"
    | Var var -> var
    | Intersection (t1, t2) -> "(" ^ to_string t1 ^ " /\\ " ^ to_string t2 ^ ")"
    | Top -> "\top"
    | Rec (r, t) -> "rec " ^ r ^ " = " ^ to_string t
end
