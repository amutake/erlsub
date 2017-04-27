module rec Positive : sig
  type t =
    | Int
    | Float
    | Atom
    | Fun of Negative.t list * t
    | Tuple of t list
    | Var of String.t
    | Union of t * t
    | Bottom
    | Rec of String.t * t
  val to_string : t -> String.t
end = struct
  type t =
    | Int
    | Float
    | Atom
    | Fun of Negative.t list * t
    | Tuple of t list
    | Var of String.t
    | Union of t * t
    | Bottom
    | Rec of String.t * t
  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Atom -> "atom"
    | Fun (args, body) ->
       "((" ^ BatString.concat ", " (BatList.map Negative.to_string args) ^ ") -> " ^ to_string body ^ ")"
    | Tuple ts ->
       "{" ^ BatString.concat ", " (BatList.map to_string ts) ^ "}"
    | Var var -> var
    | Union (t1, t2) -> "(" ^ to_string t1 ^ " \\/ " ^ to_string t2 ^ ")"
    | Bottom -> "\bot"
    | Rec (r, t) -> "rec " ^ r ^ " = " ^ to_string t
end
and Negative : sig
  type t =
    | Int
    | Float
    | Atom
    | Fun of Positive.t list * t
    | Tuple of t list
    | Var of string
    | Intersection of t * t
    | Top
    | Rec of string * t
  val to_string : t -> string
end = struct
  type t =
    | Int
    | Float
    | Atom
    | Fun of Positive.t list * t
    | Tuple of t list
    | Var of string
    | Intersection of t * t
    | Top
    | Rec of string * t
  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Atom -> "atom"
    | Fun (args, body) ->
       "((" ^ BatString.concat ", " (BatList.map Positive.to_string args) ^ ") -> " ^ to_string body ^ ")"
    | Tuple ts ->
       "{" ^ BatString.concat ", " (BatList.map to_string ts) ^ "}"
    | Var var -> var
    | Intersection (t1, t2) -> "(" ^ to_string t1 ^ " /\\ " ^ to_string t2 ^ ")"
    | Top -> "\top"
    | Rec (r, t) -> "rec " ^ r ^ " = " ^ to_string t
end
