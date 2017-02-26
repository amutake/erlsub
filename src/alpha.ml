open Base
module Pervasives = Caml.Pervasives (* to avoid ppx_compare warnings *)

module rec Alpha : sig
  type t =
    | Val of Value.t
    | AbsVar of String.t
    | LetVar of String.t
    | Tuple of t List.t
    | App of t * t List.t
    | Abs of String.t List.t * t
    | Let of String.t * t * t
    | Letrec of (String.t * t) List.t * t
    (* | Match of t * (Pattern.t * t) List.t *)
  val compare : t -> t -> Int.t
  val equal : t -> t -> Bool.t
  val to_string : t -> String.t
end = struct
  type t =
    | Val of Value.t
    | AbsVar of String.t
    | LetVar of String.t
    | Tuple of t List.t
    | App of t * t List.t
    | Abs of String.t List.t * t
    | Let of String.t * t * t
    | Letrec of (String.t * t) List.t * t
    (* | Match of t * (Pattern.t * t) List.t *)
  [@@deriving compare]
  let equal = [%compare.equal: t]
  let rec to_string = function
    | Val v -> Value.to_string v
    | AbsVar v -> v
    | LetVar v -> v
    | Tuple es -> "{" ^ (List.map es ~f:to_string |> String.concat ~sep:", ") ^ "}"
    | App (e, es) -> to_string e ^ "(" ^ (List.map es ~f:to_string |> String.concat ~sep:",") ^ ")"
    | Abs (args, body) -> "fun(" ^ String.concat ~sep:", " args ^ ") -> " ^ to_string body
    | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
    | Letrec (binds, e) ->
       let ss = List.map binds ~f:(fun (v, e) -> v ^ " = " ^ to_string e) in
       "letrec " ^ String.concat ~sep:"; " ss ^ " in " ^ to_string e
    (* | Match (e, bs) -> *)
    (*    let ss = List.map bs ~f:(fun (p, b) -> Pattern.to_string p ^ " -> " ^ to_string b) in *)
    (*    "case " ^ to_string e ^ "of " ^ String.concat ~sep:"; " ss ^ "end" *)
end
and Pattern : sig
  type t =
    | Variant of String.t * String.t List.t * Alpha.t
    (* | Var of String.t * Alpha.t *)
    (* | Struct of String.t List.t * Alpha.t *)
  val compare : t -> t -> Int.t
  val to_string : t -> String.t
end = struct
  type t =
    | Variant of String.t * String.t List.t * Alpha.t
    (* | Var of String.t * Alpha.t *)
    (* | Struct of String.t List.t * Alpha.t *)
  [@@deriving compare]
  let rec to_string = function
    | Variant (tag, vars, guard) ->
       "`" ^ tag ^ String.concat ~sep:" " vars ^ " when " ^ Alpha.to_string guard
    (* | Var (v, g) -> v ^ " when " ^ Alpha.to_string g *)
    (* | Struct (vs, g) -> "{" ^ String.concat ~sep:", " vs ^ "} when " ^ Alpha.to_string g *)
end

include Alpha
