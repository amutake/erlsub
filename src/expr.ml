open Base
module Pervasives = Caml.Pervasives (* to avoid ppx_compare warnings *)

type t =
  | Val of Value.t
  | Var of String.t
  | Tuple of t List.t
  | App of t * t List.t
  | Abs of String.t List.t * t
  | Let of String.t * t * t
  | Letrec of String.t * t * t  (*  mutual recursion is not allowed *)
  | Match of t * (String.t * String.t List.t * t) List.t
[@@deriving compare]

let equal = [%compare.equal: t]

let rec to_string = function
  | Val v -> Value.to_string v
  | Var v -> v
  | Tuple es -> "{" ^ (List.map es ~f:to_string |> String.concat ~sep:", ") ^ "}"
  | App (e, es) -> to_string e ^ "(" ^ (List.map es ~f:to_string |> String.concat ~sep:",") ^ ")"
  | Abs (args, body) -> "fun(" ^ String.concat ~sep:", " args ^ ") -> " ^ to_string body
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
  | Letrec (v, e1, e2) ->
     "let rec " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
  | Match (e, bs) ->
     let params_to_string = function
       | [] -> ""
       | args -> "(" ^ String.concat args ~sep:", " ^ ")"
     in
     let ss = List.map bs ~f:(fun (tag, params, body) ->
                              "| `" ^ tag ^ params_to_string params ^ " -> " ^ to_string body
                             ) in
     "match " ^ to_string e ^ " of " ^ String.concat ss ~sep:"\n" ^ "end"
