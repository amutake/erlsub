module rec Alpha : sig
  type t =
    | Val of Value.t
    | AbsVar of string
    | LetVar of string
    | Tuple of t list
    | App of t * t list
    | Abs of string list * t
    | Let of string * t * t
    | Letrec of (string * t) list * t
    (* | Match of t * (Pattern.t * t) list *)
  val to_string : t -> string
end = struct
  type t =
    | Val of Value.t
    | AbsVar of string
    | LetVar of string
    | Tuple of t list
    | App of t * t list
    | Abs of string list * t
    | Let of string * t * t
    | Letrec of (string * t) list * t
    (* | Match of t * (Pattern.t * t) list *)
  let rec to_string = function
    | Val v -> Value.to_string v
    | AbsVar v -> v
    | LetVar v -> v
    | Tuple es -> "{" ^ (BatList.map to_string es |> BatString.concat ", ") ^ "}"
    | App (e, es) -> to_string e ^ "(" ^ (BatList.map to_string  es |> BatString.concat ",") ^ ")"
    | Abs (args, body) -> "fun(" ^ BatString.concat ", " args ^ ") -> " ^ to_string body
    | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
    | Letrec (binds, e) ->
       let ss = BatList.map (fun (v, e) -> v ^ " = " ^ to_string e) binds in
       "letrec " ^ BatString.concat "; " ss ^ " in " ^ to_string e
    (* | Match (e, bs) -> *)
    (*    let ss = List.map bs ~f:(fun (p, b) -> Pattern.to_string p ^ " -> " ^ to_string b) in *)
    (*    "case " ^ to_string e ^ "of " ^ String.concat ~sep:"; " ss ^ "end" *)
end
and Pattern : sig
  type t =
    | Variant of string * string list * Alpha.t
    (* | Var of string * Alpha.t *)
    (* | Struct of string list * Alpha.t *)
end = struct
  type t =
    | Variant of string * string list * Alpha.t
    (* | Var of string * Alpha.t *)
    (* | Struct of string list * Alpha.t *)
  let rec to_string = function
    | Variant (tag, vars, guard) ->
       "`" ^ tag ^ BatString.concat " " vars ^ " when " ^ Alpha.to_string guard
    (* | Var (v, g) -> v ^ " when " ^ Alpha.to_string g *)
    (* | Struct (vs, g) -> "{" ^ String.concat ~sep:", " vs ^ "} when " ^ Alpha.to_string g *)
end

include Alpha
