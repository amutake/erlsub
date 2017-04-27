type t =
  | Val of Value.t
  | Var of string
  | Tuple of t list
  | App of t * t list
  | Abs of string list * t
  | Let of string * t * t
  | Letrec of string * t * t  (*  mutual recursion is not allowed *)
  | Match of t * (string * string list * t) list

let rec to_string = function
  | Val v -> Value.to_string v
  | Var v -> v
  | Tuple es -> "{" ^ (BatList.map to_string es |> BatString.concat ", ") ^ "}"
  | App (e, es) -> to_string e ^ "(" ^ (BatList.map to_string es |> BatString.concat ",") ^ ")"
  | Abs (args, body) -> "fun(" ^ BatString.concat ", " args ^ ") -> " ^ to_string body
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
  | Letrec (v, e1, e2) ->
     "let rec " ^ v ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
  | Match (e, bs) ->
     let params_to_string = function
       | [] -> ""
       | args -> "(" ^ BatString.concat ", " args ^ ")"
     in
     let ss = List.map (fun (tag, params, body) ->
                        "| `" ^ tag ^ params_to_string params ^ " -> " ^ to_string body
                       ) bs in
     "match " ^ to_string e ^ " of " ^ BatString.concat "\n" ss ^ "end"
