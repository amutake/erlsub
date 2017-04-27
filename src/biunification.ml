open Polar

let new_var =
  let var_ref = ref 0 in
  fun () -> let n = !var_ref in incr var_ref; "var_" ^ BatInt.to_string n

module Delta = struct
  type t = (string, Negative.t) BatMap.t
  let merge_all deltas =
    let strategy _ l r = match l, r with
      | Some t1, Some t2 -> Some (Negative.Intersection (t1, t2))
      | Some t, None -> Some t
      | None, Some t -> Some t
      | None, None -> None
    in
    BatList.fold_left (BatMap.merge strategy) BatMap.empty deltas
  let to_string delta =
    BatMap.bindings delta
    |> BatList.map (fun (k, v) -> k ^ " : " ^ Negative.to_string v)
    |> BatString.concat ", "
end

module Scheme = struct
  type t =
    { delta : Delta.t;
      ty : Positive.t
    }
  let from_value v =
    let t = match v with
      | Value.Int _ -> Positive.Int
      | Value.Float _ -> Positive.Float
      | Value.Atom _ -> Positive.Atom
    in
    { delta = BatMap.empty; ty = t }
  let from_var v =
    { delta = BatMap.singleton v (Negative.Var v);
      ty = Positive.Var v
    }
  let to_string scheme =
    "[" ^ Delta.to_string scheme.delta ^ "]" ^ Positive.to_string scheme.ty
end

module Bisubst : sig
  type t =
    | Positive of Positive.t * string
    | Negative of string * Negative.t
  val apply_positive : t -> Positive.t -> Positive.t
  val apply_negative : t -> Negative.t -> Negative.t
  val apply_to_scheme : t list -> Scheme.t -> Scheme.t
  val apply_to_constraints : t list -> (Positive.t * Negative.t) list -> (Positive.t * Negative.t) list
end = struct
  type t =
    | Positive of Positive.t * string
    | Negative of string * Negative.t
  let rec apply_positive b = function
    | Positive.Fun (args, body) ->
       Positive.Fun (BatList.map (apply_negative b) args, apply_positive b body)
    | Positive.Tuple ts ->
       Positive.Tuple (BatList.map (apply_positive b) ts)
    | Positive.Var var ->
       begin
         match b with
         | Positive (r, v) when BatString.equal var v ->
            Positive.Union (Positive.Var v, r)
         | _ -> Positive.Var var (* TODO: recursive *)
       end
    | Positive.Union (t1, t2) ->
       Positive.Union (apply_positive b t1, apply_positive b t2)
    | Positive.Rec (u, t) ->
       Positive.Rec (u, apply_positive b t)
    | p -> p
  and apply_negative b = function
    | Negative.Fun (args, body) ->
       Negative.Fun (BatList.map (apply_positive b) args, apply_negative b body)
    | Negative.Tuple ts ->
       Negative.Tuple (BatList.map (apply_negative b) ts)
    | Negative.Var var ->
       begin
         match b with
         | Negative (v, r) when BatString.equal var v ->
            Negative.Intersection (Negative.Var v, r)
         | _ -> Negative.Var var (* TODO: recursive *)
       end
    | Negative.Intersection (t1, t2) ->
       Negative.Intersection (apply_negative b t1, apply_negative b t2)
    | Negative.Rec (u, t) ->
       Negative.Rec (u, apply_negative b t)
    | n -> n
  let apply_to_scheme bisubsts scheme =
    let delta = BatList.fold_left (fun d b -> BatMap.map (apply_negative b) d) scheme.Scheme.delta bisubsts in
    let typ = BatList.fold_left (fun t b -> apply_positive b t) scheme.Scheme.ty bisubsts in
    { Scheme.delta = delta; Scheme.ty = typ }
  let rec apply_to_constraints bisubsts constraints = match bisubsts with
    | [] -> constraints
    | b :: bs ->
       let constraints = BatList.map (fun (p, n) -> (apply_positive b p, apply_negative b n)) constraints in
       apply_to_constraints bs constraints
end

exception Not_subtype of Positive.t * Negative.t
exception Implementation_error of string

(* destructs (t+ <= t-) form to [t1+ <= t1-; t2+ <= t2-; ...] *)
(* e.g., destruct (A -> A' <= B -> B') ==> [B <= A; A' <= B'] *)
(* destruct : Positive.t -> Negative.t -> [Positive.t * Negative.t] *)
let destruct pos neg = match (pos, neg) with
  | (Positive.Fun (nargs, pret), Negative.Fun (pargs, nret))
       when List.length nargs = List.length pargs ->
     (pret, nret) :: BatList.combine pargs nargs
  | (Positive.Int, Negative.Int) -> []
  | (Positive.Float, Negative.Float) -> []
  | (Positive.Atom, Negative.Atom) -> []
  | (Positive.Tuple pelems, Negative.Tuple nelems)
       when List.length pelems = List.length nelems ->
     BatList.combine pelems nelems
  | (Positive.Rec (a, t), _) ->
     let subst = Bisubst.Positive (pos, a) in
     [(Bisubst.apply_positive subst t, neg)]
  | (_, Negative.Rec (a, t)) ->
     let subst = Bisubst.Negative (a, neg) in
     [(pos, Bisubst.apply_negative subst t)]
  | (Positive.Union (t1, t2), _) -> [(t1, neg); (t2, neg)]
  | (_, Negative.Intersection (t1, t2)) -> [(pos, t1); (pos, t2)]
  | (Positive.Bottom, _) -> []
  | (_, Negative.Top) -> []
  | (_, _) -> raise (Not_subtype (pos, neg))

(* biunify : [Positive.t * Negative.t] -> [Positive.t * Negative.t] -> Bisubst.t list *)
let rec biunify occurs = function
  | [] -> []
  | c :: cs when BatList.mem c occurs ->
     biunify occurs cs
  | (Positive.Var v1, Negative.Var v2) :: cs when String.equal v1 v2 -> biunify occurs cs
  | (Positive.Var v, t) :: cs ->
     let bisubst = Bisubst.Negative (v, t) in
     bisubst :: biunify (Bisubst.apply_to_constraints [bisubst] occurs) (Bisubst.apply_to_constraints [bisubst] cs)
  | (t, Negative.Var v) :: cs ->
     let bisubst = Bisubst.Positive (t, v) in
     bisubst :: biunify (Bisubst.apply_to_constraints [bisubst] occurs) (Bisubst.apply_to_constraints [bisubst] cs)
  | (pos, neg) :: cs ->
     let cs' = destruct pos neg in
     biunify ((pos, neg) :: occurs) (cs' @ cs)

let rec p env = function
  | Alpha.Val v -> Scheme.from_value v
  | Alpha.AbsVar v -> Scheme.from_var v
  | Alpha.LetVar v -> BatMap.find v env
  | Alpha.Tuple es ->
     let (deltas, typs) = collect env es in
     { Scheme.delta = Delta.merge_all deltas;
       Scheme.ty = Positive.Tuple typs
     }
  | Alpha.App (f, args) ->
     let scheme_f = p env f in
     let (deltas, typs) = collect env args in
     let delta = Delta.merge_all (scheme_f.Scheme.delta :: deltas) in
     let var = new_var () in
     let scheme = { Scheme.delta = delta; Scheme.ty = Positive.Var var } in
     print_endline (Scheme.to_string scheme_f);
     print_endline (Negative.to_string (Negative.Fun (typs, Negative.Var var)));
     let bisubsts = biunify [] [(scheme_f.Scheme.ty, Negative.Fun (typs, Negative.Var var))] in
     let res = Bisubst.apply_to_scheme bisubsts scheme in
     print_endline (Scheme.to_string res);
     res
  | Alpha.Abs (args, body) ->
     let { Scheme.delta = delta; Scheme.ty = typ } = p env body in
     let delta' = BatList.fold_left (fun d k -> BatMap.remove k d) delta args in
     let tyargs = BatList.map (fun a -> BatOption.default Negative.Top (BatMap.Exceptionless.find a delta)) args in
     { Scheme.delta = delta'; Scheme.ty = Positive.Fun (tyargs, typ) }
  | Alpha.Let (var, e1, e2) ->
     let scheme1 = p env e1 in
     let scheme2 = p (BatMap.add var scheme1 env) e2 in
     { Scheme.delta = Delta.merge_all [scheme1.Scheme.delta; scheme2.Scheme.delta];
       Scheme.ty = scheme2.Scheme.ty
     }
  | Alpha.Letrec (defs, e) ->
     let vars = BatList.map (fun (var, _) -> var) defs in
     let es = BatList.map (fun (_, e) -> e) defs in
     (* 適当な typing scheme にして新しい環境を作る *)
     let env = BatList.fold_left (fun e var -> BatMap.add var (Scheme.from_var var) env) env vars in
     (* 作った環境で各 let rec 式の body の typing scheme を得る *)
     let (deltas, typs) = collect env es in
     (* 制約を洗い出す *)
     let constraints =
       let actual var typ = BatList.map (fun d -> (typ, BatMap.find var d)) deltas in
       let nested_actual = BatList.map2 actual vars typs in
       List.concat nested_actual
     in
     let bisubsts = biunify [] constraints in
     let scheme = p env e in
     Bisubst.apply_to_scheme bisubsts scheme
  (* | Alpha.Match (e, bs) -> *)

and collect env es =
  let f e =
    let { Scheme.delta = d; Scheme.ty = t } = p env e in
    (d, t)
  in
  BatList.split (BatList.map f es)
