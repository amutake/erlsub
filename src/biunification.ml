open Base
module Pervasives = Caml.Pervasives (* to avoid ppx_compare warnings *)
open Polar

let new_var =
  let var_ref = ref 0 in
  fun () -> let n = !var_ref in Caml.incr var_ref; "var_" ^ Int.to_string n

module Delta = struct
  module Key = String
  type t = Negative.t Map.M(Key).t
  let empty = Map.empty (module Key)
  let singleton = Map.singleton (module Key)
  let merge deltas =
    let strategy ~key:_ = function
      | `Left t -> Some t
      | `Right t -> Some t
      | `Both (t1, t2) -> Some (Negative.Intersection (t1, t2))
    in
    List.fold_left deltas ~init:empty ~f:(Map.merge ~f:strategy)
  let to_string delta =
    Map.to_alist delta
    |> List.map ~f:(fun (k, v) -> k ^ " : " ^ Negative.to_string v)
    |> String.concat ~sep:", "
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
    { delta = Delta.empty; ty = t }
  let from_var v =
    { delta = Delta.singleton v (Negative.Var v);
      ty = Positive.Var v
    }
  let to_string scheme =
    "[" ^ Delta.to_string scheme.delta ^ "]" ^ Positive.to_string scheme.ty
end

module Bisubst : sig
  type t =
    | Positive of Positive.t * String.t
    | Negative of String.t * Negative.t
  val apply_positive : t -> Positive.t -> Positive.t
  val apply_negative : t -> Negative.t -> Negative.t
  val apply_to_scheme : t List.t -> Scheme.t -> Scheme.t
  val apply_to_constraints : t List.t -> (Positive.t * Negative.t) List.t -> (Positive.t * Negative.t) List.t
end = struct
  type t =
    | Positive of Positive.t * String.t
    | Negative of String.t * Negative.t
  let rec apply_positive b = function
    | Positive.Fun (args, body) ->
       Positive.Fun (List.map args ~f:(apply_negative b), apply_positive b body)
    | Positive.Tuple ts ->
       Positive.Tuple (List.map ts ~f:(apply_positive b))
    | Positive.Var var ->
       begin
         match b with
         | Positive (r, v) when String.equal var v ->
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
       Negative.Fun (List.map args ~f:(apply_positive b), apply_negative b body)
    | Negative.Tuple ts ->
       Negative.Tuple (List.map ts ~f:(apply_negative b))
    | Negative.Var var ->
       begin
         match b with
         | Negative (v, r) when String.equal var v ->
            Negative.Intersection (Negative.Var v, r)
         | _ -> Negative.Var var (* TODO: recursive *)
       end
    | Negative.Intersection (t1, t2) ->
       Negative.Intersection (apply_negative b t1, apply_negative b t2)
    | Negative.Rec (u, t) ->
       Negative.Rec (u, apply_negative b t)
    | n -> n
  let apply_to_scheme bisubsts scheme =
    let delta = List.fold_left bisubsts ~init:scheme.Scheme.delta ~f:(fun d b -> Map.map d ~f:(apply_negative b)) in
    let typ = List.fold_left bisubsts ~init:scheme.Scheme.ty ~f:(fun t b -> apply_positive b t) in
    { Scheme.delta = delta; Scheme.ty = typ }
  let rec apply_to_constraints bisubsts constraints = match bisubsts with
    | [] -> constraints
    | b :: bs ->
       let constraints = List.map constraints ~f:(fun (p, n) -> (apply_positive b p, apply_negative b n)) in
       apply_to_constraints bs constraints
end

exception Not_subtype of Positive.t * Negative.t
exception Implementation_error of String.t

(* destructs (t+ <= t-) form to [t1+ <= t1-; t2+ <= t2-; ...] *)
(* e.g., destruct (A -> A' <= B -> B') ==> [B <= A; A' <= B'] *)
(* destruct : Positive.t -> Negative.t -> [Positive.t * Negative.t] *)
let destruct pos neg = match (pos, neg) with
  | (Positive.Fun (nargs, pret), Negative.Fun (pargs, nret))
       when List.length nargs = List.length pargs ->
     (pret, nret) :: List.zip_exn pargs nargs
  | (Positive.Int, Negative.Int) -> []
  | (Positive.Float, Negative.Float) -> []
  | (Positive.Atom, Negative.Atom) -> []
  | (Positive.Tuple pelems, Negative.Tuple nelems)
       when List.length pelems = List.length nelems ->
     List.zip_exn pelems nelems
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
  | c :: cs when List.mem occurs c ~equal:(fun (p1, n1) (p2, n2) -> Positive.equal p1 p2 && Negative.equal n1 n2) ->
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
  | Alpha.LetVar v -> Map.find_exn env v
  | Alpha.Tuple es ->
     let (deltas, typs) = collect env es in
     { Scheme.delta = Delta.merge deltas;
       Scheme.ty = Positive.Tuple typs
     }
  | Alpha.App (f, args) ->
     let scheme_f = p env f in
     let (deltas, typs) = collect env args in
     let delta = Delta.merge (scheme_f.Scheme.delta :: deltas) in
     let var = new_var () in
     let scheme = { Scheme.delta = delta; Scheme.ty = Positive.Var var } in
     let bisubsts = biunify [] [(scheme_f.Scheme.ty, Negative.Fun (typs, Negative.Var var))] in
     Bisubst.apply_to_scheme bisubsts scheme
  | Alpha.Abs (args, body) ->
     let { Scheme.delta = delta; Scheme.ty = typ } = p env body in
     let delta' = List.fold_left args ~init:delta ~f:Map.remove in
     let tyargs = List.map args ~f:(fun a -> Option.value (Map.find delta a) ~default:Negative.Top) in
     { Scheme.delta = delta'; Scheme.ty = Positive.Fun (tyargs, typ) }
  | Alpha.Let (var, e1, e2) ->
     let scheme1 = p env e1 in
     let scheme2 = p (Map.add env ~key:var ~data:scheme1) e2 in
     { Scheme.delta = Delta.merge [scheme1.Scheme.delta; scheme2.Scheme.delta];
       Scheme.ty = scheme2.Scheme.ty
     }
  | Alpha.Letrec (defs, e) ->
     let vars = List.map defs ~f:(fun (var, _) -> var) in
     let es = List.map defs ~f:(fun (_, e) -> e) in
     (* 適当な typing scheme にして新しい環境を作る *)
     let env = List.fold_left vars ~init:env ~f:(fun e var -> Map.add e ~key:var ~data:(Scheme.from_var var)) in
     (* 作った環境で各 let rec 式の body の typing scheme を得る *)
     let (deltas, typs) = collect env es in
     (* 制約を洗い出す *)
     let constraints =
       let actual var typ = List.map deltas ~f:(fun d -> (typ, Map.find_exn d var)) in
       let nested_actual = List.map2_exn vars typs ~f:actual in
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
  List.unzip (List.map es ~f:f)
