open Float_triplet
open Gmap
open Rule

(* we use 3-G-maps with 3D points
   t_info are used for represents darts with 3D points *)
(* Embedding signature *)

module Ebd_gmap_sig_mod = struct
  type t_ebd = Float_triplet.t_triplet

  type t_info = Float_triplet.t_triplet

  let ebd_names = [| "point"; "color" |]

  let ebd_orbits = [| [1; 2; 3]; [0; 1; 3] |]

  let dim = 3
end

module Gmap_mod = Pgmap (Ebd_gmap_sig_mod)

module Rule_sig_mod = struct
  module Gmap = Gmap_mod

  type t_param = PConst of t_triplet | PScal of float

  type t_expr =
    | Const of t_triplet
    | ConstVar of char
    | Plus of t_expr * t_expr
    | Minus of t_expr * t_expr
    | Scal of float * t_expr
    | ScalVar of char * t_expr
    | LeftShift of t_expr
    | RightShift of t_expr
    | Sum of t_ens_expr
    | Mean of t_ens_expr
    | PointEbd of t_node_expr
    | ColorEbd of t_node_expr

  and t_ens_expr =
    | EnsConst of t_triplet list
    | EnsExpr of t_expr list
    | OrbitPoint of int list * t_node_expr
    | OrbitColor of int list * t_node_expr

  and t_node_expr =
    | Node of int
    | Alpha0 of t_node_expr
    | Alpha1 of t_node_expr
    | Alpha2 of t_node_expr
    | Alpha3 of t_node_expr

  exception Bad_Parameter

  let rec eval_expr f p = function
    | Const e -> e
    | ConstVar c -> (
        match p c with PConst t -> t | _ -> raise Bad_Parameter )
    | Plus (a, b) -> eval_expr f p a +: eval_expr f p b
    | Minus (a, b) -> eval_expr f p a -: eval_expr f p b
    | Scal (x, a) -> x *: eval_expr f p a
    | ScalVar (c, a) -> (
        match p c with
        | PScal x -> x *: eval_expr f p a
        | _ -> raise Bad_Parameter )
    | LeftShift a -> tleftshift (eval_expr f p a)
    | RightShift a -> trightshift (eval_expr f p a)
    | Sum e -> tsum (eval_ens f p e)
    | Mean e -> tmean (eval_ens f p e)
    | PointEbd d -> Gmap.ebd "point" (eval_node f d)
    | ColorEbd d -> Gmap.ebd "color" (eval_node f d)

  and eval_ens f p = function
    | EnsConst e -> e
    | EnsExpr e -> List.map (eval_expr f p) e
    | OrbitPoint (o, d) -> Gmap.ebd_collect "point" o (eval_node f d)
    | OrbitColor (o, d) -> Gmap.ebd_collect "color" o (eval_node f d)

  and eval_node f = function
    | Node d -> f d
    | Alpha0 e -> Gmap.alpha 0 (eval_node f e)
    | Alpha1 e -> Gmap.alpha 1 (eval_node f e)
    | Alpha2 e -> Gmap.alpha 2 (eval_node f e)
    | Alpha3 e -> Gmap.alpha 3 (eval_node f e)

  type t_cond =
    | SameOrbit of int list * int * int
    | SubOrbit of int list * int list * int * (int -> bool)
    | EbdCond of string * (t_triplet -> bool) * int
    | TriangleSurface of int * (float -> bool)

  let eval_cond f _p = function
    | SameOrbit (o, d1, d2) ->
        Gmap.orbit_fold_left (fun b d -> b || d = f d2) false o (f d1)
    | SubOrbit (o1, o2, d0, c) ->
        let m = Gmap.get_mark () and count = ref 0 in
        Gmap.orbit_iter
          (fun d ->
            if not (Gmap.is_marked m d) then (
              count := !count + 1 ;
              Gmap.orbit_iter (Gmap.mark m) o2 d ))
          o1
          (f d0) ;
        Gmap.free_mark m ;
        c !count
    | EbdCond (name, c, d) -> c (Gmap.ebd name (f d))
    | TriangleSurface (d, c) -> (
        match Gmap.ebd_collect "point" [0; 1] (f d) with
        | [x; y; z] ->
            let u = tnorm (x -: y)
            and v = tnorm (y -: z)
            and w = tnorm (z -: x) in
            let s = (u +. v +. w) /. 2. in
            c (sqrt (s *. (s -. u) *. (s -. v) *. (s -. w)))
        | _ -> false )
end

module Rule_mod = Prule (Rule_sig_mod)
open Rule_mod

let mod_rule_list : (string * t_rule) list ref = ref []
