(* Rule module parameter types *)

open Gmap_sig ;;

module type RULE_SIG =
sig
  module Gmap : EBD_GMAP
    (* type of expressions that compute embedding
       names of rule nodes should be int and name of parameter character *)
  type t_expr
  type t_param
  type t_cond
  val eval_expr : (int -> Gmap.t_dart) -> (char -> t_param)
    -> t_expr -> Gmap.t_ebd
  val eval_cond : (int -> Gmap.t_dart) -> (char -> t_param)
    -> t_cond -> bool
    (*the function parameter accesses a dart from a rule node name*)
end
;;
