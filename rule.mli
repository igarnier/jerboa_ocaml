(* declaration of the rule module *)

open Rule_sig

module Prule (R_sig : RULE_SIG) : sig
  type t_rule =
    { hooks : int list;
      left_nodes : int list array;
      left_edges : (int * int * int) list;
      node_match : int array;
      right_nodes : int list array;
      right_edges : (int * int * int) list;
      apply_cond : R_sig.t_cond list;
      ebd_expr : (int * string * R_sig.t_expr) list
    }

  exception Can_Not_Associate_Hook

  exception Can_Not_Satisfy_Condition

  exception Can_Not_Match_Pattern

  val check_rule : t_rule -> bool * string

  val apply_rule :
    t_rule ->
    (char * R_sig.t_param) list ->
    R_sig.Gmap.t_dart list ->
    R_sig.Gmap.t_dart array array
end
