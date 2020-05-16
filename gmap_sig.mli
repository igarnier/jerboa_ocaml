(* Embedded G-map signature *)

module type EBD_GMAP_SIG =
sig
  type t_ebd
  type t_info
  val dim : int
  val ebd_names : string array
  val ebd_orbits : (int list) array
end
;;

(* Embedded G-map module *)

module type EBD_GMAP =
sig
  type t_ebd
  type t_info
  type t_dart
  type t_mark
  type t_state
  exception Undefined_Ebd
  exception None_Info ;;
  exception Unconsistant_Gmap;;
  val empty : unit -> unit
  val dim :int
  val nb_darts : unit -> int
  val nb_ebd_values : string -> int
  val alpha : int -> t_dart -> t_dart
  val label : t_dart -> int
  val from_label : int -> t_dart
  val ebd_orbit : string -> int list
  val ebd : string -> t_dart -> t_ebd
  val info : t_dart -> t_info
  val has_info : t_dart -> bool
  val has_ebd : string -> t_dart -> bool
  val set_ebd : string -> t_ebd -> t_dart -> unit
  val set_info : t_info -> t_dart -> unit
  val get_mark : unit -> t_mark
  val marked : t_mark -> t_dart list
  val free_mark : t_mark -> unit
  val mark : t_mark -> t_dart -> unit
  val unmark : t_mark -> t_dart -> unit
  val is_marked : t_mark -> t_dart -> bool
  val add_dart : unit -> t_dart
  val add_ebd_dart : (string * t_ebd) list -> t_dart
  val delete_dart : t_dart -> unit
  val link : int -> (string * bool) list -> t_dart -> t_dart -> unit
  val unlink : int -> t_dart -> unit
  val gmap_iter : (t_dart -> unit) -> unit
  val gmap_map : (t_dart -> 'a) -> 'a list
  val gmap_fold_left : ('a -> t_dart -> 'a) -> 'a -> 'a
  val orbit_iter : (t_dart -> unit) -> int list -> t_dart -> unit
  val orbit_map : (t_dart -> 'a) -> int list -> t_dart -> 'a list
  val orbit_fold_left : ('a -> t_dart -> 'a) -> 'a -> int list -> t_dart -> 'a
  val ebd_collect : string -> int list -> t_dart -> t_ebd list
  val check_gmap : unit -> unit
  val get_state : unit -> t_state
  val set_state : t_state -> unit
end
;;


(* embedded gmap generator signature *)

module type PARA_GMAP = 
  functor (G_sig : EBD_GMAP_SIG) ->
      EBD_GMAP   with type t_ebd = G_sig.t_ebd and type t_info = G_sig.t_info
;;

