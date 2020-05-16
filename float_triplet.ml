(* 3D point type used as embedding *)

type t_triplet = (float * float * float)

let ( +: ) (x1,y1,z1) (x2,y2,z2) = (x1+.x2,y1+.y2,z1+.z2) ;;

let ( -: ) (x1,y1,z1) (x2,y2,z2) = (x1-.x2,y1-.y2,z1-.z2) ;;

let ( *: ) s (x,y,z) = (s*.x,s*.y,s*.z) ;;

let tsum t_list = List.fold_left ( +: ) (0.,0.,0.) t_list ;;

let tmean t_list =
   assert(t_list!=[]);
  (1. /. float_of_int (List.length t_list)) *: (tsum t_list)
;;

let tnorm (x,y,z) = sqrt (x*.x +. y*.y +. z*.z) ;;

let tleftshift (x,y,z) = (y,z,x) ;;

let trightshift (x,y,z) = (z,x,y) ;;

let tload s_list =
  assert(List.length s_list=3);
  (float_of_string (List.nth s_list 0), 
   float_of_string (List.nth s_list 1),
   float_of_string (List.nth s_list 2))
;;

let tsave (x,y,z) =
  [string_of_float x; string_of_float y; string_of_float z]
;;






