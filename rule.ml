(* Rule implementation *)

open Rule_sig

module Prule (R_sig : RULE_SIG) = struct
  (*------ Rule data types and functions ------*)

  open R_sig
  open R_sig.Gmap

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

  (* check the adjacent edges and cycles criteria *)
  let check_rule r =
    let log = ref [] in
    let count_incid nodes edges =
      let count =
        Array.init (Array.length nodes) (fun _ -> Array.make (dim + 1) 0)
      in
      Array.iteri
        (fun i l ->
          (List.iter (fun a ->
               if a >= 0 then count.(i).(a) <- count.(i).(a) + 1))
            l)
        nodes ;
      List.iter
        (fun (i, j, a) ->
          count.(i).(a) <- count.(i).(a) + 1 ;
          if i != j then count.(j).(a) <- count.(j).(a) + 1)
        edges ;
      count
    and has_cycle nodes edges ai aj d =
      try
        if List.mem ai nodes.(d) then
          if List.mem aj nodes.(d) then true
          else
            let (i, j, _) =
              List.find (fun (i, j, a) -> a = aj && (i = d || j = d)) edges
            in
            if i = j then true
            else
              List.fold_left2
                (fun a b c -> if a then a else b = ai && c = ai)
                false
                nodes.(d)
                nodes.((if d = i then j else i))
        else if List.mem aj nodes.(d) then
          let (i, j, _) =
            List.find (fun (i, j, a) -> a = ai && (i = d || j = d)) edges
          in
          if i = j then true
          else
            List.fold_left2
              (fun a b c -> if a then a else b = aj && c = aj)
              false
              nodes.(d)
              nodes.((if d = i then j else i))
        else
          let (i1, j1, _) =
            List.find (fun (i, j, a) -> a = ai && (i = d || j = d)) edges
          and (i2, j2, _) =
            List.find (fun (i, j, a) -> a = aj && (i = d || j = d)) edges
          in
          if i1 = j1 then
            if i2 = j2 then true
            else
              let k = if i2 = d then j2 else i2 in
              let (i3, j3, _) =
                List.find (fun (i, j, a) -> a = ai && (i = k || j = k)) edges
              in
              i3 = j3
          else if i2 = j2 then
            let k = if i1 = d then j1 else i1 in
            let (i3, j3, _) =
              List.find (fun (i, j, a) -> a = aj && (i = k || j = k)) edges
            in
            i3 = j3
          else
            let k1 = if i1 = d then j1 else i1
            and k2 = if i2 = d then j2 else i2 in
            let (i3, j3, _) =
              List.find (fun (i, j, a) -> a = aj && (i = k1 || j = k1)) edges
            and (i4, j4, _) =
              List.find (fun (i, j, a) -> a = ai && (i = k2 || j = k2)) edges
            in
            (if i3 = k1 then j3 else i3) = if i4 = k2 then j4 else i4
      with Not_found -> false
    in
    (* incident edges condition *)
    let nb_right = Array.length r.right_nodes in
    let left_incid = count_incid r.left_nodes r.left_edges
    and right_incid = count_incid r.right_nodes r.right_edges
    and from = Array.make nb_right (-1) in
    Array.iteri (fun i j -> if j >= 0 then from.(j) <- i) r.node_match ;
    for i = 0 to nb_right - 1 do
      if from.(i) = -1 then
        for a = 0 to dim do
          if right_incid.(i).(a) < 1 then
            log :=
              ( "Right node " ^ string_of_int i ^ " is added without alpha"
              ^ string_of_int a ^ " edge" )
              :: !log
          else if right_incid.(i).(a) > 1 then
            log :=
              ( "Right node " ^ string_of_int i
              ^ " is added with multiple alpha" ^ string_of_int a ^ " edges" )
              :: !log
        done
      else
        for a = 0 to dim do
          if left_incid.(from.(i)).(a) > 1 then
            log :=
              ( "Left node "
              ^ string_of_int from.(i)
              ^ " has multiple alpha" ^ string_of_int a ^ " edges" )
              :: !log
          else if right_incid.(i).(a) > 1 then
            log :=
              ( "Left node " ^ string_of_int i ^ " has multiple alpha"
              ^ string_of_int a ^ " edges" )
              :: !log
          else if left_incid.(from.(i)).(a) != right_incid.(i).(a) then
            log :=
              ( "Alpha" ^ string_of_int a
              ^ " edge is not preserved from left node "
              ^ string_of_int from.(i)
              ^ " to right node " ^ string_of_int i )
              :: !log
        done
    done ;
    (* non-oriented graph condtion*)
    (* => true for edges of labeled nodes *)
    (* => true for explict given edges because of the structure *)
    (* cycle condition *)
    for a1 = 0 to dim - 2 do
      for a2 = a1 + 2 to dim do
        for i = 0 to Array.length r.left_nodes - 1 do
          if has_cycle r.left_nodes r.left_edges a1 a2 i then (
            if
              r.node_match.(i) != -1
              && not
                   (has_cycle
                      r.right_nodes
                      r.right_edges
                      a1
                      a2
                      r.node_match.(i))
            then
              log :=
                ( "Alpha" ^ string_of_int a1 ^ "alpha" ^ string_of_int a2
                ^ " cycle is not preserved from left node " ^ string_of_int i
                ^ " to right node "
                ^ string_of_int r.node_match.(i) )
                :: !log )
          else if r.node_match.(i) = -1 then
            log :=
              ( "Left node " ^ string_of_int i
              ^ " is deleted despite it has no alpha" ^ string_of_int a1
              ^ "alpha" ^ string_of_int a2 ^ " cycle" )
              :: !log
          else
            List.iter
              (fun ai ->
                List.iter
                  (fun (j, k, a) ->
                    if a = ai && (j = i || k = i) then
                      if
                        r.node_match.(j) = -1
                        || r.node_match.(k) = -1
                        || not
                             ( List.mem
                                 (r.node_match.(j), r.node_match.(k), a)
                                 r.right_edges
                             || List.mem
                                  (r.node_match.(k), r.node_match.(j), a)
                                  r.right_edges )
                      then
                        log :=
                          ( "Left node " ^ string_of_int i ^ " has no alpha"
                          ^ string_of_int a1 ^ "alpha" ^ string_of_int a2
                          ^ " cycle but edges are modified on right node"
                          ^ string_of_int r.node_match.(i) )
                          :: !log)
                  r.left_edges)
              [a1; a2]
        done ;
        for i = 0 to nb_right - 1 do
          if from.(i) = -1 then
            if not (has_cycle r.right_nodes r.right_edges a1 a2 i) then
              log :=
                ( "Right node " ^ string_of_int i ^ " is added without alpha"
                ^ string_of_int a1 ^ "alpha" ^ string_of_int a2 ^ " cycle" )
                :: !log
        done
      done
    done ;
    (!log = [], List.fold_right (fun s l -> s ^ "\n" ^ l) !log "")

  (* orbit matching for rule apply *)
  let identify_nodes r h =
    if List.length r.hooks != List.length h then raise Can_Not_Associate_Hook ;
    let nb_nodes = Array.length r.left_nodes in
    let nodes = Array.make nb_nodes [||] in
    (* get the hooks *)
    List.iter2
      (fun i d ->
        nodes.(i) <- Array.of_list (orbit_map (fun x -> x) r.left_nodes.(i) d))
      r.hooks
      h ;
    (* get the other nodes *)
    let completed = ref (List.length r.hooks) in
    while !completed != nb_nodes do
      Array.iteri
        (fun i l ->
          if l = [||] then
            List.iter
              (fun (x, y, a) ->
                if x = i && nodes.(y) != [||] then (
                  completed := !completed + 1 ;
                  nodes.(i) <- Array.map (alpha a) nodes.(y) )
                else if y = i && nodes.(x) != [||] then (
                  completed := !completed + 1 ;
                  nodes.(i) <- Array.map (alpha a) nodes.(x) ))
              r.left_edges)
        nodes
    done ;
    (* check node unicity *)
    let m = get_mark () in
    Array.iter
      (Array.iter (fun d ->
           if is_marked m d then (
             free_mark m ;
             raise Can_Not_Match_Pattern )
           else mark m d))
      nodes ;
    free_mark m ;
    (* check the orbits *)
    let first_given = List.hd r.hooks in
    let table = Hashtbl.create (Array.length nodes.(first_given)) in
    Array.iteri (fun i d -> Hashtbl.add table d i) nodes.(first_given) ;
    Array.iteri
      (fun i l ->
        if i != first_given then
          Array.iteri
            (fun j d ->
              List.iter2
                (fun ori ren ->
                  if
                    ren >= 0
                    && alpha ren d >= d
                    && alpha ren d
                       != nodes.(i).(Hashtbl.find
                                       table
                                       (alpha ori nodes.(first_given).(j)))
                  then raise Can_Not_Match_Pattern)
                r.left_nodes.(first_given)
                r.left_nodes.(i))
            l)
      nodes ;
    (* check the edges *)
    List.iter
      (fun (i, j, a) ->
        Array.iteri
          (fun k d ->
            if alpha a d != nodes.(j).(k) then raise Can_Not_Match_Pattern)
          nodes.(i))
      r.left_edges ;
    nodes

  let check_conditions r p nodes =
    List.iter
      (fun c ->
        if not (eval_cond (fun i -> nodes.(i).(0)) (fun a -> List.assoc a p) c)
        then raise Can_Not_Satisfy_Condition)
      r.apply_cond

  (* apply the rule r the list of hook concrete nodes d *)
  let apply_rule r p h =
    let start = Sys.time () in
    (* identify the nodes *)
    let left_nodes = identify_nodes r h in
    (* check conditions *)
    check_conditions r p left_nodes ;
    let orbit_length = Array.length left_nodes.(0) in
    let nb_right_nodes = Array.length r.right_nodes in
    let from = Array.make nb_right_nodes (-1) in
    Array.iteri (fun i j -> if j >= 0 then from.(j) <- i) r.node_match ;
    (* produce the serveral copies of the orbit variable *)
    let right_nodes =
      Array.init nb_right_nodes (fun i ->
          if from.(i) >= 0 then left_nodes.(from.(i))
          else Array.init orbit_length (fun _ -> add_dart ()))
    and trans_name d node_name = left_nodes.(node_name).(d)
    and get_param c = List.assoc c p
    and ebd_compute_orbit ebd_o ori_o ren_o =
      List.fold_left2
        (fun res ori ren ->
          if ren != -1 && List.mem ren ebd_o then ori :: res else res)
        []
        ori_o
        ren_o
    in
    (* construct the hash table for the original orbit *)
    let table = Hashtbl.create orbit_length and first_given = List.hd r.hooks in
    Array.iteri (fun i d -> Hashtbl.add table d i) left_nodes.(first_given) ;
    (* relabel the copies *)
    for i = 0 to nb_right_nodes - 1 do
      if from.(i) < 0 then
        List.iter2
          (fun a0 a1 ->
            if a1 >= 0 then
              for j = 0 to orbit_length - 1 do
                let d =
                  right_nodes.(i).(Hashtbl.find
                                     table
                                     (alpha a0 left_nodes.(first_given).(j)))
                in
                link a1 [] right_nodes.(i).(j) d
              done)
          r.left_nodes.(first_given)
          r.right_nodes.(i)
    done ;
    (* add links between copies *)
    List.iter
      (fun (i, j, a) ->
        if from.(i) < 0 && from.(j) < 0 && i != j then
          for k = 0 to orbit_length - 1 do
            link a [] right_nodes.(i).(k) right_nodes.(j).(k)
          done)
      r.right_edges ;
    (* compute embedding expression before transform toplogy *)
    let ebd_values = ref [] in
    List.iter
      (fun (i, name, e) ->
        let me = get_mark ()
        and oe =
          if from.(i) >= 0 then
            ebd_compute_orbit
              (ebd_orbit name)
              r.left_nodes.(first_given)
              r.right_nodes.(i)
          else ebd_orbit name
        in
        for j = 0 to orbit_length - 1 do
          if not (is_marked me right_nodes.(i).(j)) then (
            ebd_values :=
              ( right_nodes.(i).(j),
                name,
                R_sig.eval_expr (trans_name j) get_param e )
              :: !ebd_values ;
            orbit_iter (mark me) oe right_nodes.(i).(j) )
        done ;
        free_mark me)
      r.ebd_expr ;
    (* remove the left edges *)
    List.iter
      (fun (i, j, a) ->
        for k = 0 to orbit_length - 1 do
          assert (alpha a left_nodes.(i).(k) = left_nodes.(j).(k)) ;
          unlink a left_nodes.(i).(k)
        done)
      r.left_edges ;

    (* delete darts of removed nodes *)
    Array.iteri
      (fun i j ->
        if j < 0 then
          Array.iter
            (fun d ->
              for a = 0 to dim do
                unlink a d
              done ;
              delete_dart d)
            left_nodes.(i))
      r.node_match ;
    (* relabel the orginal orbit *)
    for i = 0 to nb_right_nodes - 1 do
      if from.(i) >= 0 then
        Array.iter
          (fun d ->
            let old_alpha = Array.init (dim + 1) (fun i -> alpha i d) in
            List.iter2
              (fun a0 a1 ->
                if a0 != a1 then (
                  unlink a0 d ;
                  if a1 > 0 then link a1 [] d old_alpha.(a0) ))
              r.left_nodes.(from.(i))
              r.right_nodes.(i))
          right_nodes.(i)
    done ;
    (* add links of orginal orbit *)
    List.iter
      (fun (i, j, a) ->
        if (from.(i) >= 0 || from.(j) >= 0) && i != j then
          for k = 0 to orbit_length - 1 do
            link a [] right_nodes.(i).(k) right_nodes.(j).(k)
          done)
      r.right_edges ;
    (* affect embeddings *)
    List.iter (fun (d, name, e) -> set_ebd name e d) !ebd_values ;
    Printf.printf "rule application time : %f\n" (Sys.time () -. start) ;
    flush stdout ;
    right_nodes
end
