(* 3D modeler prototype *)

open Float_triplet
open Sig_mod
open Rule_sig_mod
open Rule_sig_mod.Gmap
open Rule_mod

let () = Random.self_init ()

let point e = e

let color e = e

let point_ebd d = point (ebd "point" d)

let color_ebd d = color (ebd "color" d)

let rec orbit_sign o =
  match o with [] -> 0 | a :: l -> (1 lsl a) lor orbit_sign l

let rec rand_color () =
  let (x, y, z) = (Random.float 1., Random.float 1., Random.float 1.) in
  if
    ((x -. y) *. (x -. y)) +. ((y -. z) *. (y -. z)) +. ((z -. x) *. (z -. x))
    > 0.9
  then (x, y, z)
  else rand_color ()

let _selec = get_mark ()

let _mode = ref 0

exception Bad_Selection

(* export the scene in obj format for rendering *)
let export_obj file =
  let vtable = Hashtbl.create (nb_darts ())
  and vlist = ref []
  and vnlist = ref []
  and m = get_mark ()
  and count = ref 1 in
  gmap_iter (fun d ->
      if not (is_marked m d) then (
        Hashtbl.add vtable (label d) !count ;
        vlist := point_ebd d :: !vlist ;
        mark m d ;
        orbit_iter
          (fun di ->
            if not (is_marked m di) then Hashtbl.add vtable (label di) !count ;
            mark m di)
          [1; 2; 3]
          d ;
        count := !count + 1 )) ;
  free_mark m ;
  count := 1 ;
  let m = get_mark () and fnlist = ref [] in
  let rec face_aux i vertlist lablist d =
    if not (is_marked m d) then (
      mark m d ;
      if i = 0 then (
        let d1 = alpha 1 d in
        if not (is_marked m d1) then (
          vertlist := point_ebd d :: !vertlist ;
          lablist := Hashtbl.find vtable (label d) :: !lablist ;
          face_aux 1 vertlist lablist d1 ) )
      else face_aux 0 vertlist lablist (alpha 0 d) )
  in
  gmap_iter (fun d ->
      if not (is_marked m d) then (
        let flist = ref [] and clist = ref [] in
        orbit_iter
          (fun d ->
            if not (is_marked m d) then (
              let fvlist = ref [] and fllist = ref [] in
              face_aux 0 fvlist fllist d ;
              face_aux 0 fvlist fllist (alpha 0 d) ;
              let fc = tmean !fvlist in
              flist := (fc, !fllist) :: !flist ;
              clist := fc :: !clist ))
          [0; 1; 2]
          d ;
        let vc = tmean !clist in
        List.iter
          (fun (fc, fl) ->
            fnlist := (!count, fl) :: !fnlist ;
            count := !count + 1 ;
            let (xv, yv, zv) = fc -: vc in
            let nv = tnorm (xv, yv, zv) in
            vnlist := (xv /. nv, yv /. nv, zv /. nv) :: !vnlist)
          !flist )) ;
  free_mark m ;
  let f = open_out file in
  let rec save_v vs l =
    if l != [] then (
      save_v vs (List.tl l) ;
      output_string f vs ;
      List.iter
        (fun s ->
          output_char f ' ' ;
          output_string f s)
        (tsave (List.hd l)) ;
      output_char f '\n' )
  in
  let rec save_f l =
    if l != [] then (
      save_f (List.tl l) ;
      output_char f 'f' ;
      let (vn, fl) = List.hd l in
      List.iter
        (fun v ->
          output_char f ' ' ;
          output_string f (string_of_int v) ;
          output_string f "//" ;
          output_string f (string_of_int vn))
        fl ;
      output_char f '\n' )
  in
  save_v "v" !vlist ;
  save_v "vn" !vnlist ;
  save_f !fnlist ;
  close_out f

let link_color = [| (0., 0., 0.); (1., 0., 0.); (0., 0., 1.); (0., 1., 0.) |]

let from_point (x, y, z) = (x, z, -.y)

(* compute 3D points representing the darts *)
let calc_repr () =
  let m = get_mark ()
  and fcount = ref 0
  and vcount = ref 0
  and centers_keys = Hashtbl.create (nb_darts ())
  and centers_vals = Hashtbl.create (nb_darts () / 6) in
  let rec face_aux_1 i vlist d =
    if not (is_marked m d) then (
      mark m d ;
      Hashtbl.add centers_keys (label d) (!fcount, !vcount) ;
      if i = 0 then (
        let d1 = alpha 1 d in
        if not (is_marked m d1) then (
          vlist := point_ebd d :: !vlist ;
          face_aux_1 1 vlist d1 ) )
      else face_aux_1 0 vlist (alpha 0 d) )
  in
  let rec face_aux_2 i fc d =
    if not (is_marked m d) then (
      mark m d ;
      Hashtbl.add centers_keys (label d) (fc, !vcount) ;
      if i = 0 then face_aux_2 1 fc (alpha 1 d) else face_aux_2 0 fc (alpha 0 d)
      )
  in
  gmap_iter (fun d ->
      if not (is_marked m d) then (
        let flist = ref [] in
        fcount := !vcount + 1 ;
        orbit_iter
          (fun d ->
            if not (is_marked m d) then (
              let d3 = alpha 3 d in
              if is_marked m d3 then (
                let (fc, _) = Hashtbl.find centers_keys (label d3) in
                face_aux_2 0 fc d ;
                face_aux_2 0 fc (alpha 0 d) ;
                flist := Hashtbl.find centers_vals fc :: !flist )
              else
                let vlist = ref [] in
                face_aux_1 0 vlist d ;
                face_aux_1 0 vlist (alpha 0 d) ;
                let fc = tmean !vlist in
                Hashtbl.add centers_vals !fcount fc ;
                flist := fc :: !flist ;
                fcount := !fcount + 1 ))
          [0; 1; 2]
          d ;
        Hashtbl.add centers_vals !vcount (tmean !flist) ;
        vcount := !fcount )) ;
  free_mark m ;
  gmap_iter (fun d ->
      let (fc, vc) = Hashtbl.find centers_keys (label d) in
      let e = (0.5 *: point_ebd (alpha 0 d)) +: (0.5 *: point_ebd d)
      and f = Hashtbl.find centers_vals fc
      and v = Hashtbl.find centers_vals vc in
      set_info
        ( (0.9 *: ((0.9 *: ((0.9 *: point_ebd d) +: (0.1 *: v))) +: (0.1 *: e)))
        +: (0.1 *: f) )
        d)

let _repr_change = ref true

let _selec_colors =
  [ (1., 0.0, 0.);
    (1., 1., 0.);
    (0., 1., 0.);
    (0., 1., 1.);
    (0., 0., 1.);
    (1., 0., 1.) ]

(* OpenGl draw of the topological representation *)
let draw_topology () =
  let rec draw_selec cl = function
    | [] -> ()
    | d :: dl ->
        GlDraw.color ~alpha:1. (List.hd cl) ;
        GlDraw.vertex3 (from_point (info d)) ;
        draw_selec (List.tl cl) dl
  in
  if !_repr_change then (
    calc_repr () ;
    _repr_change := false ) ;
  GlDraw.line_width 2. ;
  for i = 0 to 3 do
    GlDraw.color ~alpha:1. link_color.(i) ;
    GlDraw.begins `lines ;
    gmap_iter (fun d ->
        if label d < label (alpha i d) then (
          GlDraw.vertex3 (from_point (info d)) ;
          GlDraw.vertex3 (from_point (info (alpha i d))) )) ;
    GlDraw.ends ()
  done ;
  GlDraw.color ~alpha:1. (0., 0., 0.) ;
  GlDraw.point_size 8. ;
  GlDraw.begins `points ;
  gmap_iter (fun d ->
      if not (is_marked _selec d) then GlDraw.vertex3 (from_point (info d))) ;
  GlDraw.ends () ;
  for _i = 0 to 2 do
    GlDraw.point_size 12. ;
    GlDraw.begins `points ;
    draw_selec _selec_colors (List.rev (marked _selec)) ;
    GlDraw.ends ()
  done

let face_mode = ref true

let line_mode = ref true

let topology_mode = ref true

(* OpenGl draw of the scene *)
let draw_faces () =
  if !topology_mode then draw_topology () ;
  if !face_mode then (
    let alpha_level = if !topology_mode then 0.5 else 1. in
    GlDraw.color ~alpha:alpha_level (0.5, 0.5, 0.5) ;
    let m = get_mark () in
    let rec ebd_face i vlist d =
      if is_marked m d then true
      else (
        mark m d ;
        mark m (alpha 3 d) ;
        if i = 0 then
          let d1 = alpha 1 d in
          if d1 = d then false
          else (
            vlist := from_point (point_ebd d) :: !vlist ;
            ebd_face 1 vlist d1 )
        else
          let d0 = alpha 0 d in
          if d0 = d then false else ebd_face 0 vlist d0 )
    in
    gmap_iter (fun d ->
        if not (is_marked m d) then
          let vlist = ref [] in
          if ebd_face 0 vlist d then (
            GlDraw.color ~alpha:alpha_level (color_ebd d) ;
            GlDraw.begins `polygon ;
            List.iter GlDraw.vertex3 !vlist ;
            GlDraw.ends () )) ;
    free_mark m ) ;
  if !line_mode && not !topology_mode then (
    let m = get_mark () in
    GlDraw.color ~alpha:1. (0., 0., 0.) ;
    GlDraw.line_width 2. ;
    GlDraw.begins `lines ;
    gmap_iter (fun d ->
        if not (is_marked m d) then (
          orbit_iter (mark m) [0; 2; 3] d ;
          GlDraw.vertex3 (from_point (point_ebd d)) ;
          GlDraw.vertex3 (from_point (point_ebd (alpha 0 d))) )) ;
    GlDraw.ends () ;
    free_mark m )

(* load under Moka format *)
let load_gmap load_vertex file =
  empty () ;
  try
    let s2i = int_of_string
    and f = open_in file
    and darts = ref []
    and points = ref []
    and eof = ref false in
    ( try
        ignore (input_line f) ;
        ignore (input_line f)
      with End_of_file -> raise (Sys_error "wrong format") ) ;
    while not !eof do
      try
        let s = input_line f in
        let r = Str.regexp "\t" in
        let sl = Str.split r s in
        match sl with
        | [i1; i2; i3; i4; _; _; _; _; _] ->
            let _ = add_dart () in
            darts := [| s2i i1; s2i i2; s2i i3; s2i i4 |] :: !darts
        | i1 :: i2 :: i3 :: i4 :: _ :: _ :: _ :: _ :: _ :: ebd ->
            let d = add_dart () in
            points := (d, load_vertex ebd) :: !points ;
            darts := [| s2i i1; s2i i2; s2i i3; s2i i4 |] :: !darts
        | _ ->
            print_endline (file ^ " is not a valid moka file") ;
            exit (-1)
      with End_of_file -> eof := true
    done ;
    close_in f ;
    Array.iteri
      (fun d to_sew ->
        for i = 0 to dim do
          if alpha i (from_label d) != from_label to_sew.(i) then
            link i [] (from_label d) (from_label to_sew.(i))
        done)
      (Array.of_list (List.rev !darts)) ;
    List.iter (fun (d, e) -> set_ebd "point" e d) !points ;
    gmap_iter (fun d ->
        if not (has_ebd "color" d) then set_ebd "color" (rand_color ()) d)
  with Sys_error _ -> Gldisplay.printError "Incorrect file name or format"

(* save under Moka format *)
let save_gmap save_vertex file =
  let index = Array.make (nb_darts ()) 0 and current = ref 0 in
  gmap_iter (fun d ->
      index.(label d) <- !current ;
      current := !current + 1) ;
  let f = open_out file in
  output_string f "Moka file [ascii]\n" ;
  output_string f "0 0 0 0 0 0 0 0 \n" ;
  let m = get_mark () in
  gmap_iter (fun d ->
      for i = 0 to dim do
        output_string f (string_of_int index.(label (alpha i d))) ;
        output_char f '\t'
      done ;
      output_string f "0\t0\t0\t0\t" ;
      if not (is_marked m d) then (
        orbit_iter (mark m) [1; 2; 3] d ;
        output_char f '1' ;
        List.iter
          (fun s ->
            output_char f '\t' ;
            output_string f s)
          (save_vertex (point_ebd d)) )
      else output_char f '0' ;
      output_char f '\n') ;
  free_mark m ;
  close_out f

let text_info = ref [""; ""; ""; ""]

let _redraw = ref true

(* OpenGL call list are clear when G-map has changed *)
let require_redraw repr_change =
  let struct_info = ref (List.rev (List.tl (List.rev !text_info))) in
  if repr_change then (
    _repr_change := true ;
    struct_info :=
      [ "number of darts: " ^ string_of_int (nb_darts ());
        "number of points: " ^ string_of_int (nb_ebd_values "point");
        "number of colors: " ^ string_of_int (nb_ebd_values "color") ] ) ;
  text_info :=
    !struct_info
    @ [ ( "mode: "
        ^
        match !_mode with
        | 0 -> "node"
        | 1 -> "vertex"
        | 2 -> "edge"
        | 3 -> "face"
        | 4 -> "volume"
        | 5 -> "connex"
        | _ -> "g-map" ) ] ;
  _redraw := true

let redraw_condition () =
  if !_redraw then (
    _redraw := false ;
    true )
  else false

(* selection points (in fact darts repr) OpenGL draw fonction *)
let draw_hits () =
  if !topology_mode then
    gmap_iter (fun d ->
        GlMisc.push_name (label d) ;
        GlDraw.begins `points ;
        GlDraw.vertex3 (from_point (info d)) ;
        GlDraw.ends () ;
        GlMisc.pop_name ())

(* function called when a dart is selected *)
let treat_hits l =
  let d = from_label l in
  if is_marked _selec d then unmark _selec d
  else (
    if List.length (marked _selec) >= if !_mode = 0 then 6 else 1 then
      unmark _selec (List.hd (marked _selec)) ;
    mark _selec d ) ;
  require_redraw false

let unselect_all () = List.iter (unmark _selec) (marked _selec)

let my_rules =
  ref
    [ ( "connex removal",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| -1 |];
          right_nodes = [||];
          right_edges = [];
          apply_cond = [];
          ebd_expr = []
        } ) ]

let load_rules () =
  ( try
      ignore (Sys.command "ocamlc -c rules_mod.ml") ;
      Dynlink.loadfile "rules_mod.cmxs"
    with
  | Dynlink.Error e -> prerr_string ("ERROR: " ^ Dynlink.error_message e ^ "\n")
  | Sys_error e -> prerr_string ("ERROR: " ^ e ^ "\n") ) ;
  let nb_rules = ref 0 in
  let l_rules =
    List.fold_right
      (fun (name, r) a ->
        let (b, l) = check_rule r in
        if not b then (
          Printf.printf
            "%s"
            ("***WARNING*** The rule " ^ name ^ " is incorrect:\n") ;
          Printf.printf "%s" l ;
          a )
        else (
          nb_rules := !nb_rules + 1 ;
          (name, r) :: a ))
      !mod_rule_list
      []
  in
  Printf.printf "%i rules have been loaded.\n" !nb_rules ;
  flush stdout ;
  l_rules

let rule_list = ref (load_rules ())

let get_rule name = List.assoc name !rule_list

let vertex_extrusion = get_rule "vertex extrusion"

let edge_extrusion = get_rule "edge extrusion"

let face_extrusion = get_rule "face extrusion"

let edge_cone = get_rule "edge cone"

let face_cone = get_rule "face cone"

let color_left_shift = get_rule "color left shift"

let color_right_shift = get_rule "color right shift"

(* some primitives using rules *)
let tetrahedron () =
  let d = add_ebd_dart [("point", (-2., -2., -1.)); ("color", rand_color ())] in
  ignore (apply_rule vertex_extrusion [('v', PConst (4., 0., 0.))] [d]) ;
  ignore (apply_rule edge_cone [('v', PConst (0., 3., 0.))] [d]) ;
  ignore (apply_rule face_cone [('v', PConst (0., 0., 3.))] [d]) ;
  ignore (apply_rule color_left_shift [] [alpha 2 d]) ;
  ignore (apply_rule color_right_shift [] [alpha 2 (alpha 1 d)])

let cube () =
  let d = add_ebd_dart [("point", (-2., -2., -2.)); ("color", rand_color ())] in
  ignore (apply_rule vertex_extrusion [('v', PConst (4., 0., 0.))] [d]) ;
  ignore (apply_rule edge_extrusion [('v', PConst (0., 4., 0.))] [d]) ;
  ignore (apply_rule face_extrusion [('v', PConst (0., 0., 4.))] [d]) ;
  ignore (apply_rule color_left_shift [] [alpha 2 d]) ;
  ignore
    (apply_rule color_left_shift [] [alpha 2 (alpha 1 (alpha 0 (alpha 1 d)))]) ;
  ignore (apply_rule color_right_shift [] [alpha 2 (alpha 1 d)]) ;
  ignore (apply_rule color_right_shift [] [alpha 2 (alpha 1 (alpha 0 d))])

let pyramid () =
  let d = add_ebd_dart [("point", (-2., -2., 2.4)); ("color", rand_color ())] in
  ignore (apply_rule vertex_extrusion [('v', PConst (4., 0., 0.))] [d]) ;
  ignore (apply_rule edge_extrusion [('v', PConst (0., 4., 0.))] [d]) ;
  ignore (apply_rule face_cone [('v', PConst (0., 0., 3.))] [d]) ;
  ignore (apply_rule color_left_shift [] [alpha 2 d]) ;
  ignore
    (apply_rule color_left_shift [] [alpha 2 (alpha 1 (alpha 0 (alpha 1 d)))]) ;
  ignore (apply_rule color_right_shift [] [alpha 2 (alpha 1 d)]) ;
  ignore (apply_rule color_right_shift [] [alpha 2 (alpha 1 (alpha 0 d))])

let prev_state = ref (get_state ())

let create_menu rule_list =
  Array.map
    (fun (n, r) ->
      ( n,
        fun () ->
          ( prev_state := get_state () ;
            if !_mode = 0 then
              try ignore (apply_rule r [] (marked _selec)) with
              | Can_Not_Associate_Hook ->
                  Gldisplay.printError "wrong darts number selected"
              | Can_Not_Satisfy_Condition ->
                  Gldisplay.printError "conditions are not satisfied"
              | Can_Not_Match_Pattern ->
                  Gldisplay.printError "nodes can not be matched"
            else
              let m = get_mark () in
              ( if !_mode = 6 then
                gmap_iter (fun d ->
                    if not (is_marked m d) then
                      try
                        let right_nodes = apply_rule r [] [d] in
                        Array.iter (Array.iter (mark m)) right_nodes
                      with
                      | Can_Not_Associate_Hook ->
                          Gldisplay.printError "rule has multiple hooks"
                      | Can_Not_Satisfy_Condition | Can_Not_Match_Pattern -> ())
              else
                match marked _selec with
                | dm :: _ ->
                    orbit_iter
                      (fun d ->
                        if not (is_marked m d) then
                          try
                            let right_nodes = apply_rule r [] [d] in
                            Array.iter (Array.iter (mark m)) right_nodes
                          with
                          | Can_Not_Associate_Hook ->
                              Gldisplay.printError "rule has multiple hooks"
                          | Can_Not_Satisfy_Condition | Can_Not_Match_Pattern ->
                              ())
                      ( match !_mode with
                      | 1 -> [1; 2; 3]
                      | 2 -> [0; 2; 3]
                      | 3 -> [0; 1; 3]
                      | 4 -> [0; 1; 2]
                      | _ -> [0; 1; 2; 3] )
                      dm
                | [] -> Gldisplay.printError "no node selected" ) ;
              free_mark m ) ;
          require_redraw true ))
    (Array.of_list rule_list)

let key_functions =
  [ ( 'e',
      fun () ->
        unselect_all () ;
        empty () ;
        require_redraw true );
    ( '1',
      fun () ->
        cube () ;
        require_redraw true );
    ( '2',
      fun () ->
        pyramid () ;
        require_redraw true );
    ( '3',
      fun () ->
        tetrahedron () ;
        require_redraw true );
    ( 't',
      fun () ->
        topology_mode := not !topology_mode ;
        require_redraw false );
    ( 'f',
      fun () ->
        face_mode := not !face_mode ;
        require_redraw false );
    ( 'r',
      fun () ->
        line_mode := not !line_mode ;
        require_redraw false );
    ( '@',
      fun () ->
        _mode := (!_mode + 1) mod 7 ;
        if !_mode > 0 && List.length (marked _selec) > 0 then
          List.iter (unmark _selec) (List.tl (List.rev (marked _selec))) ;
        require_redraw false );
    ( 'u',
      fun () ->
        set_state !prev_state ;
        require_redraw true );
    ( '-',
      fun () ->
        unselect_all () ;
        require_redraw false );
    ( 'k',
      fun () ->
        rule_list := load_rules () ;
        Gldisplay.build_menu_callback (create_menu !rule_list) );
    ( 'l',
      fun () ->
        print_string "file to load ? " ;
        unselect_all () ;
        load_gmap tload (read_line ()) ;
        require_redraw true );
    ( 'm',
      fun () ->
        print_string "file to save ? " ;
        save_gmap tsave (read_line ()) );
    ( 'o',
      fun () ->
        print_string "obj file to export ? " ;
        export_obj (read_line ()) );
    ('x', fun () -> exit 0);
    ( ')',
      fun () ->
        try check_gmap ()
        with Unconsistant_Gmap -> Gldisplay.printError "gmap is unconsistant" )
  ]

let () = require_redraw true

let () =
  Gldisplay.glInit
    "mod"
    draw_faces
    redraw_condition
    draw_hits
    treat_hits
    (fun () -> !text_info)
    key_functions
    (create_menu !rule_list)
