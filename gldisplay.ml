(* 3D OpenGl display module *)

(*------ quaternion used by camera ------*)

type t_quaternion =
  { mutable m_x : float;
    mutable m_y : float;
    mutable m_z : float;
    mutable m_w : float
  }

let pi = 3.14159265358979323846

let create () = { m_x = 0.; m_y = 0.; m_z = 0.; m_w = 0. }

let createFromAxisAngle x y z deg =
  let angle = deg /. 180. *. pi in
  let result = sin (angle /. 2.) in
  { m_w = cos (angle /. 2.);
    m_x = x *. result;
    m_y = y *. result;
    m_z = z *. result
  }

let ( ** ) q1 q2 =
  let r = create () in
  r.m_w <-
    (q1.m_w *. q2.m_w) -. (q1.m_x *. q2.m_x) -. (q1.m_y *. q2.m_y)
    -. (q1.m_z *. q2.m_z) ;
  r.m_x <-
    (q1.m_w *. q2.m_x) +. (q1.m_x *. q2.m_w) +. (q1.m_y *. q2.m_z)
    -. (q1.m_z *. q2.m_y) ;
  r.m_y <-
    (q1.m_w *. q2.m_y) +. (q1.m_y *. q2.m_w) +. (q1.m_z *. q2.m_x)
    -. (q1.m_x *. q2.m_z) ;
  r.m_z <-
    (q1.m_w *. q2.m_z) +. (q1.m_z *. q2.m_w) +. (q1.m_x *. q2.m_y)
    -. (q1.m_y *. q2.m_x) ;
  r

(*------ rotation matrix ------*)
let createMatrix q m =
  (* first row *)
  m.(0).(0) <- 1. -. (2. *. ((q.m_y *. q.m_y) +. (q.m_z *. q.m_z))) ;
  m.(0).(1) <- 2. *. ((q.m_x *. q.m_y) +. (q.m_z *. q.m_w)) ;
  m.(0).(2) <- 2. *. ((q.m_x *. q.m_z) -. (q.m_y *. q.m_w)) ;
  m.(0).(3) <- 0. ;
  (* second row *)
  m.(1).(0) <- 2. *. ((q.m_x *. q.m_y) -. (q.m_z *. q.m_w)) ;
  m.(1).(1) <- 1. -. (2. *. ((q.m_x *. q.m_x) +. (q.m_z *. q.m_z))) ;
  m.(1).(2) <- 2. *. ((q.m_z *. q.m_y) +. (q.m_x *. q.m_w)) ;
  m.(1).(3) <- 0. ;
  (* third row *)
  m.(2).(0) <- 2. *. ((q.m_x *. q.m_z) +. (q.m_y *. q.m_w)) ;
  m.(2).(1) <- 2. *. ((q.m_y *. q.m_z) -. (q.m_x *. q.m_w)) ;
  m.(2).(2) <- 1. -. (2. *. ((q.m_x *. q.m_x) +. (q.m_y *. q.m_y))) ;
  m.(2).(3) <- 0. ;
  (* fourth row *)
  m.(3).(0) <- 0. ;
  m.(3).(1) <- 0. ;
  m.(3).(2) <- 0. ;
  m.(3).(3) <- 1.

(*------ camera ------*)

type t_point = { mutable x : float; mutable y : float; mutable z : float }

type t_vector = { mutable i : float; mutable j : float; mutable k : float }

type t_sens = Pos | Neg | Nul

type t_camera =
  { mutable m_MaxPitchRate : float;
    mutable m_MaxHeadingRate : float;
    mutable m_HeadingDegrees : float;
    mutable m_PitchDegrees : float;
    mutable m_ForwardSens : t_sens;
    mutable m_MaxForwardVelocity : float;
    mutable m_ForwardVelocity : float;
    mutable m_LateralSens : t_sens;
    mutable m_MaxLateralVelocity : float;
    mutable m_LateralVelocity : float;
    mutable m_qHeading : t_quaternion option;
    mutable m_qPitch : t_quaternion option;
    m_Position : t_point;
    m_DirectionVector : t_vector;
    m_LateralVector : t_vector
  }

exception Some_value_exception

let some_value = function Some x -> x | None -> raise Some_value_exception

let ( *= ) v i =
  v.i <- v.i *. i ;
  v.j <- v.j *. i ;
  v.k <- v.k *. i

let fabs x = if x < 0. then -1. *. x else x

let changeForwardVelocity c s = c.m_ForwardSens <- s

let updateeForwardVelocity c =
  match c.m_ForwardSens with
  | Pos ->
      if c.m_ForwardVelocity <= 0. then
        c.m_ForwardVelocity <- c.m_MaxForwardVelocity *. 0.1
      else (
        c.m_ForwardVelocity <- c.m_ForwardVelocity *. 1.05 ;
        if c.m_ForwardVelocity > c.m_MaxForwardVelocity then
          c.m_ForwardVelocity <- c.m_MaxForwardVelocity )
  | Neg ->
      if c.m_ForwardVelocity >= 0. then
        c.m_ForwardVelocity <- -.c.m_MaxForwardVelocity *. 0.1
      else (
        c.m_ForwardVelocity <- c.m_ForwardVelocity *. 1.05 ;
        if c.m_ForwardVelocity < -.c.m_MaxForwardVelocity then
          c.m_ForwardVelocity <- -.c.m_MaxForwardVelocity )
  | Nul ->
      c.m_ForwardVelocity <- c.m_ForwardVelocity *. 0.97 ;
      if fabs c.m_ForwardVelocity < 0.005 then c.m_ForwardVelocity <- 0.

let changeLateralVelocity c s = c.m_LateralSens <- s

let updateeLateralVelocity c =
  match c.m_LateralSens with
  | Pos ->
      if c.m_LateralVelocity <= 0. then
        c.m_LateralVelocity <- c.m_MaxLateralVelocity *. 0.1
      else (
        c.m_LateralVelocity <- c.m_LateralVelocity *. 1.05 ;
        if c.m_LateralVelocity > c.m_MaxLateralVelocity then
          c.m_LateralVelocity <- c.m_MaxLateralVelocity )
  | Neg ->
      if c.m_LateralVelocity >= 0. then
        c.m_LateralVelocity <- -.c.m_MaxLateralVelocity *. 0.1
      else (
        c.m_LateralVelocity <- c.m_LateralVelocity *. 1.05 ;
        if c.m_LateralVelocity < -.c.m_MaxLateralVelocity then
          c.m_LateralVelocity <- -.c.m_MaxLateralVelocity )
  | Nul ->
      c.m_LateralVelocity <- c.m_LateralVelocity *. 0.97 ;
      if fabs c.m_LateralVelocity < 0.005 then c.m_LateralVelocity <- 0.

let changeHeading c degrees =
  if fabs degrees < fabs c.m_MaxHeadingRate then
    if
      (* defined so lets increment it but first we must check	  *)
      (* to see if we are inverted so that our heading will not *)
      (* become inverted	 *)
      (c.m_PitchDegrees > 90. && c.m_PitchDegrees < 270.)
      || (c.m_PitchDegrees < -90. && c.m_PitchDegrees > -270.)
    then c.m_HeadingDegrees <- c.m_HeadingDegrees -. degrees
    else c.m_HeadingDegrees <- c.m_HeadingDegrees +. degrees
  else if
    (* our heading is greater than the max heading rate that  *)
    (* we defined so we can only increment our heading by the *)
    (* maximum allowed value  *)
    degrees < 0.
  then
    if
      (* check to see if we are upside down *)
      (c.m_PitchDegrees > 90. && c.m_PitchDegrees < 270.)
      || (c.m_PitchDegrees < -90. && c.m_PitchDegrees > -270.)
    then
      (* ok we would normally decrement here but since we are *)
      (* upside down then we need to increment our heading	*)
      c.m_HeadingDegrees <- c.m_HeadingDegrees +. c.m_MaxHeadingRate
    else
      (* we are not upside down so decrement as usual *)
      c.m_HeadingDegrees <- c.m_HeadingDegrees -. c.m_MaxHeadingRate
  else if
    (* check to see if we are upside down *)
    (c.m_PitchDegrees > 90. && c.m_PitchDegrees < 270.)
    || (c.m_PitchDegrees < -90. && c.m_PitchDegrees > -270.)
  then
    (* ok we would normally increment here but since we are *)
    (* upside down then we nedd to decrement our heading	*)
    c.m_HeadingDegrees <- c.m_HeadingDegrees -. c.m_MaxHeadingRate
  else
    (* we are not upside down so increment as usual *)
    c.m_HeadingDegrees <- c.m_HeadingDegrees +. c.m_MaxHeadingRate ;
  (* we don't want our heading to run away from us either. Although it *)
  (* really doesn't matter I prefer to have my heading degrees within  *)
  (* the range of -360. to 360.	 *)
  if c.m_HeadingDegrees > 360. then
    c.m_HeadingDegrees <- c.m_HeadingDegrees -. 360.
  else if c.m_HeadingDegrees < -360. then
    c.m_HeadingDegrees <- c.m_HeadingDegrees +. 360.

let changePitch c degrees =
  if fabs degrees < fabs c.m_MaxPitchRate then
    (* our pitch is less than the max rate that we *)
    (* defined so lets increment it  *)
    c.m_PitchDegrees <- c.m_PitchDegrees +. degrees
  else if
    (* our pitch is greater than the max pitch rate that we *)
    (* defined so we can only increment our pitch by the *)
    (* maximum allowed value *)
    degrees < 0.
  then
    (* we are pitching down so decrement *)
    c.m_PitchDegrees <- c.m_PitchDegrees -. c.m_MaxPitchRate
  else
    (* we are pitching up so increment *)
    c.m_PitchDegrees <- c.m_PitchDegrees +. c.m_MaxPitchRate ;
  (* we don't want our pitch to run away from us. Although it *)
  (* really doesn't matter I prefer to have my pitch degrees  *)
  (* within the range of -360. to 360. *)
  if c.m_PitchDegrees > 360. then c.m_PitchDegrees <- c.m_PitchDegrees -. 360.
  else if c.m_PitchDegrees < -360. then
    c.m_PitchDegrees <- c.m_PitchDegrees +. 360.

let setPerspective c =
  let matrix = Array.make_matrix 4 4 0. in
  (* makes the quaternions that will represent our rotations *)
  c.m_qPitch <- Some (createFromAxisAngle 1. 0. 0. c.m_PitchDegrees) ;
  c.m_qHeading <- Some (createFromAxisAngle 0. 1. 0. c.m_HeadingDegrees) ;
  let m_qPitch = some_value c.m_qPitch
  and m_qHeading = some_value c.m_qHeading
  and m_qHeadingRight =
    createFromAxisAngle 0. 1. 0. (c.m_HeadingDegrees +. 90.)
  in
  (* combines the pitch and heading rotations and store the result in q *)
  createMatrix (m_qPitch ** m_qHeading) matrix ;
  (* let OpenGL set our new perspective on the world! *)
  GlMat.mult (GlMat.of_array matrix) ;
  (* Create a matrix from the pitch quaternion and set the j vector *)
  (* for our direction *)
  createMatrix m_qPitch matrix ;
  c.m_DirectionVector.j <- matrix.(2).(1) ;
  c.m_LateralVector.j <- matrix.(2).(1) ;
  (* combines the heading and pitch rotations and makes a matrix to get*)
  (* i and k vectors for our direction  *)
  createMatrix (m_qHeading ** m_qPitch) matrix ;
  c.m_DirectionVector.i <- matrix.(2).(0) ;
  c.m_DirectionVector.k <- matrix.(2).(2) ;
  createMatrix (m_qHeadingRight ** m_qPitch) matrix ;
  c.m_LateralVector.i <- matrix.(2).(0) ;
  c.m_LateralVector.k <- matrix.(2).(2) ;
  (* scales the direction by our speed *)
  c.m_DirectionVector *= c.m_ForwardVelocity ;
  c.m_LateralVector *= c.m_LateralVelocity ;
  (* increments our position by the vector *)
  c.m_Position.x <-
    c.m_Position.x +. c.m_DirectionVector.i +. c.m_LateralVector.i ;
  c.m_Position.y <-
    c.m_Position.y +. c.m_DirectionVector.j +. c.m_LateralVector.j ;
  c.m_Position.z <-
    c.m_Position.z +. c.m_DirectionVector.k +. c.m_LateralVector.k ;
  (* translates to our new position *)
  GlMat.translate
    ~x:(-1. *. c.m_Position.x)
    ~y:(-1. *. c.m_Position.y)
    ~z:c.m_Position.z
    ()

(*------ main display ------*)

let grid_mode = ref true

let mouseX = ref 0

let mouseY = ref 0

let viewW = ref 0

let viewH = ref 0

let grid_size = ref 10

let user_func = ref []

let mainW = ref 0

let subW = ref 0

let errMess = ref ""

let infoBox = ref true

let cam =
  { m_MaxForwardVelocity = 0.1;
    m_MaxLateralVelocity = 0.1;
    m_MaxPitchRate = 5.;
    m_MaxHeadingRate = 5.;
    m_PitchDegrees = 15.;
    m_HeadingDegrees = 0.;
    m_ForwardSens = Nul;
    m_ForwardVelocity = 0.;
    m_LateralSens = Nul;
    m_LateralVelocity = 0.;
    m_qHeading = None;
    m_qPitch = None;
    m_Position = { x = 0.; y = 3.; z = -10. };
    m_DirectionVector = { i = 0.; j = 0.; k = 0. };
    m_LateralVector = { i = 0.; j = 0.; k = 0. }
  }

(* text message displayed at the windows bottom *)
let drawString s =
  String.iter
    (fun c ->
      Glut.bitmapCharacter ~font:Glut.BITMAP_HELVETICA_12 ~c:(Char.code c))
    s

(* main windows display *)
let mainReshape ~w ~h =
  assert (h > 0) ;
  viewW := w ;
  viewH := h ;
  Glut.setWindow ~win:!subW ;
  Glut.reshapeWindow ~w:(w - 10) ~h:25 ;
  Glut.positionWindow ~x:5 ~y:(h - 30) ;
  Glut.setWindow ~win:!mainW ;
  GlDraw.viewport ~x:0 ~y:0 ~w ~h ;
  GlMat.mode `projection ;
  GlMat.load_identity () ;
  GluMat.perspective ~fovy:45. ~aspect:(float w /. float h) ~z:(0.1, 1000.) ;
  GlMat.mode `modelview ;
  GlMat.load_identity ()

(* bottom display *)
let subReshape ~w ~h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h ;
  GlMat.mode `projection ;
  GlMat.load_identity () ;
  GluMat.ortho2d ~x:(0., 1.) ~y:(0., 1.)

(* regular grid drawing *)
let drawGrid () =
  GlDraw.line_width 2. ;
  GlDraw.color ~alpha:1. (1., 0., 0.) ;
  GlDraw.begins `lines ;
  GlDraw.vertex3 (0., 0., 0.) ;
  GlDraw.vertex3 (1., 0., 0.) ;
  GlDraw.ends () ;
  GlDraw.color ~alpha:1. (0., 0., 1.) ;
  GlDraw.begins `lines ;
  GlDraw.vertex3 (0., 0., 0.) ;
  GlDraw.vertex3 (0., 0., -1.) ;
  GlDraw.ends () ;
  GlDraw.color ~alpha:1. (0., 1., 0.) ;
  GlDraw.begins `lines ;
  GlDraw.vertex3 (0., 0., 0.) ;
  GlDraw.vertex3 (0., 1., 0.) ;
  GlDraw.ends () ;
  GlDraw.color ~alpha:1. (0.4, 0.4, 0.4) ;
  GlDraw.line_width 1. ;
  GlDraw.line_width 1. ;
  for i = - !grid_size to !grid_size do
    GlDraw.begins `lines ;
    GlDraw.vertex3 (float i, 0., -.float !grid_size) ;
    GlDraw.vertex3 (float i, 0., float !grid_size) ;
    GlDraw.ends () ;
    GlDraw.begins `lines ;
    GlDraw.vertex3 (-.float !grid_size, 0., float i) ;
    GlDraw.vertex3 (float !grid_size, 0., float i) ;
    GlDraw.ends ()
  done

(* OpenGl main loop *)
let drawGLScene draw_function redraw_condition gl_list () =
  Glut.setWindow ~win:!mainW ;
  GlClear.clear [`color; `depth] ;
  GlMat.load_identity () ;
  setPerspective cam ;
  updateeForwardVelocity cam ;
  updateeLateralVelocity cam ;
  if !grid_mode then drawGrid () ;
  if redraw_condition () then (
    (* OpenGl list are used when scene has not change to reduce display time *)
    GlList.begins gl_list ~mode:`compile_and_execute ;
    draw_function () ;
    GlList.ends () )
  else GlList.call gl_list ;
  Gl.flush () ;
  Glut.swapBuffers ()

(* bottom loop *)
let drawBox get_info () =
  Glut.setWindow ~win:!subW ;
  if !errMess = "" then GlClear.color (0.25, 0.25, 0.25)
  else GlClear.color (0.75, 0.15, 0.15) ;
  GlClear.clear [`color; `depth] ;
  GlDraw.color (1., 1., 1.) ;
  GlPix.raster_pos ~x:0.015 ~y:0.35 () ;
  if !errMess = "" then
    List.iter (fun s -> drawString (s ^ "    ")) (get_info ())
  else drawString !errMess ;
  Glut.swapBuffers () ;
  Glut.setWindow ~win:!mainW

(* selection buffer *)
let select_buf = Raw.create_static `uint ~len:200000

(* return the id of the first encountered by selection *)
let select hits_draw hits_process =
  Glut.setWindow ~win:!mainW ;
  GlMisc.select_buffer select_buf ;
  ignore (GlMisc.render_mode `select) ;
  GlMat.mode `projection ;
  let mat = GlMat.get_matrix `projection_matrix in
  GlMat.push () ;
  GlMat.load_identity () ;
  GluMat.pick_matrix
    ~x:(float !mouseX)
    ~y:(float (!viewH - !mouseY))
    ~width:12.
    ~height:12. ;
  GlMat.mult mat ;
  GlMat.mode `modelview ;
  GlMisc.init_names () ;
  hits_draw () ;
  GlMat.mode `projection ;
  GlMat.pop () ;
  GlMat.mode `modelview ;
  Gl.flush () ;
  let hit = GlMisc.render_mode `render in
  if hit >= 1 then
    hits_process (Raw.get select_buf ~pos:(3 + Raw.get select_buf ~pos:0 - 1))

(* show/hide bottom *)
let displayBox () =
  Glut.setWindow ~win:!subW ;
  if !infoBox then Glut.hideWindow () else Glut.showWindow () ;
  infoBox := not !infoBox ;
  Glut.setWindow ~win:!mainW

let printError mess = errMess := mess

(* keys entries *)
let checkAlphaKeys down ~key ~x ~y =
  ignore x ;
  ignore y ;
  ( match char_of_int key with
  | '\027' -> exit 0
  (* esc *)
  | 'g' -> if down then grid_mode := not !grid_mode
  | 'q' -> changeLateralVelocity cam (if down then Neg else Nul)
  | 'd' -> changeLateralVelocity cam (if down then Pos else Nul)
  | 's' -> changeForwardVelocity cam (if down then Neg else Nul)
  | 'z' -> changeForwardVelocity cam (if down then Pos else Nul)
  | 'i' -> if down then displayBox ()
  | c -> (
      if down then try (List.assoc c !user_func) () with Not_found -> () ) ) ;
  Glut.postRedisplay ()

(* keyboard camera controls *)
let checkKeys down ~key ~x ~y =
  ignore x ;
  ignore y ;
  ( match key with
  | Glut.KEY_LEFT -> changeLateralVelocity cam (if down then Neg else Nul)
  | Glut.KEY_RIGHT -> changeLateralVelocity cam (if down then Pos else Nul)
  | Glut.KEY_DOWN -> changeForwardVelocity cam (if down then Neg else Nul)
  | Glut.KEY_UP -> changeForwardVelocity cam (if down then Pos else Nul)
  | _ -> () ) ;
  Glut.postRedisplay ()

(* mouse selction *)
let checkMouseButton hits_draw hits_process ~button ~state ~x ~y =
  ignore x ;
  ignore y ;
  ignore button ;
  if state = Glut.DOWN then (
    errMess := "" ;
    select hits_draw hits_process ;
    Glut.setCursor Glut.CURSOR_CYCLE )
  else Glut.setCursor Glut.CURSOR_INHERIT

(* mouse camera controls *)
let checkMouse ~x ~y =
  let deltaMouse = ref 0 in
  if x < !mouseX then (
    deltaMouse := !mouseX - x ;
    changeHeading cam (-0.3 *. float !deltaMouse) )
  else if x > !mouseX then (
    deltaMouse := x - !mouseX ;
    changeHeading cam (0.3 *. float !deltaMouse) ) ;
  if y < !mouseY then (
    deltaMouse := !mouseY - y ;
    changePitch cam (-0.3 *. float !deltaMouse) )
  else if y > !mouseY then (
    deltaMouse := y - !mouseY ;
    changePitch cam (0.3 *. float !deltaMouse) ) ;
  mouseX := x ;
  mouseY := y ;
  Glut.postRedisplay ()

let checkPassiveMouse ~x ~y =
  mouseX := x ;
  mouseY := y

(* idle OpenGl *)
let glVisibility ~state =
  let idle () =
    Glut.setWindow ~win:!mainW ;
    Glut.postRedisplay () ;
    Glut.setWindow ~win:!subW ;
    Glut.postRedisplay () ;
    Glut.setWindow ~win:!mainW
  in
  if state = Glut.VISIBLE then Glut.idleFunc ~cb:(Some idle)
  else Glut.idleFunc ~cb:None

let build_menu_callback menu_entries =
  let menu_callback ~value =
    if value >= 0 && value < Array.length menu_entries then
      snd menu_entries.(value) ()
  in
  ignore (Glut.createMenu ~cb:menu_callback) ;
  Array.iteri
    (fun i e -> Glut.addMenuEntry ~label:(fst e) ~value:i)
    menu_entries ;
  Glut.attachMenu ~button:Glut.RIGHT_BUTTON

(* OpenGl & main windows intialisation *)
let glInit title draw_function redraw_condition hits_draw hits_process get_info
    key_functions menu_entries =
  ignore (Glut.init ~argv:Sys.argv) ;
  Glut.initWindowSize ~w:600 ~h:600 ;
  Glut.initDisplayMode ~double_buffer:true ~alpha:true ~depth:true () ;
  mainW := Glut.createWindow ~title ;
  List.iter
    Gl.enable
    [ `depth_test;
      `alpha_test;
      `blend;
      `point_smooth;
      `polygon_smooth;
      `line_smooth;
      `color_material ] ;
  GlDraw.shade_model `flat ;
  GlClear.color (1., 1., 1.) ;
  GlClear.depth 1. ;
  GlFunc.depth_func `less ;
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha ;
  List.iter
    (fun h -> GlMisc.hint h `fastest)
    [`perspective_correction; `point_smooth; `line_smooth; `polygon_smooth] ;
  Glut.reshapeFunc ~cb:mainReshape ;
  Glut.ignoreKeyRepeat ~ignore:true ;
  Glut.keyboardFunc ~cb:(checkAlphaKeys true) ;
  Glut.keyboardUpFunc ~cb:(checkAlphaKeys false) ;
  Glut.specialFunc ~cb:(checkKeys true) ;
  Glut.specialUpFunc ~cb:(checkKeys false) ;
  Glut.mouseFunc ~cb:(checkMouseButton hits_draw hits_process) ;
  Glut.passiveMotionFunc ~cb:checkPassiveMouse ;
  Glut.motionFunc ~cb:checkMouse ;
  let gl_list =
    let base = GlList.gen_lists ~len:1 in
    GlList.nth base ~pos:0
  in
  Glut.displayFunc ~cb:(drawGLScene draw_function redraw_condition gl_list) ;
  Glut.visibilityFunc ~cb:glVisibility ;
  user_func := key_functions ;
  build_menu_callback menu_entries ;
  subW := Glut.createSubWindow ~win:!mainW ~x:5 ~y:570 ~w:590 ~h:25 ;
  Glut.displayFunc ~cb:(drawBox get_info) ;
  Glut.reshapeFunc ~cb:subReshape ;
  Glut.mainLoop () ;
  Raw.free_static select_buf
