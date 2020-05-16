(* embedded G-map and rule implementation *)

open Gmap_sig

module Pgmap : PARA_GMAP =
functor
  (G_sig : EBD_GMAP_SIG)
  ->
  struct
    (*------ G-map data types and functions ------*)

    module StdArray = Array

    type 'a std_array = 'a array

    module ResArray = Res.Array

    let dim = G_sig.dim

    type t_ebd = G_sig.t_ebd

    (* je ne comprends pas l'interet de ce type normalement
       c'est un plongement *)
    type t_info = G_sig.t_info

    type t_dart = int

    type t_mark = int

    type t_dart_ebd = Undefined | Dart of t_dart | Ebd of t_ebd

    type t_dart_info = None | Some of t_info

    exception Undefined_Ebd

    exception None_Info

    (*
     Count the number of embedded present in the gmap
  *)
    let nb_ebd = Array.length G_sig.ebd_names

    (*
     internal use (remain embedded and position)
  *)
    let ebd_table = Hashtbl.create nb_ebd

    (* fulfill the hashtable of the embedded *)

    let () =
      Array.iteri (fun i name -> Hashtbl.add ebd_table name i) G_sig.ebd_names

    (* search the correct index in the array present in the dart *)
    let ebd_index name = Hashtbl.find ebd_table name

    (* return the correct orbit for a specific embedded's index  *)
    let ebd_orbit_index index = G_sig.ebd_orbits.(index)

    (* return the name of the embedded according to its index *)
    let ebd_orbit name = ebd_orbit_index (ebd_index name)

    (*
     fait un tableau de liste vide egale a la dim+1
     (pourquoi +1...????)
  *)
    let alpha_ebd_ind = Array.make (dim + 1) []

    (* remplit le tableau precedent avec les orbits de support
       pour chaque alpha. Au final, le tableau contiendra
       pour chaque alphaI les indices des plongements supports
       NB: vraiment bien pense!
    *)

    let () =
      Array.iteri
        (fun i l ->
          List.iter (fun a -> alpha_ebd_ind.(a) <- i :: alpha_ebd_ind.(a)) l)
        G_sig.ebd_orbits

    (* definition of the dart <ul> <li>alpha is array corresponding to
        the alphaI and contain the number of the dart</li> <li>mark is a
        32bits markers used inner algorithm (especially the delete
        process)</li> <li>ebd: array of embedded where index is in
        relation with the declarationof the embedded gmap!</li>
        <li>info: used for showing the gmap object (*depreacated: it
        must be a triplet of float*)</li> </ul> *)
    type t_dart_record =
      { alpha : t_dart std_array;
        mutable mark : t_mark;
        mutable ebd : t_dart_ebd array;
        mutable info : t_dart_info
      }

    (* G-map is an risizable array, similar to dynamic pointers *)
    let gmap = ref (ResArray.of_array [||])

    (* accesseur au brin d (de type t_dart_record) *)
    (* define an accessor to the d dart *)
    let dart d = ResArray.get !gmap d

    (* tableau de 32 booleen indiquant si le marqueur associe est en
       cours d'utilisation *)
    (* array of 32 boolean that indicate if its associates marker is
        currently in use or not *)
    let used_marks = ref (Array.make 32 false)

    (* un marqueur est une liste*)
    let marked_darts = ref (Array.make 32 [])

    (* return the marker dedicated to the delete process (it is the
        first marker) *)
    let deleted =
      !used_marks.(0) <- true ;
      0

    (* vide la gcarte et tous les marqueurs *)
    (* empty the gmap and reset all markers *)
    let empty () =
      for i = 0 to 31 do
        !marked_darts.(i) <- []
      done ;
      ResArray.clear !gmap

    (* permet de recuperer le brin a l'autre cote d'une arete alpha i
       emanent du brin d *)
    (* Return the dart number corresponding to the alpha I from the
        dart d *)
    let alpha i d = (dart d).alpha.(i)

    (* affecte un marqueur et renvoie le premier indice trouve
       NB: je ne comprend pas pourquoi, il ne reaffecte pas
       une liste vide dans l'autre tableau
    *)
    (* Affect a new marker and return the first index free among the 32
        possible marker. <b>No control is made for accessing to higher
        marker</b> *)
    let get_mark () =
      let rec get_mark_rec m =
        if not !used_marks.(m) then (
          !used_marks.(m) <- true ;
          m )
        else get_mark_rec (m + 1)
      in
      get_mark_rec 1

    (* recupere la liste de (je ne sais pas quoi)  *)
    (* Return the list of dart marked by the marker m *)
    let marked m = !marked_darts.(m)

    (* libere une marque  *)
    (* Free the marker m *)
    let free_mark m =
      List.iter
        (fun d -> (dart d).mark <- (dart d).mark land lnot (1 lsl m))
        !marked_darts.(m) ;
      !marked_darts.(m) <- [] ;
      !used_marks.(m) <- false

    (* marque le marqueur dans le brin et les tableaux de marqueurs *)
    (* Mark the maker m inside the dart d  *)
    let mark m d =
      (dart d).mark <- (dart d).mark lor (1 lsl m) ;
      !marked_darts.(m) <- d :: !marked_darts.(m)

    (* Unmark the marker m inside the dart d *)
    let unmark m d =
      (dart d).mark <- (dart d).mark lxor (1 lsl m) ;
      !marked_darts.(m) <-
        List.fold_left
          (fun a b -> if b = d then a else b :: a)
          []
          !marked_darts.(m)

    (* Return true if the marker m inside the dart d is currently
        used *)
    let is_marked m d = (dart d).mark land (1 lsl m) > 0

    (* indique si le marqueur est pret à la suppression *)
    (* Indicates if the delete marker (the first) is in used for a
        dart *)
    let is_deleted = is_marked deleted

    (* recupere le nombre de brin non marque *)
    (* Retrieve the number of dart (without counting those marked as
        deleted) *)
    let nb_darts () =
      ResArray.length !gmap - List.length !marked_darts.(deleted)

    (* predicat indiquand si le chemin vert dans la these de thomas
       Dart indique un autre noeud portant l'info de plongement
       et sinon c bien le noeud.
       NB: On peut avoir un brin sans plongement ca donne Undefined
       considere comme un plongement
    *)
    (* Return true if the dart d is the carrier of the embedded name,
        else false *)
    let _is_ebd_carrier name d =
      match (dart d).ebd.(ebd_index name) with Dart _ -> false | _ -> true

    (*
    recupere le brin transportant l'info de plongement
    NB: phase de consultation, on reduit le nombre de saut
    jusqu'au brin de transport.
  *)
    (* Retrieve the dart that carrier the embedded at the position
        index in the embedded definition *)
    let rec ebd_carrier index d =
      match (dart d).ebd.(index) with
      | Dart b ->
          let c = ebd_carrier index b in
          (dart d).ebd.(index) <- Dart c ;
          c
          (* ici raccourci de chaine *)
      | _ -> d

    (*
    recupere le plongement donne par son nom a partir
    du brin d
  *)
    (* Return the embedded name for a dart d (the dart is not
        necessarily the carrier) *)
    let ebd name d =
      let index = ebd_index name in
      match (dart (ebd_carrier index d)).ebd.(index) with
      | Ebd e -> e
      | _ -> raise Undefined_Ebd

    (*
    indique le plongement name est accessible a partir du brin d
  *)
    (* Indicates if the embedded name can be retrieve from the dart d*)
    let has_ebd name d =
      let index = ebd_index name in
      match (dart (ebd_carrier index d)).ebd.(index) with
      | Undefined -> false
      | _ -> true

    (*
    Affecte le plongement name ayant la valeur e au brin d
  *)
    (* Affect the embedded name with the value e for the dart d*)
    let set_ebd name e d =
      let index = ebd_index name in
      (dart (ebd_carrier index d)).ebd.(index) <- Ebd e

    (* accesseur a l'info info du brin d *)
    let info d = match (dart d).info with Some i -> i | _ -> raise None_Info

    (* indique si une info est present dans le brin d *)
    let has_info d = match (dart d).info with None -> false | _ -> true

    (* affect une info i a un brin d *)
    let set_info i d = (dart d).info <- Some i

    (* NB: comprend pas *)
    let label d = d

    (* comprend pas *)
    let from_label d = d

    (* ajout d'un brin dans la gcarte en cours (en recyclant les
       case memoire inutile *)
    let add_dart () =
      match !marked_darts.(deleted) with
      | [] ->
          let index = ResArray.length !gmap in
          ResArray.add_one
            !gmap
            { alpha = StdArray.make (dim + 1) index;
              mark = 0;
              ebd = StdArray.make nb_ebd Undefined;
              info = None
            } ;
          index
      | d :: l ->
          !marked_darts.(deleted) <- l ;
          StdArray.fill (dart d).alpha 0 (dim + 1) d ;
          (dart d).mark <- 0 ;
          StdArray.fill (dart d).ebd 0 nb_ebd Undefined ;
          (dart d).info <- None ;
          d

    (* ajoute un brin en affectant le plongement facon thomas.

       NB: je pense encore une fois que le plongement n'est pas
       generique*)
    let add_ebd_dart ebd_list =
      let d = add_dart () in
      List.iter (fun (name, e) -> set_ebd name e d) ebd_list ;
      d

    (* supprime un brin d'une gcarte. Il faut s'assurer que plus aucun
       alpha i ne pointe sur un autre brin et ensuite aucun marqueur
    *)
    let delete_dart d =
      for i = 0 to dim do
        assert (alpha i d = d)
      done ;
      (* assure plus de connexion *)
      for m = 0 to 31 do
        if is_marked m d then unmark m d
      done ;
      mark deleted d

    (* fonction de parcours de tous les brins (non supprimes) de la gcarte*)
    let gmap_iter f =
      ResArray.iteri (fun d _ -> if not (is_deleted d) then f d) !gmap

    let gmap_fold_left f a =
      let len = ResArray.length !gmap in
      let rec fold_aux a d = if d < len then f (fold_aux a (d + 1)) d else a in
      fold_aux a 0

    let gmap_map f = gmap_fold_left (fun a d -> f d :: a) []

    (* realise un parcours d'orbit o (liste d'alpha) et d'un noeud de
       depart. Pour ce parcours, on applique une procedure f sur chaque
       brin! *)
    let orbit_iter f o d =
      let m = get_mark () in
      let rec iter_rec d =
        if not (is_marked m d) then (
          f d ;
          mark m d ;
          List.iter (fun i -> iter_rec (alpha i d)) o )
      in
      iter_rec d ;
      free_mark m

    let _orbit_fold_left f a o d =
      let m = get_mark () in
      let rec fold_rec a d =
        if not (is_marked m d) then (
          mark m d ;
          f (List.fold_left (fun l i -> fold_rec l (alpha i d)) a o) d )
        else a
      in
      let l = fold_rec a d in
      free_mark m ;
      l

    let orbit_fold_left f a o d =
      let l = ref [] in
      orbit_iter (fun d -> l := d :: !l) o d ;
      List.fold_left f a !l

    let orbit_map f = orbit_fold_left (fun a d -> f d :: a) []

    (* renvoie la liste des plongements name collecte pour l'orbite o
       à partir du brin d *)
    let ebd_collect name o d =
      let m = get_mark () and index = ebd_index name in
      let orbit = ebd_orbit_index index in
      let rec mark_rec d =
        if not (is_marked m d) then (
          mark m d ;
          List.iter (fun i -> mark_rec (alpha i d)) orbit )
      in
      let l =
        orbit_fold_left
          (fun a d ->
            if is_marked m d then a
            else (
              mark_rec d ;
              ebd name d :: a ))
          []
          o
          d
      in
      free_mark m ;
      l

    (* compte le nombre de plongement pour le plongement name dans le graphe  *)
    let nb_ebd_values name =
      let index = ebd_index name in
      gmap_fold_left
        (fun a d -> match (dart d).ebd.(index) with Ebd _ -> a + 1 | _ -> a)
        0

    (* unify embedding of two sewed darts *)
    let unify_ebd index b d1 d2 =
      let c1 = ebd_carrier index d1 and c2 = ebd_carrier index d2 in
      if c1 != c2 then
        if b then (dart c2).ebd.(index) <- Dart c1
        else (dart c1).ebd.(index) <- Dart c2

    (* duplicate embedding of two unliked darts *)
    let separate_ebd index d1 d2 =
      assert (d1 != d2) ;
      let c1 = ebd_carrier index d1 and c2 = ebd_carrier index d2 in
      if
        c1 = c2
        && orbit_fold_left
             (fun b d -> b && d != d2)
             true
             (ebd_orbit_index index)
             d1
      then (
        let e = (dart c1).ebd.(index) in
        orbit_iter
          (fun d -> (dart d).ebd.(index) <- (if d = d1 then e else Dart d1))
          (ebd_orbit_index index)
          d1 ;
        orbit_iter
          (fun d -> (dart d).ebd.(index) <- (if d = d2 then e else Dart d2))
          (ebd_orbit_index index)
          d2 )

    let link a unify_list d1 d2 =
      (* assert(alpha a d1=d1 && alpha a d2=d2) ; *)
      (dart d1).alpha.(a) <- d2 ;
      (dart d2).alpha.(a) <- d1 ;
      let unify_array = Array.make nb_ebd true in
      List.iter (fun (name, b) -> unify_array.(ebd_index name) <- b) unify_list ;
      List.iter
        (fun index -> unify_ebd index unify_array.(index) d1 d2)
        alpha_ebd_ind.(a)

    let unlink a d1 =
      let d2 = alpha a d1 in
      if d2 != d1 then (
        (dart d2).alpha.(a) <- d2 ;
        (dart d1).alpha.(a) <- d1 ;
        List.iter (fun index -> separate_ebd index d1 d2) alpha_ebd_ind.(a) )

    exception Unconsistant_Gmap

    (* check G-map constraints:
       <ol>
        <li>forall i in (alpha i (alpha i d)) = d</li>
        <li>forall i,j (i-j>=2), (alpha i (alpha j (alpha i (alpha j d)))) = d</li>
        </ol>
    *)
    let check_gmap () =
      gmap_iter (fun d ->
          for i = 0 to dim do
            if alpha i (alpha i d) != d then raise Unconsistant_Gmap ;
            for j = i + 2 to dim do
              if alpha i (alpha j d) != alpha j (alpha i d) then
                raise Unconsistant_Gmap
            done
          done)

    type t_state =
      { gmap_state : t_dart_record ResArray.t;
        mark_state : t_mark list array * bool array
      }

    (* prepare une copy de la gcarte en cours *)
    let get_state () =
      { gmap_state =
          ResArray.init (ResArray.length !gmap) (fun i ->
              let d = ResArray.get !gmap i in
              { alpha = Array.copy d.alpha;
                mark = d.mark;
                ebd = Array.copy d.ebd;
                info = d.info
              });
        mark_state = (Array.copy !marked_darts, Array.copy !used_marks)
      }

    (* restaure une copie de gcarte*)
    let set_state state =
      gmap := state.gmap_state ;
      marked_darts := fst state.mark_state ;
      used_marks := snd state.mark_state
  end
