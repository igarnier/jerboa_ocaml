let dim = 3;;
let alpha_ebd_ind = Array.make (dim+1) [] ;;
let mafonc = (fun i l -> List.iter (fun a ->
	alpha_ebd_ind.(a) <- i::alpha_ebd_ind.(a)) l)  ;;
let ebd_orbits=[|[1;2;3];[0;1;3];[1;2;3]|];;
Array.iteri mafonc ebd_orbits;;
alpha_ebd_ind;;
