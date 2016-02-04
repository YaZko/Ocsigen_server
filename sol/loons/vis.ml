open Graphics
open Batteries
open Input
       

let draw_line x y z t =
  moveto x y;
  lineto z t

	 
let iter_optarray f a =
  Array.iter
    (fun o -> match o with
		None -> ()
	      | Some x -> f x) a

			       
let draw_state loons targets posx posy =
  set_color red;
  targets |> Array.iter
	       (fun (r,c) -> fill_rect  (posx c) (posy r) (posx 1) (posy 1));
  set_color blue;
  loons |> iter_optarray
	     (fun (r,c) -> fill_rect (posx c) (posy r) (posx 1) (posy 1))

exception IllegalMovementVis of string			 
let vis data scen posx posy =
  open_graph "";

  resize_window (posx data.nb_C) (posy data.nb_R);
  set_color black;
  (1 -- data.nb_R) |> Enum.iter (fun r -> draw_line (posx 0) (posy r) (posx data.nb_C) (posy r));
  let loons = Array.init data.nb_B (fun _ -> Some data.start) in

  draw_state loons data.targets posx posy;
  ignore (wait_next_event [Key_pressed]);
  let loons = Array.init data.nb_B (fun _ -> Some data.start) in
  let alt   = Array.init data.nb_B (fun _ -> 0) in
  scen |> List.iter
		(fun (moves: int array) ->
		 (* Printf.printf "loons beginning of turn: %s at alt %s\n" (BatPervasives.dump loons) (BatPervasives.dump alt); *)
		 Array.iteri (fun i m -> 
			    if m = -1 (* on descend ! *)
			    then 
			      (if alt.(i) >= 2 then alt.(i) <- alt.(i) - 1
			       else raise (IllegalMovementVis ("Illegal diminution of altitude for balloon " ^ string_of_int i)))
			    else if m = 1 (* on monte ! *)
 			    then 
			      (if alt.(i) < data.nb_A then alt.(i) <- alt.(i) + 1
			       else raise (IllegalMovementVis ("Illegal augmentation of altitude for balloon " ^ string_of_int i)))
			    else if m = 0
			    then ()
			    else raise (IllegalMovementVis ("Unknown movement :" ^ string_of_int m))) moves; 

		 (* Printf.printf "loons after order: %s at alt %s\n" (dump loons) (dump alt); *)
		 
		 Array.iteri (fun i b -> 
			      (match b with
			       | Some (bx,by) ->
				  let a = alt.(i) in
				  if a = 0 then ()
				  else let (vx,vy) = data.wind.(a - 1).(bx).(by) in
				       let bx' = bx + vx in
				       let by' = (by + vy) mod data.nb_C in
				       (if by' < 0 then failwith "prout" else ());
				       let newb = if bx' >= 0 && bx' < data.nb_R then Some (bx', by') else None 
				       in loons.(i) <- newb
			       | None -> ())
		) loons;
		 clear_graph ();
		 draw_state loons data.targets posx posy;
		 ignore (wait_next_event [Key_pressed])
		)
