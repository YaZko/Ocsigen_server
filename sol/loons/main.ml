open Batteries
open Input
open Vis
       
exception IllegalMovement of string

let square x = x * x

let is_covered_by data (tx,ty) mb =
  let size_sq = square data.size in
  match mb with
  | Some (bx,by) ->
     let abt = abs (by - ty) in
     square (bx - tx) + square (min abt (data.nb_C - abt)) <= size_sq
  | None -> false
		     
let score_turn data loons alt =
  data.targets
  |> Array.fold_left
       (fun acc t ->
	let cov =
	  loons |>
	    Array.fold_lefti
	      (fun acc i b -> acc || (alt.(i) <> 0 && is_covered_by data t b))
	      false
	in
	if cov then acc + 1 else acc) 0

let move_alt data alt i m =
  if m = -1 (* on descend ! *)
  then 
    (if alt.(i) >= 2 then alt.(i) <- alt.(i) - 1
     else raise (IllegalMovement ("Illegal diminution of altitude for balloon " ^ string_of_int i)))
  else if m = 1 (* on monte ! *)
  then 
    (if alt.(i) < data.nb_A then alt.(i) <- alt.(i) + 1
     else raise (IllegalMovement ("Illegal augmentation of altitude for balloon " ^ string_of_int i)))
  else if m = 0
  then ()
  else raise (IllegalMovement ("Unknown movement :" ^ string_of_int m))

let move_wind data loons alt i b =
  match b with
  | Some (bx,by) ->
     let a = alt.(i) in
     if a = 0
     then () 			(* wind doesn't affect balloons still on ground *)
     else
       let (vx,vy) = data.wind.(a - 1).(bx).(by) in
       let bx' = bx + vx in
       let by' = (by + vy) mod data.nb_C in
       (if by' < 0 then raise (IllegalMovement "move_wind: illegal diminution of altitude") else ());
       loons.(i) <- (if bx' >= 0 && bx' < data.nb_R then Some (bx', by') else None)
  | None -> ()
	     
  
let simulate data scenario =
  let loons = Array.init data.nb_B (fun _ -> Some data.start) in
  let alt   = Array.init data.nb_B (fun _ -> 0) in
  scenario |>
    List.fold_left 
      (fun (acc: int) (moves: int array) ->
       moves |> Array.iteri (move_alt data alt); 
       loons |> Array.iteri (move_wind data loons alt);
       acc + score_turn data loons alt
      ) 0 


type node = { x : int;
	      y : int;
	      a : int; }
	      
module Node =
  struct
    let print oc {x;y;a} =
      String.print oc (Printf.sprintf "{x=%d;y=%d;a=%d}" x y a)
  end		   

module Dir =
  struct   
    type t = Up | Down | Stay
		     
    let da = function
	Up -> 1
      | Down -> -1
      | Stay -> 0
			     
    let print oc d =
      da d |> Int.print oc
  end
	      
type graph = (node, ( Dir.t * node ) list ) Hashtbl.t
					  
let move ({a} as n) dir =
  {n with
    a = a + Dir.da dir }

let wmap data : graph =
  let g : graph = Hashtbl.create 17 in 
  for x = 0 to data.nb_C - 1 do
    for y = 0 to data.nb_R - 1 do
      for a = 0 to data.nb_A - 1 do
	let n = {x;y;a} in
	Hashtbl.replace
	  g n
	  ([Dir.Up; Dir.Down; Dir.Stay]
	   |> List.filter_map
		(fun d ->
		 let {x;y;a} = move n d in
		 if (a = 0 && d = Dir.Stay) || (a > 0 && a < data.nb_A) then
		   try let (dy,dx) = data.wind.(a).(y).(x) in
		       let x = (x + dx) mod (data.nb_C - 1) in
		       let y = y + dy in
		       if y < 0 || y >= data.nb_R
		       then None
		       else Some (d, { x = x;
			      y = y ;
			      a = a })
		   with
		     Invalid_argument s -> failwith (Printf.sprintf "invalid access '%s' a=%d, x=%d, y=%d" s a x y)
		 else None
		))
      done
    done
  done;
  g

let safe_find h k =
  try Some (Hashtbl.find h k) with _ -> None

let is_uncovered_target data node t (cov : bool array array) =
  cov |>
    Array.fold_lefti (fun acc i l ->
		      acc || (not l.(t) && node.a <> 0 &&
			       is_covered_by data (data.targets.(i))
					     (Some (node.x, node.y)))
		     ) false
    
let reduce_triplets (a,b,c) (d,e,f) =
  if b = e then
    if a = Dir.Stay then (d,e,f) else (a,b,c)
  else if b > e then (a,b,c) else (d,e,f)

let rec route b data wmap turn pos loons alt =
  if turn >= data.nb_T then []
  else
    let (bestdir,_,bestnode) =
      match safe_find wmap pos with
	None -> failwith "Something went wrong in routing"
      | Some nexts ->
	 if nexts = []
	 then (Dir.Stay,0,pos)
	 else
	   nexts |>
	     List.map (
		 fun (dir, node) ->
		 let score = 
		   data.targets |>
		     Array.fold_left
		       (fun acc t ->
			let cov =
			  (* Are we covering target t in this turn? *)
			  loons.(turn) |>
			    Array.fold_lefti
			      (fun acc i b ->
			       acc (* is it already the case? *)
			       || (alt.(turn).(i) <> 0 && is_covered_by data t b) (* is t covered by in-flight balloon b *)
			       || (node.a <> 0 && is_covered_by data t (Some (node.x,node.y)))
			      (* is t covered by neighbour node ? *)
			      )
			      false
			in if cov then acc + 1 else acc)
		       0 in		      
		 (dir, score, node)
	       (* going in direction dir will cover score targets and our balloon b will be in position node *)
	       )
	   |> List.reduce reduce_triplets (* take the best choice *)
    in
    loons.(turn).(b) <- Some (bestnode.x, bestnode.y);
    alt.(turn).(b) <- (bestnode.a);
    bestdir :: route b data wmap (turn+1) bestnode loons alt
  

let set_route b (r: int list) (scen : int array list) =
  scen |> List.iteri
	    (fun i a ->
	     try a.(b) <- try List.nth r i with _ -> 0
	     with _ ->
	       (Printf.printf "set_route failed %d\n" b; failwith "bla")
	    )
	    
let print_scen (scen: int array list) =
  let oc = open_out "output" in
  scen |>
    List.iter
      (fun dirs ->
       dirs |> Array.iter (fun i -> Printf.fprintf oc "%d " i);
       Printf.fprintf oc "\n");
  close_out oc

let rec repeat n a =
  if n <= 0 then []
  else a::(repeat (n-1) a)

(* Strategy 1: take 1 route and launch balloons in waves on the same route *)
let strategy1 data w init wavelength =
  let loons = Array.init data.nb_T (fun t ->
				    Array.init data.nb_B (fun _ -> None))
  in
  let alt   = Array.init data.nb_T (fun _ -> Array.init data.nb_B (fun _ -> 0)) in
  let r =  route 0 data w 0 init loons alt in 
  let scen = (r |> List.map (fun i -> Array.init data.nb_B (fun b -> Dir.da i))) in
  (1 -- (data.nb_B-1)) |>
    Enum.iter (fun b -> set_route b (List.map Dir.da ((repeat (b*wavelength) Dir.Stay) @ r)) scen) ;
  scen

let strategy2 data w init wavelength =
  let loons = Array.init data.nb_T (fun t ->
				    Array.init data.nb_B (fun _ -> None))
  in
  let alt   = Array.init data.nb_T (fun _ -> Array.init data.nb_B (fun _ -> 0)) in

  (* let scen = Array.init data.nb_T (fun _ -> Array.init data.nb_B (fun _ -> 0)) in *)
  (* let scen = scen |> Array.to_list in *)
  
  (* let r = route 0 data w 0 init loons alt in *)
  (* let scen = (r |> List.map (fun i -> Array.init data.nb_B (fun b -> Dir.da i))) in *)
  (0 -- 9) |>
    Enum.map (fun b ->
	       Printf.printf "Balloon %d\n" b;
	       flush stdout;
	       (b, route b data w 0 init loons alt))

    
let do_vis data scen =
  let width = 2000 in
  let height = 600 in
  let scale = min ((float_of_int height) /. (float_of_int data.nb_R))
  		  ((float_of_int width) /. (float_of_int data.nb_C)) in
  let posy r = int_of_float ((float_of_int r) *. scale) in
  let posx c = int_of_float ((float_of_int c) *. scale) in
  vis data scen posx posy
    
let () = 
  let data = read_data (try Sys.argv.(1) with _ -> "ex.in") in
  let w = wmap data in
  let (y,x) = data.start in

  let wavelength = try Sys.argv.(2) |> int_of_string with _ -> 1 in

  (* Printf.printf "Wavelength = %d\n" wavelength; *)
  
  (* let routes = *)
  (*   strategy2 data w {x;y;a=0} wavelength in *)



  (* (1--15) |> Enum.iter *)
  (* 	       (fun wavelength -> *)
  (* 		let scen = Array.init data.nb_T (fun _ -> Array.init data.nb_B (fun _ -> 0)) in *)
  (* 		let scen = scen |> Array.to_list in *)
  (* 		Printf.printf "%d\n" wavelength; *)
  (* 		routes |> Enum.iter (fun (b,r) -> *)
				     
  (* 				     (0 -- (data.nb_B - 1)) |> *)
  (* 				       Enum.filter (fun i -> i mod 10 = b) *)
  (* 				     |> Enum.iter (fun b -> *)
  (* 						   set_route b *)
  (* 							     (List.map Dir.da ((repeat ((b/10)*wavelength) Dir.Stay) @ r)) *)
  (* 							     scen)); *)

  let scen = strategy1 data w {x;y;a=0} wavelength in
  print_scen scen;
  do_vis data scen;
  simulate data scen
  |> Int.print stdout;
  print_newline ()

  
  (* (\*  *\) *)
  (* print_scen scen; *)
  (* simulate data scen *)
  (* |> Int.print stdout; *)
  (* print_newline (); *)

	       

	       
	       

	       (* Local Variables: *)
	       (* compile-command: "ocamlbuild -use-ocamlfind main.native" *)
	       (* End: *)
