open Batteries
open Input
open Vis
       
exception IllegalMovement of string

let square x = x * x

let is_covered_by data (tr,tc) mb =
  let size_sq = square data.size in
  match mb with
  | Some (br,bc) ->
     let abt = abs (bc - tc) in
     square (br - tr) + square (min abt (data.nb_C - abt)) <= size_sq
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
  | Some (br,bc) ->
     let a = alt.(i) in
     if a = 0
     then () 			(* wind doesn't affect balloons still on ground *)
     else
       let (vr,vc) = data.wind.(a - 1).(br).(bc) in
       let br' = br + vr in
       let bc' = (bc + vc + data.nb_C) mod data.nb_C in
       (if bc' < 0 then raise (IllegalMovement "move_wind: illegal diminution of altitude") else ());
       loons.(i) <- (if br' >= 0 && br' < data.nb_R then Some (br', bc') else None)
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


type node = { r : int;
	      c : int;
	      a : int; }
	      
module Node =
  struct
    let print oc {r;c;a} =
      String.print oc (Printf.sprintf "{r=%d;c=%d;a=%d}" r c a)

    let get_coord {r;c} = (r,c)

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

(** Given a starting node and how far we are willing to search for the next target, compute a loose connex component of targets around this node **)
let get_one_connex graph starting_node targets looseness: (int * int) list =
  let rec aux (node: node) (acc: (int * int) list) (visited: node list) dist: (int * int) list * node list =
    if dist = 0 then (acc, visited) 
    else List.fold_left 
	   (fun (acct, accv) (_, n) -> 
	    if List.mem n accv 
	    then (acct, accv)
	    else if Array.mem (Node.get_coord n) targets 
	    then 
	      aux n (Node.get_coord n::acct) (n::accv) looseness
	    else
	      aux n acct (n::accv) (dist - 1) 
	   )
	   (acc, visited) (Hashtbl.find graph node)
  in
  let (ts, _) = aux starting_node [Node.get_coord starting_node] [starting_node] looseness
  in
  ts

(* let get_connex graph targets looseness: node list list = *)
(*   let rec aux todo acc1 acc2 =  *)
(*     match todo with *)
(*     | [] -> acc1::acc2 *)
(*     | t::todo' ->   *)


let wmap data : graph =
  let g : graph = Hashtbl.create 17 in 
  for c = 0 to data.nb_C - 1 do
    for r = 0 to data.nb_R - 1 do
      for a = 0 to data.nb_A do
	let n = {r;c;a} in
	Hashtbl.replace
	  g n
	  ([Dir.Up; Dir.Down; Dir.Stay]
	   |> List.filter_map
		(fun d ->
		 if a = 0 && d = Dir.Stay
		 then Some (d,n)
		 else let {r;c;a} = move n d in
		      if (a > 0 && a <= data.nb_A) then
			try let (dr,dc) = data.wind.(a-1).(r).(c) in
			    let c = (c + dc + data.nb_C) mod (data.nb_C) in
			    let r = r + dr in
			    if r < 0 || r >= data.nb_R
			    then
			      (
				if n.r = 69 && n.c=43 && n.a = 5
				then
				  Printf.printf
				     "Throwing neighbour %d (r=%d;c=%d;a=%d) because r is out\n" (Dir.da d) r c a;
				None)
			    else Some (d, {r; c; a})
			with
			  Invalid_argument s -> failwith (Printf.sprintf "invalid access '%s' a=%d, r=%d, c=%d" s a r c)
		      else
			(
			  if n.r = 69 && n.c=43 && n.a = 5
				then
				  Printf.printf
				     "Throwing neighbour %d (r=%d;c=%d;a=%d) because a is out\n" (Dir.da d) r c a;
			None)
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
					     (Some (node.r, node.c)))
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
			       || (node.a <> 0 && is_covered_by data t (Some (node.r,node.c)))
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
    loons.(turn).(b) <- Some (bestnode.r, bestnode.c);
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

(* paths data w from n : the list of all routes of length n from node 'from'
 * a route is a list of direction / current node
 *)
let rec paths data w from n : ((node * Dir.t * node) list) list =
  if n <= 0
  then [[]]
  else
    match safe_find w from with
      None ->
      Printf.printf "Couldn't find node {r=%d;c=%d; a=%d}\n" from.r from.c from.a;
      [[]]
    | Some nexts ->
       (* begin *)
       (* 	 if nexts = [] *)
       (* 	 then Printf.printf "Node {r=%d;c=%d; a=%d} has empty successors\n" *)
       (* 			    from.r from.c from.a; *)
       (* end; *)
       nexts |>
	 List.map
	   (fun (dir, node) ->
	    let pn1 = paths data w node (n-1) in
	    pn1 |>
	      List.map
		(fun path ->
		 (from,dir,node)::path))
       |> List.concat
    
let has_no_successors w node =
  match safe_find w node with
    None -> true
  | Some [] -> true
  | _ -> false

let score_path data w turn pos path (positions: node option array array) =
  (* Printf.printf "Targets are: "; *)
  (* data.targets |> Array.iter (fun (r,c) -> Printf.printf "{r=%d;c=%d} " r c); *)
  (* Printf.printf "\nRadius is %d\n" data.size; *)
  let (_,_,last) = List.last path in
  if has_no_successors w last then -1
  else 
  let not_covered turn target =
    (0 -- (data.nb_B-1))
    |> Enum.map (fun b ->
		 let pt =
		   try
		     (positions.(turn).(b) |> Option.map Node.get_coord)
		   with _ ->
		     failwith
		       (Printf.sprintf "positions.(%d).(%d) error\n" turn b)
		 in
		 not (is_covered_by data target pt))
    |> Enum.reduce ( && )
  in
  
  let turn = ref turn in
  path |>
    List.fold_left
      (fun acc (_,_,node) ->
       let sc = (if node.a = 0
	then 0
	else data.targets |>
	       Array.fold_left
		 (fun acc target ->
		  if is_covered_by data target (Some (Node.get_coord node))
		     && (if !turn < data.nb_T then
			   not_covered !turn target
			 else false)
		  then acc + 1
		  else acc) 0) in
       turn := ! turn + 1;
       sc + acc
      ) 0
  

      
let search_route data w b (positions: node option array array) directions =
  let turn = ref 0 in
  let k = 6 in
  try
    while ! turn < data.nb_T do
    match try positions.(!turn).(b) with
	    _ ->
	    failwith
	      (Printf.sprintf "positions.(%d).(%d) error" !turn b)
    with
      None -> failwith
		(Printf.sprintf
		   "Should be a defined position positions.(%d).(%d)" !turn b
		)
    | Some pos ->
       let p =
	 try paths data w pos k
	 with _ ->
	   failwith (Printf.sprintf "Error while paths turn = %d\n\n" (!turn))
       in
       begin
	 if p = []
	 then
	   (for t = !turn to data.nb_T - 1 do
	     directions.(t).(b) <- Some Dir.Stay
	   done;
	    (Printf.printf "Aborting on balloon %d: paths {r=%d;c=%d;a=%d} returned []\n"
			   b pos.r pos.c pos.a);
	    raise Exit)
       end;
       let (bscore,bpath) = 
	 p |>
	   List.map
	     (fun path ->
	      let sc = score_path data w !turn pos path positions in
	      (* Printf.printf "score = %d\n" sc; *)
	      (sc,path)
	     )
	 |> List.reduce (fun a b -> if fst a > fst b then a else b)
       in
       begin
	 if List.length bpath != k
	 then Printf.printf "Achtung, bpath is only %d long\n" (List.length bpath)
       end;
       bpath |>
	 List.iteri (fun i (_,dir,node) ->
		     if !turn + i < data.nb_T
		     then directions.(!turn + i).(b) <- Some dir;
		     if !turn + i + 1 < data.nb_T
		     then positions.(!turn + i + 1).(b) <- Some node
		     else (Printf.printf "Skipping turn %d for balloon %d\n"
					 (!turn + i + 1) b;
			   flush stdout)
		    );
       turn := !turn + k
    done
  with Exit -> (Printf.printf "Premature exit for balloon %d\n" b)
      
let strategy3 data w =
  let (r,c) = data.start in
  let init = {r;c;a=0} in
  let positions =
    Array.init data.nb_T
  	       (fun t ->
  		Array.init data.nb_B
  			   (fun b ->
  			    (* position of ballon b at turn t *)
  			    if t = 0 then Some init else None)) in
  let directions =
    Array.init data.nb_T
  	       (fun t ->
  		Array.init data.nb_B
  			   (fun b ->
  			    (* direction of ballon b at turn t *)
  			    None)) in
  for b = 0 to (data.nb_B - 1)do
    Printf.printf "Ballon %d..." b;
    flush stdout;
    (* try *) search_route data w b positions directions;
    Printf.printf "Done\n";
    flush stdout;
    (* with _ -> failwith ("error for balloon "^(string_of_int b)) *)
  done;
  let d = directions |> Array.to_list
	  |> List.map (fun oda ->
		       oda |> Array.map (fun od ->
					 match od with
					   None -> 0
					 | Some d -> Dir.da d)) in
  d

  (* let p = paths data w init 5 in *)
  (* p |> *)
  (*     List.iter *)
  (* 	 (fun path -> *)

  (* 	  path |> List.iter (fun (_,dir,_) -> *)
  (* 			     Printf.printf "%d " (Dir.da dir)); *)
  (* 	  (\* print_newline (); *\) *)
  (* 	  (\* Printf.printf "Score is:\n"; *\) *)
  (* 	  path |> score_path data init |> Int.print stdout; *)
  (* 	  print_newline (); *)
	  
  (* 	 ) *)
  
			  
  
      
let () = 
  let data = read_data (try Sys.argv.(1) with _ -> "ex.in") in
  let w = wmap data in
  (* let (y,x) = data.start in *)

  let scen = strategy3 data w in
  print_scen scen;
  (* do_vis data scen; *)
  simulate data scen
  |> Int.print stdout
  
  (* let wavelength = try Sys.argv.(2) |> int_of_string with _ -> 1 in *)

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

  (* let scen = strategy1 data w {x;y;a=0} wavelength in *)
  (* print_scen scen; *)
  (* do_vis data scen; *)
  (* simulate data scen *)
  (* |> Int.print stdout; *)
  (* print_newline () *)

  
  (* (\*  *\) *)
  (* print_scen scen; *)
  (* simulate data scen *)
  (* |> Int.print stdout; *)
  (* print_newline (); *)

	       

	       
	       

	       (* Local Variables: *)
	       (* compile-command: "ocamlbuild -use-ocamlfind main.native" *)
	       (* End: *)
