open Batteries 

exception ParseError of string

type data = {
  nb_R: int;
  nb_C: int;
  nb_A: int;
  nb_L: int;
  size: int;
  nb_B: int;
  nb_T: int;
  start: int * int;
  targets: (int * int) array;
  wind: (int * int) array array array
}
(* read input *)
let read_data file =
  let f = BatScanf.Scanning.from_file file in
  Scanf.bscanf f "%d %d %d\n" 
    (fun r c a ->
     Scanf.bscanf f "%d %d %d %d\n"
     (fun l v b t ->
      Scanf.bscanf f "%d %d\n"
      (fun x0 y0 ->
       let targets = Array.init l (fun _ -> 
		    Scanf.bscanf f "%d %d\n" (fun x y -> (x,y) ))
       in
       let wind = Array.init a 
		  (fun _ -> 
		   Array.init r (fun _ -> 
				 Array.init c 
				 (fun k ->
				  if k = c - 1 
				  then Scanf.bscanf f "%d %d\n" (fun x y -> (x,y))
				  else Scanf.bscanf f "%d %d " (fun x y -> (x,y)))))
       in
       let d: data = {
	 nb_R = r;
	 nb_C = c;
	 nb_A = a;
	 nb_L = l;
	 size = v;
	 nb_B = b;
	 nb_T = t;
	 start = x0,y0;
	 targets = targets;
	 wind = wind
       }
       in
       d
      )))

let square x = x * x
    
let score_turn data loons alt =
  let size_sq = square data.size in
  let is_covered_by (tx,ty) mb = match mb with
    | Some (bx,by) ->
       let abt = abs (by - ty) in
       square (bx - tx) + square (min abt (data.nb_C - abt)) <= size_sq
    | None -> false
  in
  Array.fold_left (fun acc t -> let cov = Array.fold_lefti (fun acc i b -> acc || (alt.(i) <> 0 && is_covered_by t b)) false loons 
				in if cov then acc + 1 else acc) 
		  0 data.targets

exception IllegalMovement of string
		  
let simulate data scenario =
  let loons = Array.init data.nb_B (fun _ -> Some data.start) in
  let alt   = Array.init data.nb_B (fun _ -> 0) in
  scenario |> List.fold_left 
		(fun acc (moves: int array) ->
		 (* Printf.printf "loons beginning of turn: %s at alt %s\n" (BatPervasives.dump loons) (BatPervasives.dump alt); *)
		 Array.iteri (fun i m -> 
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
			    else raise (IllegalMovement ("Unknown movement :" ^ string_of_int m))) moves; 

		 (* Printf.printf "loons after order: %s at alt %s\n" (dump loons) (dump alt); *)
		 
		 Array.iteri (fun i b -> 
			      (match b with
			       | Some (bx,by) ->
				  let a = alt.(i) in
				  if a = 0 then ()
				  else let (vx,vy) = data.wind.(a - 1).(bx).(by) in
				       let bx' = bx + vx in
				       let by' = 
					 let nby = by + vy in
					 if nby >= 0 then nby mod data.nb_C else data.nb_C + nby
				       in
				       let newb = if bx' >= 0 && bx' < data.nb_R then Some (bx', by') else None 
				       in loons.(i) <- newb
			       | None -> ())
		) loons;
		 (* Printf.printf "loons after moves: %s at alt %s\n\n" (BatPervasives.dump loons) (BatPervasives.dump alt); *)
		 acc + score_turn data loons alt
		) 0 
	  
let scoring s =
  let data = read_data "final_round.in" in
  (* Printf.printf "data: %s\n" (dump data); *)
  let lines = Str.split (Str.regexp "\n") s in 
  (* Printf.printf "Lines: %s\n" (dump lines); *)
  let nb_lines = List.length lines in
  let scenario =
    if nb_lines <> data.nb_T 
    then raise (ParseError ("Your solution contains " ^ string_of_int nb_lines ^ " lines where " ^ string_of_int data.nb_T ^ " were expected."))
    else 
      begin
	lines |> List.mapi 
		   (fun i line -> 
		    let numbers = Str.split (Str.regexp "[ \t]+") line in
		    let nb_numbers = List.length numbers in
		    if nb_numbers <> data.nb_B
		    then raise (ParseError ("The line number " ^ string_of_int i ^ " from your solution contains " ^ string_of_int nb_numbers ^ " numbers where " ^ string_of_int data.nb_B ^ " were expected."))
		    else Array.init data.nb_B (fun k -> int_of_string (List.nth numbers k)))	
      end
  in
  (* Printf.printf "Scenario: %s\n" (BatPervasives.dump scenario); *)
  let res = simulate data scenario in
  res;;

(* let _ = *)
(*   (try Sys.argv.(1) with _ -> "output") *)
(*   |> File.lines_of *)
(*   |> Enum.reduce (fun a b -> a ^ "\n" ^ b ) *)
(*   |> scoring *)
(*   |> Int.print stdout *)
  
  
  
