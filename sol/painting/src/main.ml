open Batteries
open Input_output
open Painting
open Order

let square = fun x -> x * x

(** We paint tiles one by one **)
let dumb_sol data: sol =
  data.painting |>
    Array.fold_lefti
      (fun acc r a ->
       Array.fold_lefti
	 (fun acc' c t ->
	  match t with
	  | Clear -> acc'
	  | Paint -> dot r c::acc')
	 acc a) []

let cost_to_paint_line_down data r c n is_done =
  let cov = ref TileSet.empty in
  let tot = ref 0 in
  let comm = ref [] in
  for r' = r to r + n do
    match data.painting.(r').(c) with
      | Clear -> comm := Erase(r',c)::!comm 
      | Paint -> 
	 if TileSet.mem (r',c) is_done 
	 then () 
	 else (tot := !tot + 1; cov := TileSet.add (r',c) !cov)
  done;
  (2 * !tot - n - 2, !cov, Line(r,c,r + n,c)::!comm)

let cost_to_paint_line_right data r c n is_done =
  let cov = ref TileSet.empty in
  let tot = ref 0 in
  let comm = ref [] in
  for c' = c to c + n do
    match data.painting.(r).(c') with
      | Clear -> comm := Erase(r,c')::!comm 
      | Paint -> 
	 if TileSet.mem (r,c') is_done 
	 then () 
	 else (tot := !tot + 1; cov := TileSet.add (r,c') !cov)
  done;
  (2 * !tot - n - 2, !cov, Line(r,c,r,c + n)::!comm)

(** Compute the best line and its score to paint (r,c) through a line of size n **)
let best_line_down_cost_n data r c n is_done =
  (* IntPairs.print stdout (r,c); *)
  (* print_newline (); *)
  let cov = ref TileSet.empty in
  let best_score = ref 0 in
  let comm = ref [] in
  for r' = r - n to r do
    if r' < 0 || r' + n >= data.rows
    then ()
    else
      begin
	let (tp, cov',comm') = cost_to_paint_line_down data r' c n is_done in
	if tp > !best_score 
	then (comm := comm'; cov := cov'; best_score := tp)
	else ()
      end
  done;
 (!best_score, !cov, !comm)

let best_line_right_cost_n data r c n is_done =
  (* IntPairs.print stdout (r,c); *)
  (* print_newline (); *)
  let cov = ref TileSet.empty in
  let best_score = ref 0 in
  let comm = ref [] in 
  for c' = c - n to c do
    if c' - n < 0 || c' + n >= data.columns
    then ()
    else
      begin
	let (tp, cov',comm') = cost_to_paint_line_right data r c' n is_done in
	if tp > !best_score 
	then (comm := comm'; cov := cov'; best_score := tp)
	else ()
      end
  done;	
 (!best_score, !cov, !comm)
 
let best_line_down_cost r c is_done data =
  let rec aux best_score n covered comm =
    let (bs,cov,comm') = best_line_right_cost_n data r c (n + 1) is_done
    in 
    if bs > best_score 
    then aux bs (n + 1) cov comm'
    else (best_score,covered, comm)
  in
  aux 0 0 (TileSet.singleton (r,c)) [Line(r,c,r,c)] 

let best_line_right_cost r c is_done data =
  let rec aux best_score n covered comm =
    let (bs,cov,comm') = best_line_right_cost_n data r c (n + 1) is_done
    in 
    if bs > best_score 
    then aux bs (n + 1) cov comm'
    else (best_score,covered, comm)
  in
  aux 0 0 (TileSet.singleton (r,c)) [Line(r,c,r,c)] 

(** Compute the cost of a square of size n centered in (r,c) in the sense that we need to erase all Clear tiles in it **)
let cost_to_paint_square data r c n is_done =
  let cov = ref TileSet.empty in
  let tot = ref 0 in
  let comm = ref [] in
  for r' = r - n to r + n do
    for c' = c - n to c + n do
      match data.painting.(r').(c') with
      | Clear -> comm := Erase(r',c')::!comm 
      | Paint -> 
	 if TileSet.mem (r',c') is_done 
	 then () 
	 else (tot := !tot + 1; cov := TileSet.add (r',c') !cov)
    done
  done;
  (2 * !tot - 1 - (square (2 * n + 1)), !cov, Square(r,c,n)::!comm)

(** Compute the best center and its score to paint (r,c) through a square of size n **)
let best_square_cost_n data r c n is_done =
  (* IntPairs.print stdout (r,c); *)
  (* print_newline (); *)
  let cov = ref TileSet.empty in
  let best_score = ref 0 in
  let comm = ref [] in
  for r' = r - n to r + n do
    if r' - n < 0 || r' + n >= data.rows
    then ()
    else
      begin
	for c' = c - n to c + n do
	  if c' - n < 0 || c' + n >= data.columns
	  then ()
	  else
	    begin
	      let (tp, cov',comm') = cost_to_paint_square data r' c' n is_done in
	      if tp > !best_score 
	      then (comm := comm'; cov := cov'; best_score := tp)
	      else ()
	    end
	done
      end
  done;
  (* Printf.printf "%d\n" !best_score; *)
  (!best_score, !cov, !comm)

(** Given (r,c) to be painting as a square, compute a good size for this square.
 It starts from a score of 0 for a square of size 1 and see if there is a square of size 2 with a better square, keeps going until the monotony breaks **)    
let best_square_cost r c is_done data =
  let rec aux best_score n covered comm =
    let (bs,cov,comm') = best_square_cost_n data r c (n + 1) is_done
    in 
    if bs > best_score 
    then aux bs (n + 1) cov comm'
    else (best_score,covered, comm)
  in
  aux 0 0 (TileSet.singleton (r,c)) [Square(r,c,0)] 

let better_sol data: sol =
  let rec aux data todo is_done sol =
    (* TileSet.print IntPairs.print stdout todo; *)
    (* print_newline (); *)
    (* List.print Order.print stdout sol; *)
    (* print_newline (); *)
    if TileSet.is_empty todo
    then sol
    else
      begin
	let (r,c) = TileSet.choose todo in
	let (bs,painted,comm) = best_square_cost r c is_done data in
	aux data (TileSet.diff todo painted) (TileSet.union is_done painted) (comm @ sol)
      end
  in
  aux data data.todo TileSet.empty []

let better_sol2 data: sol =
  let rec aux data todo is_done sol =
    (* TileSet.print IntPairs.print stdout todo; *)
    (* print_newline (); *)
    (* List.print Order.print stdout sol; *)
    (* print_newline (); *)
    if TileSet.is_empty todo
    then sol
    else
      begin
	let (r,c) = TileSet.choose todo in
	let (bs_sq,painted_sq,comm_sq) = best_square_cost r c is_done data in
	let (bs_ld,painted_ld,comm_ld) = best_line_down_cost r c is_done data in
	let (bs_lr,painted_lr,comm_lr) = best_line_right_cost r c is_done data in	
	(* Printf.printf "%d %d %d\n" bs_sq bs_ld bs_lr; *)
	let (bs, painted, comm) =
	  if bs_ld > bs_sq && bs_ld >= bs_lr 
	  then (bs_ld, painted_ld, comm_ld)
	  else if bs_lr > bs_sq && bs_lr >= bs_ld
	  then (bs_lr, painted_lr, comm_lr)
	  else (bs_sq, painted_sq, comm_sq)
	in 
	aux data (TileSet.diff todo painted) (TileSet.union is_done painted) (comm @ sol)
      end
  in
  aux data data.todo TileSet.empty []

let _ =
  let file =
    (try Sys.argv.(1) with _ -> "input") in
  let d = parse file
  in
  (* TileSet.print IntPairs.print stdout d.todo; *)
  paint_data d;
  let _ = Graphics.wait_next_event [Graphics.Button_down] in
  let sol = better_sol2 d in
  paint_sol d sol;
  let _ =  Graphics.wait_next_event [Graphics.Button_down] in
  out_sol file sol
