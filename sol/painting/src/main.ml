open Batteries
open Input_output
open Painting
 
(** Compute the cost of a square of size n centered in (r,c) in the sense that we need to erase all Clear tiles in it **)
let cost_to_paint_square data r c n =
  let tot = ref 0 in
  for r' = r to r + n - 1 do
    for c' = c to c + n - 1 do
      match data.painting.(r').(c') with
      | Clear -> () 
      | Paint -> tot := !tot + 1
    done
  done;
  2 * !tot - 1 - (n * n)

(** Compute the best corner and its score to paint (r,c) through a square of size n **)
let best_square_cost_n data r c n = 
  let best_score = ref 0 in
  let best_corner = ref (r, c) in
  for r' = r downto r - n + 1 do
    if r' < 0 
    then ()
      else
	begin
	  for c' = c downto c - n + 1 do
	    if c' < 0
	    then ()
	    else
	    begin
	      let tp = cost_to_paint_square data r' c' n in
	      if tp > !best_score 
	      then (best_score := tp; best_corner := (r',c'))
	      else ()
	    end
	  done
	end
  done;
  (!best_score, !best_corner)

(** Given (r,c) to be painting as a square, compute a good size for this square.
 It starts from a score of 0 for a square of size 1 and see if there is a square of size 2 with a better square, keeps going until the monotony breaks **)    
let best_square_cost r c data =
  let rec aux best_score best_corner n =
    let (bs,bc) = best_square_cost_n data r c (n + 1) 
    in 
    if bs > best_score 
    then aux bs bc (n + 1)
    else (bc, n)
  in
  aux 0 (r,c) 1      

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

let _ =
  let file =
    (try Sys.argv.(1) with _ -> "input") in
  let d = parse file
  in
  paint_data d;
  let _ = Graphics.wait_next_event [Graphics.Button_down] in
  let sol = dumb_sol d in
  paint_sol d sol;
  let _ =  Graphics.wait_next_event [Graphics.Button_down] in
  out_sol file sol
