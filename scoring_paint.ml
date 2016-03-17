open Batteries 
open Exn

type data = {
  nb_R: int;
  nb_C: int;
  objective: bool array array
}
	      
(* read input *)
let read_data file =
  let f = BatScanf.Scanning.from_file file in
  Scanf.bscanf
    f "%d %d\n" 
    (fun r c ->
     let obj =
       Array.init
	 r
	 (fun _ -> 
	  Array.init
	    c
	    (fun k ->
	     let treat_char x =
	       if x = '.'
	       then false
	       else if x = '#'
	       then true
	       else failwith (Printf.sprintf
				"Illegal character '%c' at k=%d, c=%d" x k c)
	     in
	     if k = c - 1
	     then Scanf.bscanf f "%c\n" treat_char
	     else Scanf.bscanf f "%c" treat_char
	    ))
     in
     let d: data = {
       nb_R = r;
       nb_C = c;
       objective = obj
     }
     in
     d
    )

type cmd =
    PSQ of int * int * int
  | PL of int * int * int * int
  | EC of int * int

exception ParseError of string
		  
let parse_cmd f =
  Scanf.bscanf f "%s@\n"
	       (fun instr ->
		let words = Str.split (Str.regexp " ") instr in 
		let instr = List.nth words 0 in
		let args = words |> List.tl
			   |> List.map int_of_string in
		match instr with
		| "PAINT_SQUARE" ->
		  begin
		    match args with
		      [r;c;s] -> PSQ (r,c,s)
		    | _ -> failwith "PAINT_SQUARE incomplete"
		  end
		| "PAINT_LINE" ->
		   begin
		     match args with
		       [r;c;r';c'] -> PL (r,c,r',c')
		     | _ -> failwith "PAINT_LINE incomplete"
		   end
		| "ERASE_CELL" ->
		   begin
		     match args with
		       [r;c] -> EC (r,c)
		     | _ -> failwith "ERASE_CELL incomplete"
		   end
		| s -> raise (ParseError ("unexpected command "^s))
	       )
  
		  
type sol = {
  nb_L: int;
  cmds : cmd array
}

(* read output *)
let read_output s =
  let f = BatScanf.Scanning.from_string s in
  Scanf.bscanf f "%d\n" 
	       (fun nb_L ->
		let cmds = Array.init nb_L (fun _ -> parse_cmd f)
		in
		let sol: sol = { nb_L ; cmds } in
		sol
	       )

let num_cells data =
  data.objective |>
    Array.fold_left
      (fun acc line -> line |>
			 Array.fold_left
			   (fun acc b -> acc + if b then 1 else 0) acc
      ) 0
	       
let score data sol =
  let nc = num_cells data in
  let snl = sol.nb_L in
  nc - snl
  
module M : Problem.Problem =
  struct
    type input = data
    type solution = sol 
    let parse_input = read_data
    let parse_output = fun _ -> read_output
    let score = score
  end
				      
(* let _ = *)
(*   let data = read_data "paint_input" in *)
(*   let sol = read_output "paint_output" in *)
(*   score data sol |> Int.print stdout *)
