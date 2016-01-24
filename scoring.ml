(***** Ranking section *****)

(** The type describing an instance of the problem **)
type data = {
  cols: int;
  rows : int;
  groups: int;
  is_undisp : bool array array;
  size : int array;
  capa : int array;
  ratio: (int * float) array;
}

(** Parse a problem and format it into a data **)
let read_data (stdin: in_channel) : data =
  (* Nombre de rangées *)
  (* Nombre d’emplacements *)
  (* Nombre d’emplacements indisponibles *)
  (* Nombre de groupes *)
  (* Nombre de serveurs *)
  Scanf.fscanf stdin "%d %d %d %d %d\n" (fun rows cols undisp groups servers ->
  (* Tableau des cases indisponibles idx -> row × col *)
    let undisp = Array.init undisp (fun _ ->
      Scanf.fscanf stdin "%d %d\n" (fun ri ci -> (ri, ci))
    ) in
  (* Tableau des serveurs à allouer idx -> size × capa *)
    let data = Array.init servers (fun _ ->
      Scanf.fscanf stdin "%d %d\n" (fun zi ci -> (zi, ci))
    ) in
  (** Tableau des cases indisponibles *)
    let is_undisp = Array.init rows (fun r ->
      Array.init cols (fun c -> false
      )) in
    Array.iter (fun (r, c) -> is_undisp.(r).(c) <- true) undisp;

  (** Ratio capa/taille par serveur *)
    let ratio = Array.init servers (fun s ->
      let (z,c) = data.(s) in
      (s, float_of_int c /. float_of_int z)
    ) in

    Array.sort (fun (s1, r1) (s2, r2) -> compare (r2, - fst data.(s2)) (r1, - fst data.(s1))) ratio;    
    let d : data = {
      cols = cols;
      rows = rows;
      groups = groups;
      ratio = ratio;
      is_undisp = is_undisp;
      size = Array.map fst data;
      capa = Array.map snd data;
    } in
    d
  )

(** A few auxilliary function to rank solutions **)
let rec foldmin n f: int =
  if n = 0 then f 0
  else min (f n) (foldmin (n - 1) f)

let rec foldsum n f: int =
  if n = 0 then f 0
  else (f n) + (foldsum (n - 1) f)    

let hurt_capa l gr d m i g : int =
  if (l.(m) = i
     || gr.(m) <> g) then 0
  else d.capa.(m)

(** guar_capa : granted capacity for group g for current allocation (l,gr) **)
let guar_capa l gr d g : int =
  let servers = Array.length d.size in
  foldmin (d.rows -  1)
    (fun i -> (foldsum (servers - 1)
		 (fun m -> hurt_capa l gr d m i g)))


(** The main function to grade a solution. 
Takes as an input the definition of the problem d and the description of the solution as an assignment from each server to its pool (gr), slot (c) and row (l) 
TODO : I think we assumed the solution to be legal, to double check and if so add a filter.
**)
let score_solution (d: data) gr c l : int =
  foldmin (d.groups - 1) (guar_capa l gr d)

exception ParseError of string
	  
(** First parsing of a solution into a list **)
let parse_output s =
  let f = Str.split (Str.regexp "\n") s in 
  let rec aux f l ln = 
    match f with
    | s::tl ->
       (try 
	Scanf.sscanf s "%d %d %d" 
		     (fun a b c -> aux tl ([a;b;c]::l) (ln + 1))
       with
	 | Scanf.Scan_failure _ | End_of_file ->
	    try 
	      Scanf.sscanf s "x" (aux tl ([-1]::l) (ln + 1))
	    with
	    | Scanf.Scan_failure _ | End_of_file ->
	       raise (ParseError
		 (Printf.sprintf
		    "Expected 'x' or three numbers; got '%s' at line %d"
		    s
		    ln))
       )
    | [] -> l
  in
  List.rev (aux f [] 1)
;;


(** Printing of a well formed solution, only for debugging purpose on our side **)
(* let out_solution (oc: out_channel) (group: int array) (ligne: int array) (column: int array) : unit = *)
(*   Array.iteri (fun s g -> *)
(*     if g < 0 *)
(*     then Printf.fprintf oc "x\n" *)
(*     else Printf.fprintf oc "%d %d %d\n" ligne.(s) column.(s) g *)
(*   ) group;; *)

(** The problem under consideration **)
let d : data = read_data (Pervasives.open_in "dc.in") 

(** Main function to grade a submitted solution **)
let stuff sol =
  let out = parse_output sol in
  let ngr = List.length out in
  let gr = Array.init ngr (fun _ -> -1) in
  let line = Array.init ngr (fun _ -> -1) in
  let col = Array.init ngr (fun _ -> -1) in
  List.iteri (fun i x ->
	      match x with 
	      | [_] -> ()
	      | [l;c;g] -> line.(i) <- l; col.(i) <- c; gr.(i) <- g
	      | _ -> failwith "A parsed input should be a list containing only triplets or single element, this is weird.") out;
  score_solution d gr col line		 

