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
