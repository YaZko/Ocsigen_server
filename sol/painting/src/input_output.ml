open Batteries

type tile = Clear | Paint

type data =
    {
      width: int;
      height: int;
      painting: tile array array
    }

let parse file =
  let f = BatScanf.Scanning.from_file file in
  Scanf.bscanf f "%d %d\n"
    (fun n m ->
     let painting = Array.init n 
        (fun _ -> Array.init m 
            (fun k -> if k = m - 1 
		      then Scanf.bscanf f "%c\n" (fun c -> if c = '#' then Paint else Clear)
		      else Scanf.bscanf f "%c"   (fun c -> if c = '#' then Paint else Clear)
	    )
	)
     in
     { width = m;
       height = n;
       painting = painting
     }
    )

type order =
  | Square of (int * int * int)
  | Line of (int * int * int * int)
  | Erase of (int * int)

let dot r c = Square (r,c,0)

type sol = order list 

let print_order oc order =
  match order with
  | Square(r,c,s) -> Printf.fprintf oc "PAINT_SQUARE %d %d %d\n" r c s
  | Line(r1,c1,r2,c2) -> Printf.fprintf oc "PAINT_LINE %d %d %d %d\n" r1 c1 r2 c2
  | Erase(r,c) -> Printf.fprintf oc "ERASE_CELL %d %d\n" r c	

let out_sol file sol =
  let (_, file) = String.split file "inputs/" in  
  let (file, _) = String.split file ".in" in  
  let oc = open_out ("outputs/" ^ file ^ ".out") in
  Printf.fprintf oc "%d\n" (List.length sol);
  sol |>
    List.iter (print_order oc);
  close_out oc
