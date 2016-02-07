open Batteries

module IntPairs =

  struct

    type t = int * int

    let print oc (a,b) =
      String.print oc (Printf.sprintf "(%d;%d)" a b)

    let compare (x0,y0) (x1,y1) =

      match Pervasives.compare x0 x1 with

	0 -> Pervasives.compare y0 y1

      | c -> c

  end

module TileSet = Set.Make(IntPairs)

type tile = Clear | Paint

type data =
    {
      rows: int;
      columns: int;
      painting: tile array array;
      todo: TileSet.t
    }

let parse file =
  let f = BatScanf.Scanning.from_file file in
  let todo = ref TileSet.empty in
  Scanf.bscanf f "%d %d\n"
    (fun n m ->
     let painting = Array.init n 
        (fun r -> Array.init m 
            (fun c -> if c = m - 1 
		      then Scanf.bscanf f "%c\n" (fun t -> if t = '#' then (todo := TileSet.add (r,c) !todo; Paint) else Clear)
		      else Scanf.bscanf f "%c"   (fun t -> if t = '#' then (todo := TileSet.add (r,c) !todo; Paint) else Clear)
	    )
	)
     in
     { rows = n;
       columns = m;
       painting = painting;
       todo = !todo
     }
    )

module Order =
struct
  type order =
    | Square of (int * int * int)
    | Line of (int * int * int * int)
    | Erase of (int * int)

  let print oc o = 
    match o with
    | Square (a,b,c) -> String.print oc (Printf.sprintf "Square(%d;%d;%d)" a b c) 
    | Line (a,b,c,d) -> String.print oc (Printf.sprintf "Line(%d;%d;%d;%d)" a b c d) 
    | Erase (a,b) -> String.print oc (Printf.sprintf "Erase(%d;%d)" a b) 
		 
end

open Order 

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
