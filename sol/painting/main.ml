open Batteries

module type DRAW = sig

  type color =
     | Empty
     | Painted

  val init : int -> int -> unit

  val redraw : unit -> unit
                       
  val point : int -> int -> color -> unit

  (* val text : string -> unit *)
                                       
  val close : unit -> unit
                                       
end

module Draw : DRAW = struct
  open Graphics

  type color =
     | Empty
     | Painted

  let translate_color (c:color) : Graphics.color =
    match c with
     | Empty -> rgb 255 255 255
     | Painted -> rgb 0 200 100
                
  let scalex = ref 1
  let scaley = ref 1
  (* let text_height = 30 *)
  let height = ref 0
  let resh = 1200
  let resv = 600
  let close = close_graph

  (* let text s = *)
  (*   set_color white; *)
  (*   fill_rect 0 0 300 (text_height-2); *)
  (*   set_color black; *)
  (*   moveto 10 10; *)
  (*   draw_string s *)
                
  let init (r:int) (c:int) : unit = 
    let _ = open_graph "" in
    scalex := resh / c;
    scaley := resv / r;
    let width = !scalex * c in
    height := !scaley * r;
    Graphics.resize_window (width) (!height(* +text_height *));
    (* fill_rect 0 text_height width !height; *)
    auto_synchronize false
    
  let redraw () : unit =
    synchronize ()

  let point (r:int) (c:int) (col:color) : unit =
    set_color (translate_color col);
    fill_rect (!scalex*c) (!height - !scaley*r (* + text_height *)) !scalex !scaley 

end


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

let paint_data data =
  Draw.init data.height data.width;
  Array.iteri (fun i a -> Array.iteri (fun j t -> match t with | Clear -> () | Paint -> Draw.point i j Draw.Painted) a)
    data.painting;
  Draw.redraw ()

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
  let oc = open_out ("output_" ^ file) in
  Printf.fprintf oc "%d\n" (List.length sol);
  sol |>
    List.iter (print_order oc);
  close_out oc

let paint_order order =
  match order with
  | Square(r,c,s) -> if s = 0 then Draw.point r c Draw.Painted else failwith "TODO"
  | Line(r1,c1,r2,c2) -> 
     if r1 = r2 
     then for k = c1 to c2 do
	    Draw.point r1 k Draw.Painted
	  done
     else if c1 = c2
     then for k = r1 to r2 do
	    Draw.point k c1 Draw.Painted
	  done
     else failwith "Illegal line drawing"
  | Erase(r,c) -> Draw.point r c Draw.Empty

let paint_sol data sol =
  Draw.init data.height data.width;
  List.iter paint_order sol;
  Draw.redraw ()

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
