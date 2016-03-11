module type DRAW = sig

  type color =
     | Black
     | Blue

  val init : int -> int -> unit

  val redraw : unit -> unit
                       
  val point : int -> int -> color -> unit

  val rect : int -> int -> int -> int -> color -> unit
  (* val text : string -> unit *)
                                       
  val close : unit -> unit
                                       
end

module Draw : DRAW = struct
  open Graphics
         
  type color =
     | Black
     | Blue

  let translate_color (c:color) : Graphics.color =
    match c with
     | Black -> rgb 0 0 0
     | Blue -> rgb 30 144 255
                
  let scalex = ref 1
  let scaley = ref 1
  (* let text_height = 30 *)
  let height = ref 0
  let resh = 600
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

  let point (x:int) (y:int) (col:color) : unit =
    set_color (translate_color col);
    fill_rect (!scalex*x) (!height - !scaley*y (* + text_height *)) !scalex !scaley 

  let rect x y width height (col: color): unit =
     for x = x to x + width - 1 do
       for y = y to y + height - 1 do
	 point x y col
       done
     done
             
end

                       
       
