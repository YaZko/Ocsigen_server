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
