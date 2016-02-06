open Drawing
open Input_output

let paint_data data =
  Draw.init data.height data.width;
  Array.iteri (fun i a -> Array.iteri (fun j t -> match t with | Clear -> () | Paint -> Draw.point i j Draw.Painted) a)
    data.painting;
  Draw.redraw ()

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
