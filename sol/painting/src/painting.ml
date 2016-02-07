open Drawing
open Input_output
open Order

let paint_data data =
  Draw.init data.rows data.columns;
  Array.iteri (fun i a -> Array.iteri (fun j t -> match t with | Clear -> () | Paint -> Draw.point i j Draw.Painted) a)
    data.painting;
  Draw.redraw ()

let paint_order order =
  match order with
  | Square(r,c,s) -> 
     for r = r - s to r + s do
       for c = c - s to c + s do
	 Draw.point r c Draw.Painted
       done
     done
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
  Draw.init data.rows data.columns;
  List.iter paint_order sol;
  Draw.redraw ()
