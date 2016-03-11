open Drawing
open Input_output
open Drawing.Draw

let draw_car x y : unit =
  rect x y 2 6 Blue;
  point (x+2) (y+1) Black;
  point (x+2) (y+4) Black;
  point (x-1) (y+1) Black;
  point (x-1) (y+4) Black

let draw_city city: unit =
  
