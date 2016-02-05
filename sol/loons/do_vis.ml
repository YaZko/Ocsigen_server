open Batteries
open Input
open Vis

let do_vis data scen =
  let width = 2000 in
  let height = 600 in
  let scale = min ((float_of_int height) /. (float_of_int data.nb_R))
  		  ((float_of_int width) /. (float_of_int data.nb_C)) in
  let posy r = int_of_float ((float_of_int r) *. scale) in
  let posx c = int_of_float ((float_of_int c) *. scale) in
  vis data scen posx posy

exception ParseError of string
			  
let go data outputfile : int array list =


  let s = outputfile
	  |> File.lines_of
	  |> Enum.reduce (fun a b -> a ^ "\n" ^ b ) in
  let lines = Str.split (Str.regexp "\n") s in 
  let nb_lines = List.length lines in
  let scenario =
    if nb_lines <> data.nb_T 
    then raise (ParseError ("Your solution contains " ^ string_of_int nb_lines ^ " lines where " ^ string_of_int data.nb_T ^ " were expected."))
    else 
      begin
	lines |> List.mapi 
		   (fun i line -> 
		    let numbers = Str.split (Str.regexp "[ \t]+") line in
		    let nb_numbers = List.length numbers in
		    if nb_numbers <> data.nb_B
		    then raise (ParseError ("The line number " ^ string_of_int i ^ " from your solution contains " ^ string_of_int nb_numbers ^ " numbers where " ^ string_of_int data.nb_B ^ " were expected."))
		    else Array.init data.nb_B (fun k -> int_of_string (List.nth numbers k)))	
      end
  in
  scenario

let _ =
  let data = read_data Sys.argv.(1) in
  let scen = go data Sys.argv.(2) in
  do_vis data scen
    

    (* Local Variables: *)
    (* compile-command: "ocamlbuild -use-ocamlfind do_vis.native" *)
    (* End: *)
