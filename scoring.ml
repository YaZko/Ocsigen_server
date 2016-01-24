exception ParseError of string
	  
let scoring s =
  let lines = Str.split (Str.regexp "\n") s in 
  let nb_lines = List.length lines in
  if nb_lines <> 400 
  then raise (ParseError ("Your solution contains " ^ string_of_int nb_lines ^ " lines where 400 were expected."))
  else 
    lines |> List.iteri 
	       (fun i line -> 
		let numbers = Str.split (Str.regexp "[ \t]+") line in
		let nb_numbers = List.length numbers in
		if nb_numbers <> 53
		then raise (ParseError ("The line number " ^ string_of_int i ^ " from your solution contains " ^ string_of_int nb_numbers ^ " numbers where 53 were expected."))
		else ());
  0
