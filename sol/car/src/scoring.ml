open Batteries
open Input_output
       
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

module StreetSet = Set.Make(IntPairs)

exception BadOutput of string
                         
let score_output file (d: data): int =
  let f = BatScanf.Scanning.from_file file in
  let visited = ref StreetSet.empty in
  let score = ref 0 in
  let time = ref 0 in
  Scanf.bscanf f "%d\n"
               (fun nb_cars ->
                 if nb_cars <> d.nb_cars
                 then raise (BadOutput "Wrong number of cars")
                 else
                   for i = 0 to nb_cars - 1 do (** For each car **)
                     Scanf.bscanf f "%d\n"
                                  (fun n ->    (** Car i visits n nodes **)
                                    time := 0;
                                    Scanf.bscanf f "%d\n"
                                                 (fun start -> 
                                                   if start <> d.starting_node
                                                   then raise (BadOutput ("Wrong start for car " ^ string_of_int i))
                                                   else
                                                     let node1 = ref start in
                                                     for j = 1 to n - 1 do
                                                       Scanf.bscanf f ("%d\n")
                                                                    (fun node2 ->
                                                                      match get_vert d.city !node1 node2 with
                                                                      | Some (t,dist) -> time := !time + t;
                                                                                         if !time > d.time
                                                                                         then raise (BadOutput ("Too long tour for car number " ^ string_of_int i))
                                                                                         else
                                                                                           if StreetSet.mem (!node1, node2) !visited
                                                                                           then ()
                                                                                           else
                                                                                             begin
                                                                                               score := !score + dist;
                                                                                               visited := StreetSet.add (!node1,node2) (StreetSet.add (node2,!node1) !visited)
                                                                                             end;
                                                                                         node1 := node2                                                                                            
                                                                      | None -> raise (BadOutput ("No street between nodes " ^ string_of_int !node1 ^ " and " ^ string_of_int node2))
                                                                    )
                                                     done
                                                 )
                                  )
                   done
               );
  !score
