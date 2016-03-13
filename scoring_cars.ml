open Batteries 

type data_raw =
    {
      nb_cars: int;
      time: int;

      starting_node: int;
      nb_nodes: int;
      nb_vertices: int;

      nodes_add: (float * float) array; (* Pure trivia? *)
      streets: (int * int * bool * int * int) array      
    }
     
let parse file: data_raw =
  let f = BatScanf.Scanning.from_file file in
  Scanf.bscanf f "%d %d %d %d %d\n"
    (fun n m t c s ->
      let nodes =
        Array.init n 
                   (fun _ -> Scanf.bscanf f "%f %f\n" (fun lat long -> (lat, long))) in
      let streets =
        Array.init m
                   (fun _ -> Scanf.bscanf f "%d %d %d %d %d\n"
                                          (fun a b d time_cost length ->
                                            let d' = if d=1 then false else true in (a,b,d',time_cost,length)))
      in
      {
        nb_cars = c;
        time = t;
        starting_node = s;
        nb_nodes = n;
        nb_vertices = m;
        nodes_add = nodes;
        streets = streets                     
      }
    )

          
type graph =
  {
    nb_nodes: int;
    nb_vertices: int;
    graph: ((int * int * int) list) array (* Node, time, length *)       
  }
    
let get_vert (g: graph) n1 n2: (int * int) option =
  try
    let (_,t,d) = List.find (fun (x,t,d) -> x = n2) g.graph.(n1)
    in Some (t,d)
  with
  | Not_found -> None
      
type data =
  {
    nb_cars: int;
    time: int;
    starting_node: int;
    city: graph
  }
 
let make_data (d: data_raw): data =
  let graph =
    Array.make d.nb_nodes [] in
  d.streets |>
    Array.iter
    (fun (node1,node2,b,time,dist) ->
      graph.(node1) <- (node2,time,dist)::graph.(node1);
      if b then 
        graph.(node2) <- (node1,time,dist)::graph.(node2);
    );
  {
    nb_cars = d.nb_cars;
    time = d.time;
    starting_node = d.starting_node;
    city = { nb_nodes = d.nb_nodes;
             nb_vertices = d.nb_vertices;
             graph = graph
           }
  }
  
type sol =
  {
    nb_cars: int;
    itinaries: (int list) array
  }

let out_sol file sol =
  let (_, file) = String.split file "inputs/" in  
  let (file, _) = String.split file ".in" in  
  let oc = open_out ("outputs/" ^ file ^ ".out") in
  Printf.fprintf oc "%d\n" sol.nb_cars;
  sol.itinaries |>
    Array.iter
      (fun l ->
        Printf.fprintf oc "%d\n" (List.length l);
        List.iter (fun n -> Printf.fprintf oc "%d\n" n) l);
  close_out oc

exception BadOutput of string

let parse_output (d: data) file: sol = 
  let f = BatScanf.Scanning.from_file file in
  let itinaries = Array.make d.nb_cars [] in
  let time = ref 0 in
  Scanf.bscanf f "%d\n"
    (fun nb_cars ->
       if nb_cars <> d.nb_cars
       then raise (BadOutput "Wrong number of cars")
       else
         for i = 0 to nb_cars - 1 do (** For each car **)
           time := 0;
           Scanf.bscanf f "%d\n"
             (fun n ->    (** Car i visits n nodes **)
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
                              | Some (t,dist) ->
                                time := !time + t;
                                if !time > d.time
                                then raise (BadOutput ("Too long tour for car number " ^ string_of_int i))
                                else
                                  begin
                                    itinaries.(i) <- node2::itinaries.(i);
                                    node1 := node2
                                  end                                 
                              | None -> raise (BadOutput ("No street between nodes " ^ string_of_int !node1 ^ " and " ^ string_of_int node2))
                           )
                       done
                  )
             )
         done
    );
  {nb_cars = d.nb_cars ; itinaries = itinaries }
  
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

exception BadSol of string

let score (d: data) (s: sol): int =
  let visited = ref StreetSet.empty in
  let score = ref 0 in
  s.itinaries |>
  Array.iter
    (fun l ->
       let n1 = ref (List.hd l) in
       List.tl l |>
       List.iter
         (fun n2 ->
            match get_vert d.city !n1 n2 with
            | Some (t,dist) -> if StreetSet.mem (!n1,n2) !visited
              then ()
              else
                begin
                  score := !score + dist;
                  visited := StreetSet.add (!n1,n2) (StreetSet.add (n2,!n1) !visited)
                end;
              n1 := n2
            | None -> raise (BadSol ("No street between nodes " ^ string_of_int !n1 ^ " and " ^ string_of_int n2))
         )
    );
  !score
    
module M : Problem.Problem =
  struct
    type input = data
    type solution = sol
    let parse_input = fun file -> file |> parse |> make_data
    let parse_output = parse_output
    let score = score
  end
	
