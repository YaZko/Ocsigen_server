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
                                            let d' = if b=1 then false else true in (a,b,d',time_cost,length)))
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
