open Batteries
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open Str

(***** Ranking section *****)

(** The type describing an instance of the problem **)
type data = {
  cols: int;
  rows : int;
  groups: int;
  is_undisp : bool array array;
  size : int array;
  capa : int array;
  ratio: (int * float) array;
}

(** Parse a problem and format it into a data **)
let read_data (stdin: in_channel) : data =
  (* Nombre de rangées *)
  (* Nombre d’emplacements *)
  (* Nombre d’emplacements indisponibles *)
  (* Nombre de groupes *)
  (* Nombre de serveurs *)
  Scanf.fscanf stdin "%d %d %d %d %d\n" (fun rows cols undisp groups servers ->
  (* Tableau des cases indisponibles idx -> row × col *)
    let undisp = Array.init undisp (fun _ ->
      Scanf.fscanf stdin "%d %d\n" (fun ri ci -> (ri, ci))
    ) in
  (* Tableau des serveurs à allouer idx -> size × capa *)
    let data = Array.init servers (fun _ ->
      Scanf.fscanf stdin "%d %d\n" (fun zi ci -> (zi, ci))
    ) in
  (** Tableau des cases indisponibles *)
    let is_undisp = Array.init rows (fun r ->
      Array.init cols (fun c -> false
      )) in
    Array.iter (fun (r, c) -> is_undisp.(r).(c) <- true) undisp;

  (** Ratio capa/taille par serveur *)
    let ratio = Array.init servers (fun s ->
      let (z,c) = data.(s) in
      (s, float_of_int c /. float_of_int z)
    ) in

    Array.sort (fun (s1, r1) (s2, r2) -> compare (r2, - fst data.(s2)) (r1, - fst data.(s1))) ratio;    
    let d : data = {
      cols = cols;
      rows = rows;
      groups = groups;
      ratio = ratio;
      is_undisp = is_undisp;
      size = Array.map fst data;
      capa = Array.map snd data;
    } in
    d
  )

(** A few auxilliary function to rank solutions **)
let rec foldmin n f: int =
  if n = 0 then f 0
  else min (f n) (foldmin (n - 1) f)

let rec foldsum n f: int =
  if n = 0 then f 0
  else (f n) + (foldsum (n - 1) f)    

let hurt_capa l gr d m i g : int =
  if (l.(m) = i
     || gr.(m) <> g) then 0
  else d.capa.(m)

(** guar_capa : granted capacity for group g for current allocation (l,gr) **)
let guar_capa l gr d g : int =
  let servers = Array.length d.size in
  foldmin (d.rows -  1)
    (fun i -> (foldsum (servers - 1)
		 (fun m -> hurt_capa l gr d m i g)))


(** The main function to grade a solution. 
Takes as an input the definition of the problem d and the description of the solution as an assignment from each server to its pool (gr), slot (c) and row (l) 
TODO : I think we assumed the solution to be legal, to double check and if so add a filter.
**)
let score_solution (d: data) gr c l : int =
  foldmin (d.groups - 1) (guar_capa l gr d)


(** First parsing of a solution into a list **)
let parse_output s =
  let f = Str.split (Str.regexp "\n") s in 
  let rec aux f l = 
    match f with
    | s::tl ->
       (try 
	Scanf.sscanf s "%d %d %d" 
		     (fun a b c -> aux tl ([a;b;c]::l))
       with
	 | Scanf.Scan_failure _ ->
	    Scanf.sscanf s "x" (aux tl ([-1]::l))    
       )
    | [] -> l
  in
  List.rev (aux f [])
;;


(** Printing of a well formed solution, only for debugging purpose on our side **)
(* let out_solution (oc: out_channel) (group: int array) (ligne: int array) (column: int array) : unit = *)
(*   Array.iteri (fun s g -> *)
(*     if g < 0 *)
(*     then Printf.fprintf oc "x\n" *)
(*     else Printf.fprintf oc "%d %d %d\n" ligne.(s) column.(s) g *)
(*   ) group;; *)

(** The problem under consideration **)
let d : data = read_data (Pervasives.open_in "dc.in") 

(** Main function to grade a submitted solution **)
let stuff sol =
  let out = parse_output sol in
  let ngr = List.length out in
  let gr = Array.init ngr (fun _ -> -1) in
  let line = Array.init ngr (fun _ -> -1) in
  let col = Array.init ngr (fun _ -> -1) in
  List.iteri (fun i x ->
	      match x with 
	      | [_] -> ()
	      | [l;c;g] -> line.(i) <- l; col.(i) <- c; gr.(i) <- g
	      | _ -> failwith "A parsed input should be a list containing only triplets or single element, this is weird.") out;
  score_solution d gr col line		 


(***** Server/Client interaction *****)

let participants: string list ref = ref ["Yannick"]

(** We store the scores as a hashtbl **)
let init_score_table =
  let init = Hashtbl.create (List.length !participants) in
  List.iter (fun name -> Hashtbl.add init name 0) !participants;
  init

let _ = Hashtbl.replace init_score_table "Pierre" (-1) 

(** When we generate a new list, we convert the hashtbl to an association list in order to sort it before dumping it into html. Not quite subtle. **)    
let generate_score_table_html htblt = 
  let t = Hashtbl.fold (fun name score acc -> (name,score)::acc) htblt [] in
  let t = List.sort (fun (s1,p) (s2,q) -> let c = compare p q in if c = 0 then - (compare s1 s2) else - c) t in
  ul (
      List.fold_left
	(fun acc (name, score) -> 
	 (li [pcdata (name ^ " : " ^ string_of_int score)]):: acc)
        [] t
    ) 

(** The directory used to temporary store uploaded solutions **)
let uploadDir = "local/var/data/serv/Upload"
     
let setup () =
  (* creates directory if not present *)
  (try Unix.mkdir uploadDir 0o755 with _ -> ())

(** The main service **)
     
let main_service =
  setup ();
  Eliom_service.Http.service
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()

(*** Dealing with accounts ***)

(** Create a service at /users/name for any name **)
let user_service =
  Eliom_registration.Html5.register_service
    ~path:["users"]
    ~get_params:
       (Eliom_parameter.suffix (Eliom_parameter.string "name"))
    (fun name () ->       Lwt.return
        (html
           (head (title (pcdata "Hash Code")) [])
           (body [
		h1 [pcdata name];
		p [a ~service:main_service [pcdata "Home"] ()]
    ] ))
    )

let users: (string * string) list ref = ref [("Yannick","coucou")]

(** Check that a connection is valid **)

let check_pwd name pwd =
  try List.assoc name !users = pwd with Not_found -> false

(** We setup the connection service. It takes post parameters so that name and password aren't displayed in the url, and fallback to main_service if accessed without post parameters. **)
let connection_service =
  Eliom_service.Http.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

(** Default session value **)

let username =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope None


(** connection_service is registered as an action instead of a service so that it only produces a side-effect and redirect to the main page rather than an independent one **)
let _ = Eliom_registration.Action.register
    connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Lwt.return ())

(** The disconection service **)

let disconnection_service =
  Eliom_service.Http.post_coservice' ~post_params:Eliom_parameter.unit ()

let disconnect_box () =
  post_form disconnection_service
    (fun _ -> [p [string_input
                    ~input_type:`Submit ~value:"Log out" ()]]) ()

let _ =
  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.default_session_scope ())


(** The connection form to add to the main service **)
let connection_box () =
  lwt u = Eliom_reference.get username in
    Lwt.return
      (match u with
       | Some s -> div [p [pcdata "You are connected as "; pcdata s]; disconnect_box ()]
       | None -> 
	  post_form connection_service
		    (fun (name1, name2) ->
		     [fieldset
			[label ~a:[a_for name1] [pcdata "login: "];
			 string_input ~input_type:`Text
                                      ~name:name1 ();
			 br ();
			 label ~a:[a_for name2] [pcdata "password: "];
			 string_input ~input_type:`Password
                                      ~name:name2 ();
			 br ();
			 string_input ~input_type:`Submit
                                          ~value:"Connect" ()
		    ]]) ()
      )
  
(** The upload service and handler **)
 
let upload =
  Eliom_registration.Html5.register_post_service
   ~fallback:main_service
   ~post_params:(Eliom_parameter.file "file")
   (fun () file ->
      let newname = uploadDir ^ "/testation" in
      (try
        Unix.unlink newname;
      with _ -> ());
      Unix.link (Eliom_request_info.get_tmp_filename file) newname;
      let i = File.lines_of newname |> Enum.reduce (fun a b -> a ^ "\n" ^ b ) in
      let score = 
      try
      	  Printf.sprintf "Solution has score %d" (stuff i)
      	with
      	  Invalid_argument s ->
      	  Printf.sprintf "Error while scoring: '%s'" s
      	| _ -> "Unknown error while scoring" in
      Lwt.return
        (html
           (head (title (pcdata "Upload")) [])
           (body [h1 [pcdata score]]))
    )

(** The upload form **)

let upload_box () = 
  post_form upload
             (fun file ->
              [p [file_input ~name:file ();
                  br ();
                  string_input ~input_type:`Submit ~value:"Send" ();
             ]]) ()
 
(** Registration of the main service **)

let main_service2 =
  Eliom_registration.Html5.register main_service
   (fun () () ->
    lwt ub = connection_box () in
    Lwt.return
        (html
           (head (title (pcdata "Hash Code")) [])
           (body [
		h1 [pcdata "Hash Code"];
		(generate_score_table_html init_score_table);
		upload_box ();
		ub
	   ])
	)
    )

