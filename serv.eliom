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
let out_solution (oc: out_channel) (group: int array) (ligne: int array) (column: int array) : unit =
  Array.iteri (fun s g ->
    if g < 0
    then Printf.fprintf oc "x\n"
    else Printf.fprintf oc "%d %d %d\n" ligne.(s) column.(s) g
  ) group;;

(** The problem under consideration **)
let d : data = read_data (open_in "dc.in") 

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

let participants: string list = ["Yannick"; "David"; "Martin"; "Patrice"; "Pierre"]

(** We store the scores as a hashtbl **)
let init_score_table =
  let init = Hashtbl.create (List.length participants) in
  List.iter (fun name -> Hashtbl.add init name 0) participants;
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

let setup () =
  (* creates directory if not present *)
  (try Unix.mkdir "local/var/data/serv/Upload" 0o755 with _ -> ())
     
let main_service =
  setup ();
  Eliom_service.Http.service
    ~path:["prout"]
    ~get_params:Eliom_parameter.unit
    ()

let upload =
  Eliom_registration.Html5.register_post_service
   ~fallback:main_service
   ~post_params:(Eliom_parameter.file "file")
   (fun () file ->
      let newname = "local/var/data/serv/Upload/testation" in
      (try
        Unix.unlink newname;
      with _ -> ());
      Lwt_unix.link (Eliom_request_info.get_tmp_filename file) newname;
      Lwt.return
        (html
           (head (title (pcdata "Upload")) [])
           (body [h1 [pcdata "ok"]]))
    )

let main_service2 =
  Eliom_registration.Html5.register main_service
   (fun () () ->
      let f =
        (post_form upload
           (fun file ->
             [p [file_input ~name:file ();
                 br ();
                 string_input ~input_type:`Submit ~value:"Send" ();
               ]]) ()) in
       (* let _ = {unit{ box () }} in *)
      Lwt.return
        (html
           (head (title (pcdata "Hash Code")) [])
           (body [
		h1 [pcdata "Hash Code"];
		(generate_score_table_html init_score_table);
		f
	   ])
	)
    )


(* {client{ *)
(*   let box () = Eliom_lib.alert "Hello!" *)
(* }} *)

(* module My_app = *)
(*   Eliom_registration.App (struct *)
(*       let application_name = "serv" *)
(*     end) *)


(* let count = ref 0 *)

(* {shared{ *)
(*   (\* Modules opened in the shared-section are available in client- *)
(*      and server-code *\) *)
(*   open Eliom_content.Html5.D *)
(*   open Lwt *)
(* }} *)

(* {shared{ *)
(*   let width = 700 *)
(*   let height = 400 *)
(* }} *)

(* {client{ *)
(*   let draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) = *)
(*     let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in *)
(*     ctx##strokeStyle <- (Js.string color); *)
(*     ctx##lineWidth <- float size; *)
(*     ctx##beginPath(); *)
(*     ctx##moveTo(float x1, float y1); *)
(*     ctx##lineTo(float x2, float y2); *)
(*     ctx##stroke() *)
(* }} *)

(* let canvas_elt = *)
(*   canvas ~a:[a_width width; a_height height] *)
(*     [pcdata "your browser doesn't support canvas"] *)

(* let page = *)
(*   (html *)
(*     (head (title (pcdata "Graffiti")) []) *)
(*     (body [h1 [pcdata "Graffiti"]; *)
(*            canvas_elt] ) ) *)

(* {client{ *)
(* let init_client () = *)

(*   let canvas = Eliom_content.Html5.To_dom.of_canvas %canvas_elt in *)
(*   let ctx = canvas##getContext (Dom_html._2d_) in *)
(*   ctx##lineCap <- Js.string "round"; *)

(*   let x = ref 0 and y = ref 0 in *)

(*   let set_coord ev = *)
(*     let x0, y0 = Dom_html.elementClientPosition canvas in *)
(*     x := ev##clientX - x0; y := ev##clientY - y0 *)
(*   in *)

(*   let compute_line ev = *)
(*     let oldx = !x and oldy = !y in *)
(*     set_coord ev; *)
(*     ((0, 0, 0), 5, (oldx, oldy), (!x, !y)) *)
(*   in *)

(*   let line ev = draw ctx (compute_line ev); Lwt.return () in *)

(*   Lwt.async *)
(*     (fun () -> *)
(*       let open Lwt_js_events in *)
(*       mousedowns canvas *)
(*         (fun ev _ -> *)
(*           set_coord ev; line ev >>= fun () -> *)
(*           Lwt.pick [mousemoves Dom_html.document (fun x _ -> line x); *)
(* 		    mouseup Dom_html.document >>= line])) *)
(* }} *)


(* let test2_service = *)
(*   My_app.register_service *)
(*      ~path:["test2"] *)
(*      ~get_params:Eliom_parameter.unit *)
(*     (fun () () -> *)
(*       let c = incr count; !count in *)
(*       {unit{ *)
(*         Dom_html.window##alert(Js.string *)
(* 	  (Printf.sprintf "You came %i times to this page" %c)) *)
(*       }}; *)
(*       Lwt.return *)
(*         (html *)
(*            (head (title (pcdata "Graffiti")) []) *)
(*            (body [h1 [pcdata "Graffiti"]]) ) ) *)

(* let coucou_service = *)
(*   My_app.register_service *)
(*     ~path:["coucou"] *)
(*     ~get_params:Eliom_parameter.unit *)
(*     (fun () () -> *)
(*       (\* Cf. the box "Client side side-effects on the server" *\) *)
(*       let _ = {unit{ init_client () }} in *)
(*       Lwt.return page) *)

(* (\** Test static: juste a static html5 page **\) *)
(* let test_service = *)
(*   Eliom_registration.Html5.register_service *)
(*     ~path:["test"] *)
(*     ~get_params:Eliom_parameter.unit *)
(*     (fun () () -> *)
(*       Lwt.return *)
(*         (html *)
(*            (head (title (pcdata "Hash")) []) *)
(*            (body [h1 [pcdata "Hash"]]) ) ) *)


