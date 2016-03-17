open Batteries
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open Str
open Sqlite3
open Scoring_loons
open Scoring_paint
open Config
open Users
open Problem

let show_solution =
  Eliom_service.Http.service
    ~path:["solution"]
    ~get_params:Eliom_parameter.
                  (string "uname" ** string "pid" ** string "iid") ()

let _ =
  Eliom_registration.Html5.register
    ~service:show_solution
    (fun (requname,(pid,iid)) () ->
       lwt uname = Eliom_reference.get username in
       let is_current_user u =
         match uname with
	   None -> false
         | Some u' -> u = u' in
       let req = (Printf.sprintf
		    "SELECT solution, score 
FROM submissions
LEFT OUTER JOIN users
ON submissions.uid = users.uid
LEFT OUTER JOIN problems 
ON submissions.pid = problems.pid
LEFT OUTER JOIN inputs
ON submissions.iid = inputs.iid
WHERE (admin='0' OR 1=1)
AND problems.pname = '%s' AND inputs.iname = '%s'
AND users.uname = '%s'"
		    (secure pid) (secure iid) (secure requname))
       in
       let res = ref ("",0) in
       let rc =
         exec_not_null_no_headers
	   db
	   (fun (row: string array) ->
	      let (solution,score) = (row.(0),int_of_string row.(1)) in
	      res := (solution,score)
	   )
	   req	 
       in
       (* debug_sql rc ("Recup solution\nReq:"^req); *)
       Lwt.return
         (html
	    (head
	       (title (pcdata "Solution")) [])
	    (body
	       (if is_current_user requname
	        then 
		  [
		    h2 [pcdata
			  (Printf.sprintf
			     "Solution of user %s to problem %s on input %s" requname pid iid)];
		    pcdata
		      (Printf.sprintf
		         "Solution has a score of %d"
		         (snd !res));
		    br ();
		    pre [pcdata (fst !res)]
		  ]
	        else
		  []
	       )
	    )
         )
    )

(* Generates the scores for problem `pb` and input `inp` as a
   <ul>. Current user `uname` is highlighted. *)
let generate_score_table_html uname pb inp =
  let res = ref [] in
  let is_current_user u =
    match uname with
      None -> false
    | Some u' -> u = u' in
  let req = (Printf.sprintf
	       "SELECT uname, score 
FROM users
LEFT OUTER JOIN submissions 
ON submissions.uid = users.uid
LEFT OUTER JOIN problems 
ON submissions.pid = problems.pid
LEFT OUTER JOIN inputs
ON submissions.iid = inputs.iid
WHERE (admin='0' OR 1=1)
AND problems.pname = '%s' AND inputs.iname = '%s'
ORDER BY CAST(score as integer) DESC, uname ASC"
	       (secure pb) (secure inp))
  in
  let rc =
    exec_not_null_no_headers
      db
      (fun (row: string array) ->
         let (name,score) = (row.(0),row.(1)) in
         if is_current_user name
         then res := (li ~a:[a_style "color:blue;"]
		        [a
			   ~service:show_solution
			   [pcdata (name ^ " : " ^ score)]
		       	   (name,( pb, inp))]):: !res
         else
           res := (li [pcdata (name ^ " : " ^ score)]):: !res;
      )
      req	 
  in
  (* debug_sql rc ("List of scores\nReq:"^req); *)
  ul (List.rev !res)

let get_score name pb inp =
  let res = ref 0 in
  let req =
    (Printf.sprintf
       "SELECT score
FROM users 
LEFT OUTER JOIN submissions
ON submissions.uid = users.uid
LEFT OUTER JOIN problems
ON submissions.pid = problems.pid
LEFT OUTER JOIN inputs
ON submissions.iid = inputs.iid
WHERE users.uname='%s'
AND problems.pname = '%s'
AND inputs.iname = '%s'
" (secure name) (secure pb) (secure inp)) in
  let rc =
    exec_not_null_no_headers
      db
      ~cb:(fun row -> res := int_of_string (row.(0)))
      req
  in
  debug_sql rc ("Getting score\nReq:"^req);
  !res

(** The upload service and handler **)

let upload problem =
  Eliom_registration.Html5.register_post_service
    ~fallback:main_service
    ~post_params:
      Eliom_parameter.
        (file "file" ** string "input")
    (fun (_,_)
      (file, input) ->
      lwt u = Eliom_reference.get username in
      let newname = uploadDir ^ "/testation" in
      (try
	 Unix.unlink newname;
       with _ -> ());
      Unix.link (Eliom_request_info.get_tmp_filename file) newname;
      let i = File.lines_of newname |> Enum.reduce (fun a b -> a ^ "\n" ^ b ) in
      match u with
      | None -> 
	Lwt.return
	  (html
	     (head (title (pcdata "Unexpected error")) [])
	     (body [h1 [pcdata ("You shouldn't be able to upload without being logged")];
		    br ();
		    a ~service:main_service [pcdata "Return to the scores"] (Some problem,Some input)]))

      | Some name -> 
        let implem = Hashtbl.find implems problem in
	let module I = (val implem : Problem.Problem) in
        try
	  (if Hashtbl.mem implems problem
           then
             begin 
               debug "1\n";
	       let data = I.parse_input input in
               debug "2\n";
	       let sol = I.parse_output data i in 
               debug "3\n";
	       let score = I.score data sol in
               debug "4\n";
	       let prev_score = get_score name problem input in
               debug (Printf.sprintf "Score was %d, new score is %d\n" prev_score score);
	       if score > prev_score
	       then
	         let req =
		   (Printf.sprintf
		      "INSERT OR REPLACE INTO submissions 
VALUES (NULL, (SELECT uid FROM users WHERE uname = '%s'),
(SELECT pid FROM problems WHERE pname = '%s'),
(SELECT iid FROM inputs WHERE iname = '%s'),
%d,
'%s')"
		      (secure name) (secure problem) (secure input) score
		      (secure i)
		   ) in
	         let rc =
		   exec db req
	         in
	         debug_sql rc ("Updating score\n Req:"^req);
                 debug (Printf.sprintf "\nScore now is: %d\n" (get_score name problem input));
	         Lwt.return
		   (html
		      (head (title (pcdata "Upload")) [])
		      (body [h1 [pcdata ("You scored : " ^ (string_of_int score));
			         br ();
			         a ~service:main_service [pcdata "Return to the scores"] (Some problem,Some input)]]))
	       else 
	         Lwt.return
		   (html
		      (head (title (pcdata "Upload")) [])
		      (body [h1 [pcdata "Upload successful"];
			     pcdata ("You scored : " ^ (string_of_int score));
			     br ();
			     pcdata (Printf.sprintf
				       "It is not better than your previous score: %d"
				       prev_score);
			     br ();
	                     a ~service:main_service [pcdata "Return to the scores"] (Some problem,Some input)]))
             end
           else
             Lwt.return
               (html
                  (head (title (pcdata "Upload failed")) [])
                  (body [h1 [pcdata ("The problem you try to upload a solution for is not known to the system.")];
	                 br ();
	                 p [pcdata "That definitely should not have happened, please contact administrator.";
		            br ();
                            pcdata (Printf.sprintf "The unknown problem has id '%s'" problem)]]))
          )
        with
        | Exn.ParseError perror ->
          Lwt.return
            (html
               (head (title (pcdata "Upload failed")) [])
               (body [h1 [pcdata ("It seems that your solution is not a valid solution.")];
	              br ();
	              p [pcdata "Parser reported the following error:";
		         br ();
		         pcdata perror];
	              a ~service:main_service [pcdata "Return to the scores"] (Some problem,Some input)]))
        | End_of_file ->
          Lwt.return
            (html
	       (head (title (pcdata "Upload failed")) [])
	       (body [h1 [pcdata ("It seems that your solution is not a valid solution.")];
		      br ();
		      p [pcdata "No Parser error:";
		         br () ];
		      a ~service:main_service [pcdata "Return to the scores"] (Some problem,Some input)]))
    )

(** The upload form **)

let select_of_list name pb =
  let l : string list = (Hashtbl.find_all inputs pb)
  in
  match l with
    [] -> failwith "No problem man. Wait, yes, there's a problem: there are no problems registered. Understand me, yo?"
  | inp::r ->
    string_select
      ~name:name
      (Option ([],inp,None,true))
      (r |> List.map (fun inp -> (Option ([],inp,None,false))))

let upload_box pb = 
  post_form (upload pb)
    (fun (file,input) ->
       [p [
	  pcdata "Input solved: ";
	  select_of_list input pb;
	  br();
          pcdata "Your solution: ";                 
	  file_input ~name:file ();
	  br ();
	  string_input ~input_type:`Submit ~value:"Send" ();
	]]) (None,None)

let drop_db_service =
  Eliom_service.Http.coservice'
    (* ~fallback:main_service *)
    ~get_params:Eliom_parameter.unit
    ()

let _ = Eliom_registration.Action.register
    drop_db_service
    (fun () () ->
       let rc = exec db "drop table users" in
       debug (Printf.sprintf "******Drop table %s\n"
		(Rc.to_string rc));
       setup ();
       Lwt.return ())


(** Registration of the main service **)

let pbinput (pb,inp) =
  match pb,inp with
    None, _
  | _, None -> List.hd problems,
	       (Hashtbl.find inputs (List.hd problems))
  | Some pb, Some inp -> pb,inp

let links_inputs pb =
  (
    Hashtbl.fold
      (fun pb' inp acc ->
         if pb = pb'
         then (li [a ~service:main_service
	             [pcdata inp]
	             (Some pb, Some inp)]) ::
	      acc
         else acc)
      inputs [])
  |> ul

let links_problems pb =
  problems
  |> List.fold_left
    (fun acc pb' ->
       if pb = pb'
       then acc
       else
         try
           let inp = Hashtbl.find inputs pb' in 
           (li [a ~service:main_service
	          [pcdata pb']
	          (Some pb', Some inp)]) ::
	   acc
         with
           _ -> acc)
    [] 
  |> ul

let sendpdf =
  Eliom_registration.File.register_service
    ~path:["getpdf"]
    ~get_params:Eliom_parameter.(string "problem")
    (fun problem () -> Lwt.return (problem ^ ".pdf"))

let sendinput =
  Eliom_registration.File.register_service
    ~path:["getinput"]
    ~get_params:Eliom_parameter.(string "input")
    (fun input () -> Lwt.return input)

let main_service2 =
  Eliom_registration.Html5.register
    main_service
    (fun (pb,inp) () ->
       let (pb,inp) = pbinput (pb,inp) in
       lwt ub = connection_box () in
       lwt u = Eliom_reference.get username in
       lwt ad = Eliom_reference.get admin in
       Lwt.return
         (html
	    (head (title (pcdata "Hash Code")) [])
	    (body ([
	       h1 [pcdata "Hash Code"];                    h2 [pcdata "Current problem: ";
                                                               a ~service:sendpdf [pcdata (Printf.sprintf "%s" pb)] pb];
	       h4 [pcdata "You may want to switch to another problem:"];
	       (links_problems pb);
	       h3 [pcdata "Current input: ";
                   a ~service:sendinput [pcdata (Printf.sprintf "%s" inp)] inp];
	       h4 [pcdata "You may want to choose another input:"];
	       (links_inputs pb);
	       h2 [pcdata (Printf.sprintf
			     "Scores for problem %s on input %s"
			     pb inp)];
	       (generate_score_table_html u pb inp);
	       (match u with
		| None -> br ()
		| Some _ -> upload_box pb);
	       ub ]
	       @
	       (if ad = Some true
		then
		  [br ();
		   p [a drop_db_service [pcdata "Clear database"] ()]]
		else [])
	     )))
    )



