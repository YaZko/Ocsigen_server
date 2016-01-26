open Batteries
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open Str
open Sqlite3
open Scoring


(***** Server/Client interaction *****)

(** The directory used to temporary store uploaded solutions **)
let uploadDir = "local/var/data/serv/Upload"

let db = db_open "users_db"

let debug msg =
  ignore (Lwt_log_core.log ~logger:(Lwt_log.channel
				      ~template: "$(message)"
				      ~close_mode:`Keep
				      ~channel:Lwt_io.stdout ())
			   ~level: Lwt_log_core.Notice
			   msg)

let insert_if_absent ?admin:(ad=false) db user pwd  =
  let req =
    Printf.sprintf
      "INSERT INTO users(id,name,pwd,score, admin) 
       SELECT NULL, '%s', '%s', 0, '%d'
       WHERE NOT EXISTS(SELECT 1 FROM users WHERE name = '%s')"
      user pwd (if ad then 1 else 0) user in
  let rc = exec db req in
  debug (Printf.sprintf "*****Insert code = %s\n" (Rc.to_string rc))

let debug_sql rc descr =
  debug
    (Printf.sprintf "****%s : %s\n" descr (Rc.to_string rc))
    
let setup () : unit =
  (* creates directory if not present *)
  (try Unix.mkdir uploadDir 0o755 with _ -> ());
  let rc = exec db "create table if not exists users
		    (id INTEGER PRIMARY KEY ASC, name, pwd, score, admin)" in
  debug_sql rc "Creating table";
  (* following two lines to be removed from final stuff *)
  insert_if_absent db "pwilke" "prout" ~admin:true;
  insert_if_absent db "Yannick" "test" ~admin:true;
  ()


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



(*** The session logics. Built following this tutorial: http://ocsigen.org/tuto/4.2/manual/interaction ***)


    
(** Service to create a new user **)
let new_user_form_service =
  Eliom_service.Http.service ~path:["registration"] ~get_params:Eliom_parameter.unit ()

(** Account creation service. I do not understand quite well the coservice thingy. **)
let create_account_service =
  Eliom_service.Http.post_coservice
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

(** Account creation confirmation service **)
let account_confirmation_service =
  Eliom_service.Http.post_coservice
    ~fallback:new_user_form_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

(** The creation is a bit tricky to handle confirmation, otherwise it just extends the list of users **)
let _ = Eliom_registration.Html5.register
	  ~service:account_confirmation_service
	  (fun () (name, pwd) ->
	   let create_account_service =
	     Eliom_registration.Action.register_coservice
               ~fallback:main_service
               ~get_params:Eliom_parameter.unit
               ~timeout:60.
               (fun () () ->
		let b = ref false in
		let rc = exec_not_null_no_headers
			   db
			   ~cb:(fun row -> b := (int_of_string row.(0)) <> 0)
			   (Printf.sprintf
			      "select count(*) from users where name='%s'" name) in
		debug_sql rc "Counting similar names";
		if !b
		then
		  Lwt.return ()
			     (* (html *)
			     (* 	 (head (title (pcdata "Username already used")) []) *)
			     (* 	 (body *)
			     (* 	    [h1 [pcdata ("Username "^name^" already used")]; *)
			     (* 	     p [pcdata "Please choose another one"]; *)
			     (* 	     br (); *)
			     (* 	     a ~service:main_service [pcdata "Back to account creation form"] *)
			     (* 	       () *)
			     (* 	    ] *)
			     (* 	 ) *)
			     (* ) *)
		else (
		  insert_if_absent db name pwd;
		  Lwt.return ()
		(* (html *)
	     	(*    (head (title (pcdata "Username already used")) []) *)
	     	(*    (body *)
	     	(*       [h1 [pcdata ("Registration done")]; *)

	     	(*       ] *)
	     	(*    ) *)
		(* ) *)
		))
	   in
	   Lwt.return
	     (html
		(head (title (pcdata "")) [])
		(body
		   [h1 [pcdata "Confirm account creation for "; pcdata name];
		    p [a ~service:create_account_service [pcdata "Yes"] ();
                       pcdata " ";
                       a ~service:main_service [pcdata "No"] ()]
		   ])))

(** The account creation form **)
let account_form =
  post_form ~service:account_confirmation_service
	    (fun (name1, name2) ->
	     [fieldset
		[label ~a:[a_for name1] [pcdata "login: "];
		 string_input ~input_type:`Text ~name:name1 ();
		 br ();
		 label ~a:[a_for name2] [pcdata "password: "];
		 string_input ~input_type:`Password ~name:name2 ();
		 br ();
		 string_input ~input_type:`Submit ~value:"Connect" ()
		]]) ()

(** Registration of the new user form **)
let _ =
  Eliom_registration.Html5.register
    ~service:new_user_form_service
    (fun () () ->
     Lwt.return
       (html (head (title (pcdata "")) [])
             (body [h1 [pcdata "Create an account"];
		    account_form;
                   ])))

(** Check that a connection is valid **)

let check_pwd name pwd =
  let b = ref false in
  let rc = exec_not_null_no_headers
	     db
	     (fun row ->
	      b := (int_of_string row.(0)) <> 0)
	     (Printf.sprintf
		"select count(*) from users where name='%s' and pwd='%s'" name pwd)
  in
  debug_sql rc "Checking password";
  !b

let is_admin name =
  let b = ref false in
  let rc = exec_not_null_no_headers
	     db
	     (fun row ->
	      b := (int_of_string row.(0)) <> 0)
	     (Printf.sprintf
		"select count(*) from users where name='%s' and admin='1'" name) in
  debug_sql rc "Is Admin?";
  !b
   
   
(** We setup the connection service. It takes post parameters so that name and
password aren't displayed in the url, and fallback to main_service if accessed
without post parameters. **)
let connection_service =
  Eliom_service.Http.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

(** Default session value **)

let username =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

let admin =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

let wrong_pwd = Eliom_reference.eref ~scope:Eliom_common.request_scope false

(** When we generate a new list, we convert the hashtbl to an association list in order to sort it before dumping it into html. Not quite subtle. **)    
let generate_score_table_html uname =
  let res = ref [] in
  let rc =
    exec_not_null_no_headers
      db
      (fun (row: string array) ->
       let (name,score) = (row.(0),row.(1)) in
       res := (li ~a:(match uname with
	     	      | Some u' when name = u' ->
	     		 [a_style "color:blue;"]
	     	      | _ -> [])
		  [pcdata (name ^ " : " ^ score)]):: !res;
      )
      "select name, score from users where admin='0' order by cast(score as integer) desc, name asc"
  in
  debug_sql rc "List of scores";
  ul (List.rev !res)

     
(** connection_service is registered as an action instead of a service so that
it only produces a side-effect and redirect to the main page rather than an
independent one **)
let _ = Eliom_registration.Action.register
	  connection_service
	  (fun () (name, password) ->
	   if check_pwd name password
	   then (
	     ignore (Eliom_reference.set username (Some name));
	     Eliom_reference.set admin (Some (is_admin name)))
	   else Eliom_reference.set wrong_pwd true)

let drop_db_service =
  Eliom_service.Http.coservice
    ~fallback:main_service
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
	  
(** The disconnection service **)

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
    lwt wp = Eliom_reference.get wrong_pwd in 
    Lwt.return
      (match u with
       | Some s -> div [p [pcdata "You are connected as "; pcdata s]; disconnect_box ()]
       | None -> 
	  let l =
	    [post_form connection_service
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
			   ]]) ();
	     p [a new_user_form_service [pcdata "Register a team"] ()];
	    ]
	  in
	  if wp 
	  then div ((p [em [pcdata "Wrong username or password"]])::l)
	  else div l
		   
      )

let get_score name =
  let res = ref 0 in
  let rc =
    exec_not_null_no_headers db
	 ~cb:(fun row -> res := int_of_string (row.(0)))
	 (Printf.sprintf "select score from users where name='%s'" name)
  in
  debug_sql rc "Getting score";
  !res
      
(** The upload service and handler **)
      
let upload =
  Eliom_registration.Html5.register_post_service
    ~fallback:main_service
    ~post_params:(Eliom_parameter.file "file")
    (fun () file ->
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
		    a ~service:main_service [pcdata "Return to the scores"] ()]))

     | Some name -> 
	try let score = scoring i in
	    let prev_score = get_score name in
	    if score > prev_score
	    then 
	      let rc =
		exec db
		     (Printf.sprintf
			"update users set score='%d' where name='%s'"
			score name
		     ) in
	      debug_sql rc "Updating score";
	      Lwt.return
		(html
		   (head (title (pcdata "Upload")) [])
		   (body [h1 [pcdata ("You scored : " ^ (string_of_int score));
			      br ();
			      a ~service:main_service [pcdata "Return to the scores"] ()]]))
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
			  a ~service:main_service [pcdata "Return to the scores"] ()]))
	with ParseError perror ->
	     Lwt.return
	       (html
		  (head (title (pcdata "Upload failed")) [])
		  (body [h1 [pcdata ("It seems that your solution is not a valid solution.")];
			 br ();
			 p [pcdata "Parser reported following error:";
			    br ();
			    pcdata perror];
			 a ~service:main_service [pcdata "Return to the scores"] ()]))
	   | End_of_file ->
	      Lwt.return
		(html
		   (head (title (pcdata "Upload failed")) [])
		   (body [h1 [pcdata ("It seems that your solution is not a valid solution.")];
			  br ();
			  p [pcdata "No Parser error:";
			     br () ];
			  a ~service:main_service [pcdata "Return to the scores"] ()]))
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
  Eliom_registration.Html5.register
    main_service
    (fun () () ->
     lwt ub = connection_box () in
     lwt u = Eliom_reference.get username in
     lwt ad = Eliom_reference.get admin in
     Lwt.return
       (html
	  (head (title (pcdata "Hash Code")) [])
	  (body ([
		    h1 [pcdata "Hash Code"];
		    (generate_score_table_html u);
		    (match u with
		     | None -> br () 
		     | Some _ ->
			upload_box ());
		    ub ]
		 @
		   (if ad = Some true
		    then
		      [br ();
		       p [a drop_db_service [pcdata "Clear database"] ()]]
		    else [])
		)))
    )
				    

