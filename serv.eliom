open Batteries
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
open Str
open Sqlite3

open Scoring


(***** Server/Client interaction *****)

let participants: string list ref = ref []

(** We store the scores as a hashtbl **)
let score_table = Hashtbl.create 10


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

let insert_if_absent db user pwd =
  let req =
    Printf.sprintf
      "INSERT INTO users(id,name,pwd,score) 
       SELECT NULL, '%s', '%s', 0
       WHERE NOT EXISTS(SELECT 1 FROM users WHERE name = '%s')"
      user pwd user in
  let rc = exec db req in
  debug (Printf.sprintf "*****Insert code = %s\n" (Rc.to_string rc))
	
let users: (string * string) list ref = ref []

let setup () : unit =
  (* creates directory if not present *)
  (try Unix.mkdir uploadDir 0o755 with _ -> ());
  let rc = exec db "create table if not exists users
		    (id INTEGER PRIMARY KEY ASC, name, pwd, score)" in
  (* following two lines to be removed from final stuff *)
  insert_if_absent db "pwilke" "prout";
  insert_if_absent db "Yannick" "test";

  let rc = exec_not_null db
  		~cb:(fun (row: string array) (headers: string array) ->
  		     users := (row.(1),row.(2))::!users;
  		     Hashtbl.replace score_table row.(1) (int_of_string row.(3))
  		    )
  		"select * from users" in
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
	 if List.mem_assoc name !users
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
	 else ((* Hashtbl.add score_table name 0; *)
               users := (name, pwd)::!users;
	       insert_if_absent db name pwd;
	       exec_not_null db
			     (fun row hd ->
			      Hashtbl.add score_table name (int_of_string row.(0))
			     )
		    (Printf.sprintf
		       "select score from users where name='%s'"
		       name
		    );
	       
               Lwt.return ()))
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
  try List.assoc name !users = pwd with Not_found -> false

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

let wrong_pwd = Eliom_reference.eref ~scope:Eliom_common.request_scope false

(** When we generate a new list, we convert the hashtbl to an association list in order to sort it before dumping it into html. Not quite subtle. **)    
let generate_score_table_html htblt uname = 
  let t = Hashtbl.fold (fun name score acc -> (name,score)::acc) htblt [] in
  let t = List.sort (fun (s1,p) (s2,q) -> let c = compare p q in if c = 0 then - (compare s1 s2) else - c) t in
  ul (
      List.fold_left
	(fun acc (name, score) -> 
	 (li ~a:(match uname with
	     	 | Some u' when name = u' ->
	     	    [a_style "color:blue;"]
	     	 | _ -> [])
	     [pcdata (name ^ " : " ^ string_of_int score)]):: acc)
        [] t
    ) 

				     
(** connection_service is registered as an action instead of a service so that
it only produces a side-effect and redirect to the main page rather than an
independent one **)
let _ = Eliom_registration.Action.register
    connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
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
	   users := [];
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
	     Hashtbl.replace score_table name score;
	     exec db
		  (Printf.sprintf
		     "update users set score='%d' where name='%s'"
		     score name
		  );
	     Lwt.return
               (html
		  (head (title (pcdata "Upload")) [])
		  (body [h1 [pcdata ("You scored : " ^ (string_of_int score));
			     br ();
			     a ~service:main_service [pcdata "Return to the scores"] ()]]))
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
  Eliom_registration.Html5.register main_service
   (fun () () ->
    lwt ub = connection_box () in
    lwt u = Eliom_reference.get username in
    Lwt.return
        (html
           (head (title (pcdata "Hash Code")) [])
           (body [
		h1 [pcdata "Hash Code"];
		(generate_score_table_html score_table u);

		p [pcdata "No solution has been given by the following registrated users:"];
		ul
		  (!users |>
		     List.filter_map (fun (name,_) ->
				      if not (Hashtbl.mem score_table name)
				      then Some (li
						   ~a:(match u with
							 None -> []
						       | Some u' ->
							  if name = u' then
							    [a_style "color:blue;"]
							  else [])
						   [pcdata name])
				      else None
				     )
		  );
		
		(match u with
		| None -> br () 
		| Some _ ->
		   upload_box ());
		ub;
		br ();
		p [a drop_db_service [pcdata "Clear database"] ()]
	   ])
	)
    )

