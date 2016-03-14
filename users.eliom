open Batteries
open Eliom_content.Html5.D
open Sqlite3
open Config


(***** Server/Client interaction *****)
(** Default session value **)
let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    None

let admin =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    None

let wrong_pwd =
  Eliom_reference.eref
    ~scope:Eliom_common.request_scope
    false
    
(** The main service **)
    
let main_service =
  setup ();
  Eliom_service.Http.service
    ~path:[""]
    ~get_params:Eliom_parameter.(opt (string "pid") ** opt (string "iid"))
    ()

    
(*** The session logics. Built following this tutorial: http://ocsigen.org/tuto/4.2/manual/interaction ***)
    
(** Service to create a new user **)
let new_user_form_service =
  Eliom_service.Http.service
    ~path:["registration"]
    ~get_params:Eliom_parameter.unit ()

(** Account creation service. I do not understand quite well the coservice thinggy. **)
let create_account_service =
  Eliom_service.Http.post_coservice
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

(** Account creation confirmation service **)
let account_confirmation_service =
  Eliom_service.Http.post_coservice
    ~fallback:new_user_form_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

(** The creation is a bit tricky to handle confirmation, otherwise it just extends the list of users **)
let _ =
  Eliom_registration.Html5.register
    ~service:account_confirmation_service
    (fun () (name, pwd) ->
     let create_account_service =
       Eliom_registration.Action.register_coservice'
         (* ~fallback:main_service *)
         ~get_params:Eliom_parameter.unit
         ~timeout:60.
         (fun () () ->
	  let b = ref false in
	  let rc =
	    exec_not_null_no_headers
	      db
	      ~cb:(fun row -> b := (int_of_string row.(0)) <> 0)
	      (Printf.sprintf
		 "select count(*) from users where uname='%s'"
		 (secure name)) in
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
                 a ~service:main_service [pcdata "No"] (None,None)]
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
		"select count(*) from users where uname='%s' and pwd='%s'" (secure name) (secure pwd))
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
		"select count(*) from users where uname='%s' and admin='1'" (secure name)) in
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
			   ]]) (None,None);
	     p [a new_user_form_service [pcdata "Register a team"] ()];
	    ]
	  in
	  if wp 
	  then div ((p [em [pcdata "Wrong username or password"]])::l)
	  else div l
		   
      )


(** connection_service is registered as an action instead of a service so that
it only produces a side-effect and redirect to the main page rather than an
independent one **)
let _ = Eliom_registration.Action.register
	  connection_service
	  (fun (_,_) (name, password) ->
	   if check_pwd name password
	   then (
	     ignore (Eliom_reference.set username (Some name));
	     Eliom_reference.set admin (Some (is_admin name)))
	   else Eliom_reference.set wrong_pwd true)
