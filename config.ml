open Batteries
open Sqlite3
open Problem

       
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

let secure s = Str.global_replace (Str.regexp_string "'") "''" s
	 
let insert_if_absent ?admin:(ad=false) db user pwd  =
  let req =
    Printf.sprintf
      "INSERT INTO users(uid,uname,pwd, admin) 
       SELECT NULL, '%s', '%s', '%d'
       WHERE NOT EXISTS(SELECT 1 FROM users WHERE uname = '%s')"
      (secure user) (secure pwd) (if ad then 1 else 0) (secure user) in
  let rc = exec db req in
  debug (Printf.sprintf "*****Insert code = %s\nReq=%s\n" (Rc.to_string rc) req)

let insert_pb_if_absent db pb_name  =
  let req =
    Printf.sprintf
      "INSERT INTO problems(pid,pname) 
       SELECT NULL, '%s'
       WHERE NOT EXISTS(SELECT 1 FROM problems WHERE pname = '%s')"
      (secure pb_name) (secure pb_name) in
  let rc = exec db req in
  debug (Printf.sprintf "*****Insert code = %s\nReq=%s\n" (Rc.to_string rc) req)


let insert_input_if_absent db pb_name in_name  =
  let req =
    Printf.sprintf
      "INSERT INTO inputs(iid,pid,iname) 
       SELECT NULL, (SELECT pid FROM problems WHERE pname = '%s'), '%s'
       WHERE NOT EXISTS(SELECT 1 FROM inputs WHERE iname = '%s')"
      (secure pb_name) (secure in_name) (secure in_name) in
  let rc = exec db req in
  debug (Printf.sprintf "*****Insert code = %s\nReq=%s\n" (Rc.to_string rc) req)

	
	
let debug_sql rc descr =
  debug
    (Printf.sprintf "****%s : %s\nSQL error:%s\n" descr (Rc.to_string rc) (errmsg db))

let implems = Hashtbl.create 17
let problems = ["balloons";"paint"]
		  
let inputs = Hashtbl.create 17
  
let setup () : unit =
  (* creates directory if not present *)
  (try Unix.mkdir uploadDir 0o755 with _ -> ());
  let
    sql =
    "CREATE TABLE if not exists `inputs` (
     `iid`INTEGER PRIMARY KEY AUTOINCREMENT,
     `pid`INTEGER NOT NULL,
     `iname`TEXT NOT NULL
     );
     CREATE TABLE  if not exists `problems` (
     `pid`INTEGER PRIMARY KEY AUTOINCREMENT,
     `pname`TEXT NOT NULL
     );
     CREATE TABLE if not exists `submissions` (
     `sid`INTEGER PRIMARY KEY AUTOINCREMENT,
     `uid`INTEGER NOT NULL,
     `pid`INTEGER NOT NULL,
     `iid`INTEGER NOT NULL,
     `score`INTEGER,
     `solution`TEXT NOT NULL
     );
     CREATE TABLE if not exists `users` (
     `uid`INTEGER PRIMARY KEY AUTOINCREMENT,
     `uname`TEXT NOT NULL,
     `pwd`TEXT,
     `admin`INTEGER
     );
     CREATE UNIQUE INDEX sub_idx ON submissions(uid,pid,iid);" in
  let rc = exec db sql in
  debug_sql rc "Creating table";
  (* following two lines to be removed from final stuff *)
  insert_if_absent db "Yannick" "test" ~admin:true;

  let implem_balloons = (module Scoring.M : Problem) in
  Hashtbl.replace implems "balloons" implem_balloons;
  let implem_paint = (module Scoring_paint.M : Problem) in
  Hashtbl.replace implems "paint" implem_paint;
  Hashtbl.add inputs "balloons" "ex.in";
  Hashtbl.add inputs "balloons" "final_round.in";
  Hashtbl.add inputs "paint" "paint_input";

  inputs |>
    Hashtbl.iter (fun pb inp ->
		  insert_pb_if_absent db pb;
		  insert_input_if_absent db pb inp);
  
  

  ()
