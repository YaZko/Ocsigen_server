open Batteries
open Input_output
open Scoring
       
let _ =
  let file =
    (try Sys.argv.(1) with _ -> "input") in
  let d = make_data (parse file)
  in
  Printf.printf "Score: %d\n" (score_output "output/ex.out" d)
  (* Draw.init 50 50; *)
  (* draw_car 25 25; *)
  (* Graphics.wait_next_event [Graphics.Button_down] *)
  (* paint_data d; *)
  (* let _ = Graphics.wait_next_event [Graphics.Button_down] in *)
  (* let sol = better_sol2 d in *)
  (* paint_sol d sol; *)
                (* let _ = *)
    (* in *)
  (* out_sol file sol *)
