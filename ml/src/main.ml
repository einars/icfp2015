(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std

open Game
open Tests



let run_prog filename opt_tag first print_solution () = 

  if not print_solution then run_test_suite ();

  let preprocess = if first then (fun f -> [List.hd_exn f]) else (fun f -> f) in

  let states = 
    List.map ~f:solve  (preprocess (Yojson.Basic.from_file filename |> states_of_json))
  in
  if not print_solution then List.iter ~f:(fun e -> ignore(print_state e)) states
  else (
    String.concat ~sep:"," (List.map states ~f:(fun state ->
    sprintf {json|{"problemId": %d
,"seed": %d
,"tag": "%s"
,"solution": "%s"
}
|json} state.id state.initial_seed (Option.value opt_tag ~default:"OCAML.DEV") (s_of_moves state.moves)
    )) |> printf "[%s]";
  )

;;




let main () =
  Command.basic
    ~summary: "ICFP-2015 OCaml submission / Raging Mushrooms"
    Command.Spec.(
      empty
      +> flag "-f" (required string) ~doc:"file_name Process the program given in file"
      +> flag "--tag" (optional string) ~doc:"tag Use tag in JSON"
      +> flag "--first" no_arg ~doc:" Process only the first seed"
      +> flag "-s" no_arg ~doc:" Print solution output"
    )
    run_prog
  |> Command.run
;;





let _ = main ()

