(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std

open Game
open Tests



let run_prog filename opt_tag print_solution () = 

  if not print_solution then run_test_suite ();

  let state = 
    Yojson.Basic.from_file filename |> first_state_of_json |> put_figure_on_board_and_go
  in
  if not print_solution then ignore(print_state state)
  else  printf {json|[{"problemId": %d
,"seed": %d
,"tag": "%s"
,"solution": "%s"
}]
|json} state.id state.initial_seed (Option.value opt_tag ~default:"OCAML.DEV") (s_of_moves state.moves)
;;




let main () =
  Command.basic
    ~summary: "ICFP-2015 OCaml submission / Raging Mushrooms"
    Command.Spec.(
      empty
      +> flag "-f" (required string) ~doc:"file_name Process the program given in file"
      +> flag "--tag" (optional string) ~doc:"tag Use tag in JSON"
      +> flag "-s" no_arg ~doc:" Print solution output"
    )
    run_prog
  |> Command.run
;;





let _ = main ()

