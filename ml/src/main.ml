(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std

open Game
open Tests






let main () =

  let file_name = (try
    Sys.argv.(1)
  with _ -> "../problems/problem_0.json"
  ) in

  run_test_suite();

  let ss = Yojson.Basic.from_file file_name |> states_of_json in
  let first_game = List.hd_exn ss in
  let state = put_figure_on_board_and_go first_game in
  print_state state;
;;  

let _ = main ()

