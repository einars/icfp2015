(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std

open Game
open Tests





let print_state state =

  let s = freeze_figure state in
  
  printf "%d×%d ID=%d, all_figs=%d, remaining=%d, seed=%d\n" s.width s.height s.id (List.length s.figures) s.remaining s.seed;
  for row = 0 to s.height - 1 do
    if (row % 2 = 1) then printf " ";
    for col = 0 to s.width - 1 do

      printf "%s" (if s.field.(row).(col) then "XX" else "··");

    done;
    printf "\n";
  done;
  printf "\n%!"
;;



let pick_best_move state =
  if true then raise (Locked state);
  state


let rec put_figure_on_board_and_go st =
  let n, next_seed = next_random st.seed in
  let fig = List.nth_exn st.figures (n mod (List.length st.figures)) in
  let st = { st with
    seed = next_seed;
  } in
  try
    initially_place_figure st fig |> pick_best_move |> put_figure_on_board_and_go
  with Locked state -> state
;;


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

