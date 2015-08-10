(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std

open Gametypes
open Game
open Tests



let run_prog filenames opt_tag first is_debug _ _ _ power_words () = 

  if is_debug then run_test_suite ();

  let preprocess = if first then (fun f -> [List.hd_exn f]) else (fun f -> f) in

  let solutions = List.map filenames ~f:(fun (filename) ->
    List.map ~f:(solve ~power_words) (preprocess (Yojson.Basic.from_file filename |> states_of_json))
  ) in
  let states = List.fold solutions ~init:[] ~f:List.append
      (*


      let solutions = List.map ~f:solve (preprocess (Yojson.Basic.from_file filename |> states_of_json))
      
      List.fold ~init:[] fs ~f:(fun accum filename ->
      List.append accum (
        List.map ~f:solve (preprocess (Yojson.Basic.from_file filename |> states_of_json))
      )
      *)
  in
  if is_debug then List.iter ~f:(fun e -> ignore(print_state e)) states
  else (
    String.concat ~sep:"," (List.map states ~f:(fun state ->
    sprintf {json|{"problemId": %d
,"seed": %d
,"tag": "%s"
,"solution": "%s"
}
|json} state.id state.initial_seed (Option.value opt_tag ~default:"OCAML.DEV") (get_solution state)
    )) |> printf "[%s]";
  )

;;




let main () =
  Command.basic
    ~summary: "ICFP-2015 OCaml submission / Raging Mushrooms"
    Command.Spec.(
      empty
      +> flag "-f" (listed string) ~doc:"file_name Process the program given in file"
      +> flag "--tag" (optional string) ~doc:"tag Use tag in JSON"
      +> flag "--first" no_arg ~doc:" Process only the first seed"
      +> flag "--debug" no_arg ~doc:" Print debug output"
      +> flag "-t" (optional string) ~doc:" (unused) Time limit"
      +> flag "-m" (optional string) ~doc:" (unused) Memory limit"
      +> flag "-c" (optional string) ~doc:" (unused) Core limit"
      +> flag "-p" (listed string) ~doc:" (unused) Power word"
    )
    run_prog
  |> Command.run
;;





let _ = main ()

