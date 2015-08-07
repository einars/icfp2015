(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Game

open Core.Std


let run_test_suite () =

  let json_4x4 = {json|
  {"height":4,"width":4,"sourceSeeds":[0],"units":[],"id":0,"filled":[],"sourceLength":0}
  |json} in
  let json_5x5 = {json|
  {"height":5,"width":5,"sourceSeeds":[0],"units":[],"id":0,"filled":[],"sourceLength":0}
  |json} in

  let fd_4x4 = Yojson.Basic.from_string json_4x4 |> first_state_of_json
  and fd_5x5 = Yojson.Basic.from_string json_5x5 |> first_state_of_json in


  let fig_X =
    { members = [0,0]
    ; pivot   = 0,0
  } in

  let fig_XX =
    { members = [0,0; 1,0]
    ; pivot   = 0,0
  } in


  let fig_XXX =
    { members = [0,0; 1,0; 2,0]
    ; pivot   = 0,0
  } in


  assert ( figure_bounds fig_X = (0,0,0,0) );
  assert ( figure_bounds fig_XX = (0,0,1,0) );
  assert ( figure_bounds fig_XXX = (0,0,2,0) );

  assert ( initial_figure_offset fd_4x4 fig_X = (1,0) );
  assert ( initial_figure_offset fd_5x5 fig_X = (2,0) );
  assert ( initial_figure_offset fd_4x4 fig_XX = (1,0) );

  assert ( initial_figure_offset fd_5x5 fig_XX = (1,0) );

  assert ( initial_figure_offset fd_4x4 fig_XXX = (0,0) );
  assert ( initial_figure_offset fd_5x5 fig_XXX = (1,0) );

  let n_randoms n initial_seed =
    let out = ref []
    and seed = ref initial_seed in

    for i = 0 to n - 1  do
      let n, next_seed = next_random !seed in
      out := n :: !out;
      seed := next_seed;
    done;
    List.rev !out
  in



  let first_10_rands = n_randoms 10 17 in
  assert ( first_10_rands = [0; 24107; 16552; 12125; 9427; 13152; 21440; 3383; 6873; 16117]);

  let s_of_pt pt = sprintf "%d:%d" (fst pt) (snd pt) in

  let test_rotation turn_func func_title pivot pts =
    let rec vrfy = function 
    | pt::exp::rest ->
        if (turn_func pivot pt <> exp) 
        then printf "Rotating %s %s around %s expected %s, got %s\n"
          (s_of_pt pt) func_title (s_of_pt pivot) (s_of_pt exp) (turn_func pivot pt |> s_of_pt);
        vrfy (exp :: rest)
    | _ -> ()
    in

    vrfy pts
  in

  test_rotation turn_cw "CW" (2,6) [1,5; 2,5; 3,6; 2,7; 1,7; 1,6; 1,5];
  test_rotation turn_cw "CW" (2,5) [2,4; 3,4; 3,5; 3,6; 2,6; 1,5; 2,4];
  test_rotation turn_ccw "CCW" (2,6) [1,5; 1,6; 1,7; 2,7; 3,6; 2,5; 1,5];
  test_rotation turn_ccw "CCW" (2,5) [2,4; 1,5; 2,6; 3,6; 3,5; 3,4; 2,4];


  printf "Tests passed.\n%!";

;;




