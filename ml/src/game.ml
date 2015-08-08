(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std
open Gametypes
open Movement


let s_of_pt pt = sprintf "[%d:%d]" (fst pt) (snd pt)

let first_moves = [ MOVE_SE ; MOVE_SW ; TURN_CW; TURN_CCW; MOVE_E ; MOVE_W ; TURN_CW ; TURN_CCW ]

let s_of_moves moves = 
  let mcs = List.rev_map moves ~f:(function 
    | MOVE_W -> "p"
    | MOVE_E -> "b"
    | MOVE_SW -> "a"
    | MOVE_SE -> "m"
    | TURN_CW -> "q"
    | TURN_CCW -> "k"
    (*| LOCK_MARK -> ""*)
    | LOCK_MARK -> ""
  ) in
  String.concat mcs
;;

let s_of_moves_dbg moves = 
  let mcs = List.rev_map moves ~f:(function 
    | MOVE_W -> "<"
    | MOVE_E -> ">"
    | MOVE_SW -> "/"
    | MOVE_SE -> "\\"
    | TURN_CW -> "+"
    | TURN_CCW -> "-"
    (*| LOCK_MARK -> ""*)
    | LOCK_MARK -> " "
  ) in
  String.concat mcs
;;



let figure_hash fig =
  let items = List.sort ~cmp:(fun (a,b) (c,d) -> if a = c then b - d else a - c) fig.members
  and out = ref [] in
  List.iter items ~f:(fun pt -> out := (s_of_pt pt) :: !out );
  String.concat !out
;;
  

let figure_bounds fig =

  let (first_x, first_y) = List.hd_exn fig.members in

  let xmin = ref first_x
  and xmax = ref first_x
  and ymin = ref first_y
  and ymax = ref first_y in

  let iter_points = fun (x, y) ->
    if x < !xmin then xmin := x;
    if x > !xmax then xmax := x;
    if y < !ymin then ymin := y;
    if y > !ymax then ymax := y in

  List.iter fig.members ~f:iter_points;

  !xmin, !ymin, !xmax, !ymax
;;


let initial_figure_offset state fig =
  (* returns... pivot? *)
  let xmin, ymin, xmax, ymax = figure_bounds fig in
  let width = xmax - xmin + 1 in
  let xoffs = (state.width - width) / 2 - xmin in
  let yoffs = - ymin in
  xoffs, yoffs
;;

let offset_fig fig xoffs yoffs =
  let px, py = fig.pivot in
  { members = List.map fig.members ~f:(fun (x,y) -> (x + xoffs), (y + yoffs) )
  ; pivot   = (px + xoffs), (py + yoffs)
  }
;;







let next_random seed =
  seed lsr 16 land 0x7fff, (seed * 0x41c64e6d + 12345) land 0xffffffff
;;


let pt_solid print_mode state (x,y) =

  let rec touch_rec drops x y = function 
  | [] -> if x < 0 || x >= state.width || y >= state.height then true else 
    if y < 0 then y < -drops else state.field.(y).(x)
  | (Finish _)                 :: rest -> touch_rec drops x y rest
  (* | (LivePlacement _)          :: rest -> touch_rec x y rest *)
  | (LivePlacement (fig, _))          :: rest -> 
      if print_mode && (List.mem fig.members (x,y)) then true
      else touch_rec drops x y rest
  | (ColumnDrop col)           :: rest -> touch_rec (drops + 1) x (if y <= col then y - 1 else y) rest
  | (LockedPlacement (fig, _)) :: rest ->
      if List.mem fig.members (x,y) then true
      else touch_rec drops x y rest
  in

  touch_rec 0 x y state.diff
;;


let get_solution state = 
  let out = ref [] in

  let append_diff = function 
    | Finish _ -> ()
    | LockedPlacement (_, m) ->
        out := "" :: (s_of_moves m) :: !out
    | LivePlacement (_, m) ->
        out := (s_of_moves m) :: !out
    | ColumnDrop col -> ()
  in

  List.iter ~f:append_diff (List.rev state.diff);
  String.concat (List.rev !out)
;;

let print_state state =

  let dbg_print_diff = function 
    | Finish _ -> printf "FIN!"
    | LockedPlacement (fig, m) -> printf "%s " (s_of_moves_dbg m)
    | LivePlacement (fig, m) -> printf "*%s " (s_of_moves_dbg m)
    | ColumnDrop col -> printf "(drop %d) " col
  in

  let normal_print_diff = function 
    | Finish _ -> ()
    | LockedPlacement (fig, m) -> printf "%s" (s_of_moves m)
    | LivePlacement (fig, m) -> printf "%s" (s_of_moves m)
    | ColumnDrop col -> ()
  in

  let print_diff = dbg_print_diff in

  printf "%d×%d ID=%d, all_figs=%d, remaining=%d, seed=%d\n" state.width state.height state.id (List.length state.figures) state.remaining state.initial_seed;
  for y = 0 to state.height - 1 do
    if (y % 2 = 1) then printf " ";
    for x = 0 to state.width - 1 do

      printf "%s" (if pt_solid true state (x,y) then "XX" else "··");

    done;
    printf "\n";
  done;
  printf "\n%!";

  List.iter ~f:print_diff (List.rev state.diff);
  (* printf "\n%s\n%!" (s_of_moves s.moves); *)
  state

;;


let fig_touches_something state pts = 
  List.exists pts ~f:(pt_solid false state)
;;

let pick_new_or_finalize state =
  if state.remaining = 0 then
    { state with diff = (Finish true) :: state.diff }
  else
    let n, next_seed = next_random state.seed in
    let n_fig = (n mod (List.length state.figures)) in
    let fig = List.nth_exn state.figures n_fig in
    (* novietojam figūru pareizajā centrā *)
    let xoffs, yoffs = initial_figure_offset state fig in
    (* apdeitojam koordinātes *)
    let moved_figure = offset_fig fig xoffs yoffs in

    if fig_touches_something state moved_figure.members then
      { state with diff = (Finish true) :: state.diff }
    else
      { state with seed = next_seed; remaining = state.remaining - 1; diff = (LivePlacement (moved_figure, [])) :: state.diff }
;;






let maybe_drop state =
  (* highly ineffective *)
  let new_diff = ref state.diff in
  for y = 0 to state.height - 1 do 
    let drop = ref true in
    for x = 0 to state.width - 1 do
      if not (pt_solid false state (x,y)) then drop := false
    done;
    if !drop then new_diff := (ColumnDrop y) :: !new_diff;
  done;
  { state with diff = !new_diff }




let process_live_placement state move (fig, moves) hashes diff =
  let translated_fig = moved_fig fig move in
  let fig_hash = figure_hash translated_fig in
  if List.mem !hashes fig_hash then None else (

    hashes := fig_hash :: !hashes;

    if fig_touches_something state translated_fig.members then (
      Some (( { state with diff = (LockedPlacement (fig, (move :: moves))) :: diff } |> maybe_drop |> pick_new_or_finalize ), false )

    ) else (
      Some (( { state with diff = (LivePlacement (translated_fig, (move :: moves) )) :: diff }), true)
    )
  )
;;



let rec next_states state hashes = match state.diff with
| []                                      -> [pick_new_or_finalize state]
| (Finish _)                      :: rest -> [state]
| (LockedPlacement _)             :: rest -> failwith "Unexpected LockedPlacement"
| (ColumnDrop _)                  :: rest -> failwith "Unexpected ColumnDrop"
| (LivePlacement (fig, p)) :: rest ->
    let out = ref [] in
    let had_lock = ref false in
    List.iter first_moves ~f:(
      fun move -> match process_live_placement state move (fig, p) hashes rest with
      | None -> ()
      | Some (s, true) ->
          (*
          printf "Hashes hashed: %d, shit gathered: %d\n" (List.length !hashes) (List.length !out);
          print_state s;*)
          out := List.append !out (next_states s hashes)
      | Some (s, false) ->
          (* printf "Booyaka booyaked\n";
          print_state s; *)
          if not !had_lock then out := s :: !out;
          had_lock := true
    );
    !out
;;










let json_cell json = Yojson.Basic.Util.(
  let x = json |> member "x" |> to_int
  and y = json |> member "y" |> to_int in
  x, y
)


let take_json_cell json field = Yojson.Basic.Util.(
  json |> member field |> json_cell
)


let take_json_cells json field = Yojson.Basic.Util.(
  let f = json |> member field |> to_list in
  List.map f ~f:json_cell
)


let make_field width height =
  Array.make_matrix height width false
;;



let make_filled_field width height json_cells =
  let f = make_field width height in

  let put_figure jsc =
    let x, y = json_cell jsc in
    f.(y).(x) <- true
  in

  List.iter json_cells ~f:put_figure;

  f
;;





let state_heuristic state =

  let totes = ref 0 in
  for y = 0 to state.height - 1 do
    for x = 0 to state.width - 1 do
      if pt_solid true state (x,y) then totes := !totes + y
    done;
  done;

  totes := !totes + 1000 * List.count state.diff ~f:(function
    | ColumnDrop _ -> true
    | _ -> false
  );

  (* print_state state |> ignore;
  printf "Heur %d\n" !totes |> ignore; *)
  !totes
;;


let take_some_states n (l:'a list) =

  if List.length l < n then l else (
    let heured = List.map l ~f:(fun a -> (state_heuristic a), a) in
    let sl = List.sort ~cmp:(fun (ha, a) (hb, b) -> hb - ha) heured in
    List.map (List.take sl n) ~f:snd
  )
;;



let make_figure json_fig =
  { members = take_json_cells json_fig "members"
  ; pivot = take_json_cell json_fig "pivot"
  }
;;


  (* atdod masīvu ar steitiem, atbilstošu source_seediem *)
let states_of_json json = Yojson.Basic.Util.(
  let w = json |> member "width" |> to_int
  and h = json |> member "height" |> to_int
  and id = json |> member "id" |> to_int
  and source_seeds = json |> member "sourceSeeds" |> to_list
  and source_length = json |> member "sourceLength" |> to_int
  and units = json |> member "units" |> to_list
  and filled = json |> member "filled" |> to_list in

  let base_state = {
    id = id;
    figures = List.map units ~f:make_figure;
    width = w;
    height = h;
    field = make_filled_field w h filled;
    remaining = source_length;
    initial_seed = 0;
    seed = 0;
    diff = [];
  } in

  List.map source_seeds ~f:(fun (seed) ->
    { base_state with 
      seed = to_int seed;
      initial_seed = to_int seed;
    }

  )
)


let first_state_of_json something = states_of_json something |> List.hd_exn

let ignored fn = fun x -> ignore(fn x)

let is_terminal_state state = match state.diff with
  | Finish _ :: _ -> true
  | _ -> false


let rec put_figure_on_board_and_go (states:state_t list) : state_t =

  let hashes = ref [] in

  let next_states = 
    List.fold ~init:[] states ~f:(fun accu state -> 
      List.append accu (next_states state hashes)
    )
  in
  let next_states = take_some_states 10 next_states in
  (match List.hd next_states with
  | None -> failwith "ok"
  | Some s ->
      (* print_state s; *)
      if not (is_terminal_state s) then 
        put_figure_on_board_and_go next_states
      else s
  )
;;


let solve state =
  eprintf "Solving seed %04x\n%!" state.initial_seed;
  (* ignore(print_state state); *)
  put_figure_on_board_and_go [ state ]
;;


