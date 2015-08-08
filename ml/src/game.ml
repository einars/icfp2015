(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std


type field_t = bool array array

type cell_t = int * int

type figure_t =
  { members: cell_t list
  ; pivot: cell_t
  }

type move_t = MOVE_E | MOVE_W | MOVE_SE | MOVE_SW | TURN_CW | TURN_CCW | LOCK_MARK

let first_moves = 
  [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW ; TURN_CCW ]

let next_moves = function
  (* | MOVE_E ->  [ MOVE_E ;          MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | MOVE_W ->  [          MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | MOVE_SE -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | MOVE_SW -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | TURN_CW -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | TURN_CCW-> [ MOVE_W ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ] *)
  | MOVE_E ->  [ MOVE_E ;          MOVE_SE ; MOVE_SW ; TURN_CW  ]
  | MOVE_W ->  [          MOVE_W ; MOVE_SE ; MOVE_SW ;            TURN_CCW ]
  | MOVE_SE -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | MOVE_SW -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | TURN_CW -> [ MOVE_E ;          MOVE_SE ; MOVE_SW ; TURN_CW  ;          ]
  | TURN_CCW-> [          MOVE_W ; MOVE_SE ; MOVE_SW ;            TURN_CCW ]
  | LOCK_MARK -> []

let s_of_pt pt = sprintf "[%d:%d]" (fst pt) (snd pt)

(*

a.(0) - rindiņas
a.(0).(1) - kolonnas

 * *)


type state_t =
  { id: int
  ; figures: figure_t list
  ; width: int
  ; height: int
  ; field: field_t
  ; initial_seed: int
  ; seed: int
  ; remaining: int (* inv sourceLength *)
  ; current_fig: figure_t option
  ; current_fig_offs: int*int
  ; moves: move_t list
  ; base_hash: string
  (* score stuff *)
  ; score: int
  ; score_adj: int
  ; ls_old: int
}

exception Locked of state_t
exception Finished of state_t
exception Impossible

let mut_drop_full_lines state =
  (* and update score *)
  let s_size = state.score_adj
  and s_ls = ref 0
  and s_ls_old = state.ls_old in

  for i = state.height - 1 downto 0 do
    if not (Array.exists state.field.(i) ~f:(fun e -> not e)) then begin
      s_ls := !s_ls + 1;
      for j = i downto 1 do
        state.field.(j) <- state.field.(j - 1);
      done;
      state.field.(0) <- Array.init state.width (fun n -> false)
    end;
  done;
  let s_points = (s_size + 100 * (1 + !s_ls) * (!s_ls) / 2) in
  { state with
    score = state.score + s_points + (if s_ls_old > 1 then ((s_ls_old - 1) * s_points / 10) else 0);
    ls_old = !s_ls
  }
;;

let with_base_hash state =
  let h = ref [] in
  for i = 0 to state.height - 1 do
    for j = 0 to state.width - 1 do
      h := (if state.field.(i).(j) then "X" else "-") :: !h
    done;
  done;
  { state with base_hash = String.concat !h }
;;



let freeze_figure state =

  let clone_field field =
    Array.init (Array.length field) (fun i -> Array.copy field.(i))
  in


  let new_field = clone_field state.field in
  let ox, oy = state.current_fig_offs in
  let score_adj = 
  (match state.current_fig with
    | Some fig ->
        List.iter fig.members ~f:(fun (x, y) -> new_field.(y + oy).(x + ox) <- true);
        List.length fig.members;
    | None -> 0
  ) in

  { state with
    current_fig = None;
    field = new_field;
    score_adj = score_adj;
  } |> with_base_hash
;;


let s_of_moves moves = 
  let mcs = List.map moves ~f:(function 
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



let print_state state =

  let s = freeze_figure state in
  
  printf "%d×%d ID=%d, all_figs=%d, remaining=%d, seed=%d score=%d\n" s.width s.height s.id (List.length s.figures) s.remaining s.initial_seed s.score;
  for row = 0 to s.height - 1 do
    if (row % 2 = 1) then printf " ";
    for col = 0 to s.width - 1 do

      printf "%s" (if s.field.(row).(col) then "XX" else "··");

    done;
    printf "\n";
  done;
  printf "\n%!";
  (* printf "\n%s\n%!" (s_of_moves s.moves); *)
  state

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


let next_random seed =
  seed lsr 16 land 0x7fff, (seed * 0x41c64e6d + 12345) land 0xffffffff
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



let check_fig_placement state fig xoffs yoffs =
  
  let check_fig_point (x,y) =

    let sy = y + yoffs
    and sx = x + xoffs in

    (* out of bounds *)
    if sy < 0 then raise (Locked state);
    if sx < 0 then raise (Locked state);
    if sx >= state.width then raise (Locked state);
    if sy >= state.height then raise (Locked state);
    if state.field.(sy).(sx) then raise (Locked state);
  in

  List.iter fig.members ~f:check_fig_point
;;


let offset_fig fig xoffs yoffs =
  let px, py = fig.pivot in
  { members = List.map fig.members ~f:(fun (x,y) -> (x + xoffs), (y + yoffs) )
  ; pivot   = (px + xoffs), (py + yoffs)
  }
;;


let validate_figure_state st =
  match st.current_fig with
  | Some fig ->
    List.iter fig.members ~f:(fun (x,y) ->
      if y < 0 then raise (Locked st);
      if x < 0 then raise (Locked st);
      if x >= st.width then raise (Locked st);
      if y >= st.height then raise (Locked st);
      if st.field.(y).(x) then raise (Locked st);
    );
    st
  | None -> st
;;


let initially_place_figure state fig  =
  let xoffs, yoffs = initial_figure_offset state fig in
  let moved_figure = offset_fig fig xoffs yoffs in
  let st = { state with current_fig = Some moved_figure } in
  validate_figure_state st
;;


let move_e (x,y) = x + 1, y
let move_w (x,y) = x - 1, y
let move_sw (x,y) = x + (if y % 2 = 1 then 0 else -1), y + 1
let move_se (x,y) = x + (if y % 2 = 1 then  1 else 0), y + 1

let turn_cw (px, py) (x, y) =

  (* pivot 2d to cube *)
  let xxpiv = px - (py - py % 2) / 2 in
  let zzpiv = py in
  let yypiv = - xxpiv - zzpiv in
  
  (* point 2d to cube *)
  let xx = x - (y - y % 2) / 2 in
  let zz = y in
  let yy = - xx - zz in

  (* adjust pivot *)
  let xx, yy, zz = xx - xxpiv, yy - yypiv, zz - zzpiv in

  (* rotate *)
  let xx, yy, zz = -zz, -xx, -yy in

  (* readjust pivot *)
  let xx, yy, zz = xx + xxpiv, yy + yypiv, zz + zzpiv in

  (xx + (zz - zz % 2) / 2), zz
;;

let turn_ccw (px, py) (x, y) =

  (* pivot 2d to cube *)
  let xxpiv = px - (py - py % 2) / 2 in
  let zzpiv = py in
  let yypiv = - xxpiv - zzpiv in

  (* point 2d to cube *)
  let xx = x - (y - y % 2) / 2 in
  let zz = y in
  let yy = - xx - zz in

  (* adjust pivot *)
  let xx, yy, zz = xx - xxpiv, yy - yypiv, zz - zzpiv in

  (* rotate *)
  let xx, yy, zz = -yy, -zz, -xx in

  (* readjust pivot *)
  let xx, yy, zz = xx + xxpiv, yy + yypiv, zz + zzpiv in

  (xx + (zz - zz % 2) / 2), zz
;;


let moved_fig fig = function
  | MOVE_E ->
      { pivot = move_e fig.pivot
      ; members = List.map fig.members ~f:move_e
      }
  | MOVE_W ->
      { pivot = move_w fig.pivot
      ; members = List.map fig.members ~f:move_w
      }
  | MOVE_SE ->
      { pivot = move_se fig.pivot
      ; members = List.map fig.members ~f:move_se
      }
  | MOVE_SW ->
      { pivot = move_sw fig.pivot
      ; members = List.map fig.members ~f:move_sw
      }
  | TURN_CW ->
      { pivot = fig.pivot
      ; members = List.map fig.members ~f:(fun pt -> turn_cw fig.pivot pt)
      }
  | TURN_CCW ->
      { pivot = fig.pivot
      ; members = List.map fig.members ~f:(fun pt -> turn_ccw fig.pivot pt)
      }
  | LOCK_MARK -> failwith "Unexpected LOCK_MARK"


let apply_move state (move:move_t) =
  match state.current_fig with
  | None -> raise Impossible
  | Some fig ->
    let s = { state with current_fig = Some (moved_fig fig move) } in
    validate_figure_state s
;;




let move_score = function
  | MOVE_E -> 0
  | MOVE_W -> 0
  | MOVE_SW | MOVE_SE -> 0
  | TURN_CW | TURN_CCW -> 0
  | LOCK_MARK -> 0
;;


let state_full_lines state =
  let is_full_line row = not (Array.exists row ~f:(fun e -> not e)) in
  Array.count state.field ~f:is_full_line
;;

let state_has_full_lines st = state_full_lines st > 0


let state_heuristic state moves =

  (*
  let run_bonus pt = 

    let rec n_consecutive (x,y) diff =
      if x < 0 || x >= state.width || (not state.field.(y).(x)) then 0 else
        1 + n_consecutive (x + diff, y) diff
    in

    (n_consecutive pt (-1)) + (n_consecutive pt 1)
  in
  *)

  let hole_penalty pt all_pts = 
    let is_solid (x,y) =
      if x < 0 || x >= state.width || y < 0 || y >= state.height then true else
        if state.field.(y).(x) then true
        else List.mem all_pts (x,y)
    in

    (if move_sw pt |> is_solid then 1 else 0) + (if move_se pt |> is_solid then 1 else 0)
  in

  let is_locked = match moves with
  | LOCK_MARK :: _ -> true
  | _ -> false 
  in


  let figure_score = function
  | None -> 0
  | Some fig -> 
      let ax, ay, bx, by = figure_bounds fig in

      (* the lower it fell, the better *)
      (* let height_penalty = (by + 1) in *)
      let height_penalty = ay in (* List.fold fig.members ~init:0 ~f:(fun accu (x,y) -> accu + y) in *)

  (*
      let run_bonus = 
        if is_locked then (
          List.fold fig.members ~init:0 ~f:(fun accum pt -> accum + run_bonus pt)
        ) else 0
      in
      *)
      let hole_penalty = 
        if is_locked then (
          List.fold fig.members ~init:0 ~f:(fun accum pt -> accum + hole_penalty pt fig.members);
        ) else 0
      in
      - height_penalty (*+ run_bonus / 5*) - hole_penalty
      (* List.fold fig.members ~init:0 ~f:(fun accu (x,y) -> accu - y) *)
  in

  (* List.fold moves ~init:0 ~f:(fun accum move -> accum + move_score move) + *)
  (500 * (state_full_lines state))
  + figure_score state.current_fig
;;


let take_some_states n (l:'a list) =

  if List.length l < n then l else (
    let heured = List.map l ~f:(fun (a,m) -> (state_heuristic a m), a, m) in
    let sl = List.sort ~cmp:(fun (ha, a, m) (hb, b, mb) -> ha - hb) heured in
    List.map (List.take sl n) ~f:(fun (a,b,c) -> (b,c))
  )
;;


let state_hash state =

  let compare_pts (ax,ay) (bx,by) = 
    if ax = bx then by - ay else bx - ax
  in
  
  ( match state.current_fig with
  | Some fig ->
    let h = ref [state.base_hash] in
    List.iter (List.sort ~cmp:compare_pts fig.members) ~f:(fun m -> h := (s_of_pt m) :: !h);
    String.concat !h
  | None -> state.base_hash
  );
;;
  



let pick_best_move states =

  let seen = ref (Set.empty ~comparator:String.comparator) in
  let pool = ref []
  and final_pool = ref [] in

  let add_to_final_pool state moves =
    (* let debug_m = s_of_moves (List.rev moves) in
    if (debug_m = "bbqbmmb\\t") then ignore(print_state state); *)
    final_pool := (state, moves) :: !final_pool;
  in

  let add_to_pool state moves =

    (*
    let debug_m = s_of_moves (List.rev moves) in
    if (debug_m = "b") then (
      ignore(print_state state);
      raise Impossible;
    );
    *)

    let hash = state_hash state in
    if not (Set.mem !seen hash) then (
      seen := Set.add !seen hash;
      pool := (state, moves) :: !pool;
    )
  in

  let rec consider_moves state moves_so_far =
    let consider_move move =
      try 
        let next_state = apply_move state move in
        add_to_pool next_state (move :: moves_so_far)
      with
        | Locked _ -> add_to_final_pool state (LOCK_MARK :: move :: moves_so_far)
    in
    List.iter ~f:consider_move
  in

  let rec move_ya () =
    let old_pool = !pool in
    pool := [];
    (* printf "pool: %d\n%!" (List.length old_pool); *)
    List.iter (take_some_states 100 old_pool) ~f:(fun (s, m) -> consider_moves s m (next_moves (List.hd_exn m)));
    if !pool <> [] then move_ya()
    else (
      List.map (take_some_states 100 !final_pool) ~f:(fun (st, fig_moves) ->
        let s = st |> freeze_figure in
        mut_drop_full_lines { s with moves = List.append s.moves (List.rev fig_moves) }
      )
    )
  in

  List.iter states ~f:(fun st -> consider_moves st [] first_moves);

  move_ya()
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
    current_fig = None;
    initial_seed = 0;
    seed = 0;
    current_fig_offs = 0,0;
    moves = [];
    base_hash = "";
    score = 0;
    score_adj = 0;
    ls_old = 0;
  } in

  List.map source_seeds ~f:(fun (seed) ->
    { base_state with 
      seed = to_int seed;
      initial_seed = to_int seed;
    }

  )
)


let first_state_of_json something = states_of_json something |> List.hd_exn

let rec put_figure_on_board_and_go (sts:state_t list) : state_t =
  let process_single_state st =
    let st = with_base_hash st in
    if st.remaining = 0 then raise (Finished st);
    let n, next_seed = next_random st.seed in
    let n_fig = (n mod (List.length st.figures)) in
    let fig = List.nth_exn st.figures n_fig in

    initially_place_figure { st with
      seed = next_seed;
      remaining = st.remaining - 1
    } fig
  in
  try (

    let last_lock = ref None in
    let good = ref [] in
    List.iter sts ~f:(fun s ->
      try good := (process_single_state s) :: !good;
      with Locked l -> last_lock := Some s;
    );

    if !good <> [] then (
      !good |> pick_best_move |> put_figure_on_board_and_go;
    ) else (
      match !last_lock with
      | None -> raise Impossible
      | Some state -> state
    )
  )
  with Finished state -> state
;;

let solve state =
  eprintf "Solving seed %08x\n%!" state.initial_seed;
  (* ignore(print_state state); *)
  put_figure_on_board_and_go [ state ]
;;


