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
  | MOVE_E ->  [ MOVE_E ;          MOVE_SE ; MOVE_SW ; TURN_CW             ]
  | MOVE_W ->  [          MOVE_W ; MOVE_SE ; MOVE_SW            ; TURN_CCW ]
  | MOVE_SE -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | MOVE_SW -> [ MOVE_E ; MOVE_W ; MOVE_SE ; MOVE_SW ; TURN_CW  ; TURN_CCW ]
  | TURN_CW -> [ MOVE_E ;          MOVE_SE ; MOVE_SW                       ]
  | TURN_CCW-> [          MOVE_W ; MOVE_SE ; MOVE_SW                       ]
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
}

exception Locked of state_t
exception Impossible

let mut_drop_full_lines state =
  for i = state.height - 1 downto 0 do
    if not (Array.exists state.field.(i) ~f:(fun e -> not e)) then begin
      for j = i downto 1 do
        state.field.(j) <- state.field.(j - 1);
      done;
      state.field.(0) <- Array.init state.width (fun n -> false)
    end;
  done;
  state
;;

let freeze_figure state =

  let clone_field field =
    Array.init (Array.length field) (fun i -> Array.copy field.(i))
  in


  let new_field = clone_field state.field in
  let ox, oy = state.current_fig_offs in
  (match state.current_fig with
    | Some fig -> 
        List.iter fig.members ~f:(fun (x, y) -> new_field.(y + oy).(x + ox) <- true);
    | None -> ()
  );

  { state with
    current_fig = None;
    field = new_field
  }
;;


let s_of_moves moves = 
  let mcs = List.map moves ~f:(function 
    | MOVE_W -> "p"
    | MOVE_E -> "b"
    | MOVE_SW -> "a"
    | MOVE_SE -> "m"
    | TURN_CW -> "q"
    | TURN_CCW -> "k"
    (* | LOCK_MARK -> "\\t" *)
    | LOCK_MARK -> ""
  ) in
  String.concat mcs
;;



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
  printf "\n%s\n%!" (s_of_moves s.moves);
  state

;;






let json_cell json =
  let open Yojson.Basic.Util in
  let x = json |> member "x" |> to_int
  and y = json |> member "y" |> to_int in
  x, y


let take_json_cell json field =
  let open Yojson.Basic.Util in
  json |> member field |> json_cell


let take_json_cells json field =
  let open Yojson.Basic.Util in
  let f = json |> member field |> to_list in
  List.map f ~f:json_cell


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
  let xoffs = (state.width - width) / 2 in
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
  | MOVE_E -> 10
  | MOVE_W -> 10
  | MOVE_SW | MOVE_SE -> 30
  | TURN_CW | TURN_CCW -> 1
  | LOCK_MARK -> 0
;;

let state_has_full_lines state =
  let is_full_line row = not (Array.exists row ~f:(fun e -> not e)) in
  Array.exists state.field ~f:is_full_line
;;

let state_heuristic state moves =

  let figure_score = function
  | None -> 0
  | Some fig -> List.fold fig.members ~init:0 ~f:(fun accu (x,y) -> accu - y)
  in

  List.fold moves ~init:0 ~f:(fun accum move -> accum + move_score move)
  + (if state_has_full_lines state then 500 else 0)
  + figure_score state.current_fig
;;

let take_some_states n (l:'a list) =

  let sl = List.sort ~cmp:(fun (a, m) (b, mb) -> (state_heuristic a m) - (state_heuristic b m)) l in
  List.take sl n
;;


let state_hash state =
  let h = ref [] in
  for i = 0 to state.height - 1 do
    for j = 0 to state.width - 1 do
      h := (if state.field.(i).(j) then "X" else "-") :: !h
    done;
  done;
  ( match state.current_fig with
  | Some fig -> List.iter fig.members ~f:(fun m -> h := (s_of_pt m) :: !h)
  | None -> ()
  );
  String.concat !h
;;
  



let pick_best_move state =



  let seen = ref (Set.empty ~comparator:String.comparator) in
  let pool = ref []
  and final_pool = ref [] in

  let add_to_final_pool state moves =
    final_pool := (state, moves) :: !final_pool;
  in

  let add_to_pool state moves =
    let hash = state_hash state in
    if not (Set.mem !seen hash) then (
      seen := Set.add !seen hash;
      pool := (state, moves) :: !pool;
    )
  in

  let rec consider_moves state moves_so_far moves =
    let consider_move move =
      try 
        let next_state = apply_move state move in
        add_to_pool next_state (move :: moves_so_far)
      with
        | Locked _ ->
            add_to_final_pool state (LOCK_MARK :: move :: moves_so_far)
    in
    List.iter moves ~f:consider_move
  in

  let rec move_ya () =
    let old_pool = !pool in
    pool := [];
    List.iter (take_some_states 100 old_pool) ~f:(fun (s, m) -> consider_moves s m (next_moves (List.hd_exn m)));
    if !pool <> [] then move_ya()
    else match (take_some_states 1 !final_pool) with
    | [] -> None
    | foo :: _ -> Some foo


  in

  consider_moves state [] first_moves;
  match move_ya () with
  | Some (st, fig_moves) -> let s = st |> freeze_figure in
  mut_drop_full_lines { s with moves = List.append s.moves (List.rev fig_moves) }
  | None -> raise Impossible
;;



let make_figure json_fig =
  { members = take_json_cells json_fig "members"
  ; pivot = take_json_cell json_fig "pivot"
  }
;;


  (* atdod masīvu ar steitiem, atbilstošu source_seediem *)
let states_of_json json =
  let open Yojson.Basic.Util in
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
    moves = []
  } in

  List.map source_seeds ~f:(fun (seed) ->
    { base_state with 
      seed = Yojson.Basic.Util.to_int seed;
      initial_seed = Yojson.Basic.Util.to_int seed;
    }

  )
;;


let first_state_of_json something = states_of_json something |> List.hd_exn

let rec put_figure_on_board_and_go st =
  let n, next_seed = next_random st.seed in
  let fig = List.nth_exn st.figures (n mod (List.length st.figures)) in
  if st.remaining = 0 then raise (Locked st);

  let st = { st with
    seed = next_seed;
    remaining = st.remaining - 1
  } in
  try
    initially_place_figure st fig |> pick_best_move |> put_figure_on_board_and_go
  with Locked state -> state
;;


