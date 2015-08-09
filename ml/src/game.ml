(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std
open Gametypes
open Movement


let s_of_pt pt = sprintf "[%d:%d]" (fst pt) (snd pt)


let m_se = MOVE_SE, "m"
let m_sw = MOVE_SW, "a"
let m_cw = TURN_CW, "q"
let m_cc = TURN_CCW, "k"
let m_ee = MOVE_E, "b"
let m_ww = MOVE_W, "p"

let default_moves = [  m_se; m_sw; m_cw; m_cc; m_ww; m_ee ]
let next_moves = function
  | MOVE_E   -> [ m_se ; m_sw ; m_cw ; m_cc ; m_ee        ]
  | MOVE_W   -> [ m_se ; m_sw ; m_cw ; m_cc ;        m_ww ]
  | MOVE_SE  -> [ m_se ; m_sw ; m_cw ; m_cc ; m_ee ; m_ww ]
  | MOVE_SW  -> [ m_se ; m_sw ; m_cw ; m_cc ; m_ee ; m_ww ]
  | TURN_CW  -> [ m_se ; m_sw ; m_cw ;        m_ee ; m_ww ]
  | TURN_CCW -> [ m_se ; m_sw ;        m_cc ; m_ee ; m_ww ]
  | NOP      -> default_moves

let s_of_moves moves =
  let mcs = List.rev_map moves ~f:(function
    | _, s -> s
  ) in
  String.concat mcs
;;

let blank_hash = Set.empty ~comparator:String.comparator

let ignored fn = fun x -> ignore(fn x)



type procesing_t = Finalizing of state_t | Running of state_t | Borkbork

(*
let s_of_moves_dbg moves =
  let mcs = List.rev_map moves ~f:(function
    | MOVE_W -> "<"
    | MOVE_E -> ">"
    | MOVE_SW -> "/"
    | MOVE_SE -> "\\"
    | TURN_CW -> "+"
    | TURN_CCW -> "-"
    | NOP -> " "
  ) in
  String.concat mcs
;;
*)

let is_terminal_state state = match state.diff with
  | Finish _ :: _ -> true
  | _ -> false
;;

let is_live_state state = match state.diff with
  | LivePlacement _ :: _ -> true
  | _ -> false
;;

let moved_fig_pos state = match state.diff with
  | (LivePlacement (fig, _)) :: _ -> fig.members
  | _ -> []
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

let offset_fig fig (xoffs,yoffs) =
  let px, py = fig.pivot in
  { members = List.map fig.members ~f:(fun (x,y) -> (x + xoffs), (y + yoffs) )
  ; pivot   = (px + xoffs), (py + yoffs)
  }
;;







let next_random seed =
  seed lsr 16 land 0x7fff, (seed * 0x41c64e6d + 12345) land 0xffffffff
;;


let pt_solid state (x,y) =
  if x < 0 || x >= state.width || y < 0 || y >= state.height
  then true
  else state.repr.(y * state.width + x)

let pt_solid_live state (x,y) =

  let check_live_block () = match state.diff with
    | LivePlacement (fig, _) :: _ -> List.mem fig.members (x,y)
    | _ -> false
  in

  if x < 0 || x >= state.width || y < 0 || y >= state.height
  then true
  else state.repr.(y * state.width + x) || check_live_block ()
;;


let get_solution state =
  let out = ref [] in

  let append_diff = function
    | Finish _ -> ()
    | LockedPlacement (_, m) ->
        out := (s_of_moves m) :: !out
    | LivePlacement (_, m) ->
        out := (s_of_moves m) :: !out
    | ColumnDrop col -> ()
  in

  List.iter ~f:append_diff (List.rev state.diff);
  String.concat (List.rev !out)
;;


let print_state state =

  (*
  let dbg_print_diff = function
    | Finish _ -> printf "EOF\n"
    | LockedPlacement (fig, m) -> printf "%s " (s_of_moves_dbg m)
    | LivePlacement (fig, m) -> printf "*%s " (s_of_moves_dbg m)
    | ColumnDrop col -> printf "(drop %d) " col
  in
  *)

  let normal_print_diff = function
    | Finish _ -> ()
    | LockedPlacement (fig, m) -> printf "%s" (s_of_moves m)
    | LivePlacement (fig, m) -> printf "%s" (s_of_moves m)
    | ColumnDrop col -> ()
  in

  let print_diff = normal_print_diff in

  printf "%d×%d ID=%d, all_figs=%d, remaining=%d, seed=%d\n" state.width state.height state.id (List.length state.figures) state.remaining state.initial_seed;
  for y = 0 to state.height - 1 do
    if (y % 2 = 1) then printf " ";
    for x = 0 to state.width - 1 do

      printf "%s" (if pt_solid_live state (x,y) then "O " else "· ");

    done;
    printf "\n";
  done;
  printf "Moves: ";

  List.iter ~f:print_diff (List.rev state.diff);
  printf "\n%!";
  state
;;


let fig_touches_something state pts =
  List.exists pts ~f:(pt_solid state)
;;


let pick_new_or_finalize state =
  if state.remaining = 0 then
    { state with diff = (Finish true) :: state.diff }
  else
    let n, next_seed = next_random state.seed in
    let n_fig = (n mod (List.length state.figures)) in
    let fig = List.nth_exn state.figures n_fig in

    if fig_touches_something state fig.members then
      { state with diff = (Finish true) :: state.diff }
    else
      { state with seed = next_seed; remaining = state.remaining - 1; diff = (LivePlacement (fig, [])) :: state.diff }
;;



let maybe_drop state =

  (* apply last locked figure *)
  let new_repr = Array.copy state.repr in
  begin match state.diff with
  | LockedPlacement (fig, _) :: _ -> List.iter fig.members ~f:(fun (x,y) -> new_repr.(y * state.width + x) <- true)
  | _ -> failwith "maybe_drop: unexpected last element"
  end;

  let tmp_state = { state with repr = new_repr } in
  let new_diff = ref state.diff in

  for y = 0 to state.height - 1 do
    let drop = ref true in
    for x = 0 to state.width - 1 do
      if not (pt_solid tmp_state (x,y)) then drop := false
    done;
    if !drop then begin
      new_diff := (ColumnDrop y) :: !new_diff;
      (* update actual drop *)
      for yy = y downto 1 do
        for x = 0 to state.width - 1 do
          new_repr.(yy * state.width + x) <- new_repr.((yy - 1) * state.width + x);
        done;
      done;
      for x = 0 to state.width - 1 do
        new_repr.(x) <- false;
      done;
    end
  done;


  { state with diff = !new_diff; repr = new_repr }
;;




let process_live_placement state (move,c) (fig, moves) hashes =

  let translated_fig = moved_fig fig move in
  let fig_hash = figure_hash translated_fig in
  if Set.mem !hashes fig_hash then None else (

    hashes := Set.add !hashes fig_hash;

    if fig_touches_something state translated_fig.members then (
      Some (state, false)
    ) else (
      Some (( { state with diff = (LivePlacement (translated_fig, ( (move,c) :: moves) )) :: (List.tl_exn state.diff) }), true)
    )
  )
;;


let initial_hash state =
  (* pick_new_or_finalize uzliek figūru, bet hašs netiek apdeitots *)
  begin match state.diff with
    | (LivePlacement (fig, _)) :: _ ->
        Set.add blank_hash (figure_hash fig)
    | _ -> blank_hash
  end
;;



let global_dupes = ref blank_hash
let state_hash state =
  let repr = Array.copy state.repr in
  List.iter (moved_fig_pos state) ~f:(fun (x,y) -> repr.(y * state.width + x) <- true);
  String.concat (List.map (Array.to_list repr) ~f:(fun x -> if x then "i" else "o"))
;;

let global_dup_removal = function
  | Finalizing st ->
      let hash = state_hash st in
      if Set.mem !global_dupes hash then Borkbork
      else (
        global_dupes := Set.add !global_dupes hash;
        Finalizing st
      )
  | foo -> foo
;;


let apply_move state (move:ext_move_t) ref_hashes =
  match state.diff with 
    | (LivePlacement (fig, moves)) :: _ ->
        if (fst move = NOP) then (
          (* tikai apdeito movu sarakstu ar nopu *)
          Running { state with diff = (LivePlacement (fig, ( move :: moves) )) :: (List.tl_exn state.diff) }
        ) else (
          let translated_fig = moved_fig fig (fst move) in
          let fig_hash = figure_hash translated_fig in
          if Set.mem !ref_hashes fig_hash then Borkbork else (
            ref_hashes := Set.add !ref_hashes fig_hash;
            if fig_touches_something state translated_fig.members then (
              (Finalizing state) (* |> global_dup_removal *)
            ) else (
              Running { state with diff = (LivePlacement (translated_fig, ( move :: moves) )) :: (List.tl_exn state.diff) }
            )
          )
        )

    | _ -> Borkbork
;;

let apply_power_word ?(debug=false) state word hashes =

  let ref_hashes = ref hashes in

  let rec apply_power_letter st = function
    | [] -> Some (st, !ref_hashes)
    | move :: rest -> (
      match  apply_move st move ref_hashes with
      | Borkbork -> None
      | Finalizing st -> None
      | Running st -> (if debug then ignore(print_state st)); apply_power_letter st rest
    )
  in

  apply_power_letter state word




let process_state ?(power_words=[]) state =
  (*
   * state ir starta state, bet viņai ir jābūt aktīvam LivePlacement - iesp., pirmā figūra
   * *)

  let source_pool = ref [ state,NOP ] in
  let target_pool = ref [] in
  let hashes = ref (initial_hash state) in

  (*
  print_state state;
  eprintf "process_state start\n%!";
  *)

  let iteration = ref 0 in
  while !source_pool <> [] do
    iteration := !iteration + 1;
    (* eprintf "process_state source_pool = %d\n%!" (List.length !source_pool); *)
    let pool = !source_pool in 
    source_pool := [];
    List.iter pool ~f:(fun (state,last_move) ->

      (* very rarely try using the words *)
      let unmodified_hash = !hashes in (* visi vārdi ies ar vienu un to pašu hašu, lai varētu iet paralēli *)

      List.iter (next_moves last_move) ~f:(fun m ->
        match apply_move state m hashes with
        | Finalizing s ->  target_pool := (s,m) :: !target_pool
        | Running s ->  source_pool := (s,(fst m)) :: !source_pool
        | Borkbork ->  ()
      );

      if (!iteration mod 5 = 1) then
      (* izejam cauri spēka vārdiem un piefiksējam labus variantus *)
      List.iter power_words ~f:(fun (full_w, w) ->
        match apply_power_word state w unmodified_hash with
        | None -> ()
        | Some (word, hash) ->
            (*
            printf "Applied %s\n%!" full_w;
            apply_power_word ~debug:true state w unmodified_hash;
            *)
          source_pool := (word,NOP) :: !source_pool;
          hashes := Set.union !hashes hash;
      );


    );

    (* List.iter !source_pool ~f:(ignored print_state); *)
    (* printf "\nHave %d in source pool\n%!" (List.length !source_pool); *)
  done;

  (* eprintf "process_state finished, target_pool = %d\n%!" (List.length !target_pool); *)

  !target_pool









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



let state_heuristic state =

  (* let is_live = is_live_state state in *)
  let moved_pos = moved_fig_pos state in


  let totes = ref 0 in
  (*

  let max_interesting_row = ref (state.height - 1) in
  for y = state.height - 1 downto 0 do
    let tot = ref 0 in
    for x = 0 to state.width - 1 do
      if pt_solid_live state (x,y) then tot := !tot + 1;
    done;
    if (!tot = state.width) then (
      totes := !totes + 1000;
      max_interesting_row := y - 1;
    )
  done;

  totes := !totes + 1000 * List.count state.diff ~f:(function
    | ColumnDrop _ -> true
    | _ -> false
  );




  let out_of_bounds (x,y) =
    x < 0 || y < 0 || x >= state.width || y >= !max_interesting_row
  in

  let strapon pt =
    let rec r_strapon pt penalize accum func = 
      if out_of_bounds pt then accum else
      if pt_solid_live state pt 
      then r_strapon (func pt) true accum func
      else r_strapon (func pt) false (if penalize then accum + 1 else accum) func
    in
      + 3 * r_strapon pt false 0 move_se
  in


  let penalty = ref 0 in
  for x = 0 to state.width - 1 do
    penalty := !penalty + strapon (x,0)
  done;
  for y = 0 to !max_interesting_row do
    penalty := !penalty + strapon (0,y)
  done;

  totes := !totes - !penalty;


  *)
  let solid = pt_solid_live state
  and free = (fun pt -> not (pt_solid_live state pt)) in


  let max_y = ref (0) in
  List.iter moved_pos  ~f:(
    fun (x,y) ->
      let pt = x,y in

      let sw = move_sw pt and se = move_se pt in
      let swsw = move_sw sw and sese = move_se se in

      (*
      if y > !max_y then max_y := y;
      *)
      totes := !totes + 2 * y;

      if (free ((x - 1),y)) && solid ((x - 2),y) then totes := !totes - 1;
      if (free ((x + 1),y)) && solid ((x + 2),y) then totes := !totes - 1;

      if (free sw) && (solid (move_e sw)) && (solid (move_w sw)) then totes := !totes - 2;
      if (free se) && (solid (move_e se)) && (solid (move_w se)) then totes := !totes - 2;

      if (free sw) && (free se) then totes := !totes - 2;

      if (free sw) && (free swsw) && (solid (move_e sw)) && (solid (move_w sw))
      &&                             (solid (move_e swsw)) && (solid (move_w swsw))
      then totes := !totes - 4;

      if (free se) && (free sese) && (solid (move_e se)) && (solid (move_w se))
      &&                             (solid (move_e sese)) && (solid (move_w sese))
      then totes := !totes - 4;

  );
  totes := !totes + !max_y;


  (*

  List.iter moved_pos  ~f:(
    fun pt ->
      (*
      if (not (pt_solid_live state (move_nw pt))) then totes := !totes -1;
      if (not (pt_solid_live state (move_ne pt))) then totes := !totes -1;
      *)

      if (not (pt_solid_live state (move_sw pt))) then totes := !totes -4;
      if (not (pt_solid_live state (move_se pt))) then totes := !totes -4;
      if (not (pt_solid_live state (move_e pt))) then totes := !totes -2;
      if (not (pt_solid_live state (move_w pt))) then totes := !totes -2;
      totes := !totes + 5 * (snd pt)
  );
  *)

  (*
  print_state state |> ignore;
  printf "\nHeur %d\n" !totes |> ignore;
  *)
  !totes
;;


let take_some_states n (l:'a list) =

  if List.length l < n then l else (
    let heured = List.map l ~f:(fun (s, m) -> (state_heuristic s), s, m) in
    let sl = List.sort ~cmp:(fun (h1, s1, m1) (h2, s2, m2) -> h2 - h1) heured in
    List.map (List.take sl n) ~f:(fun (a,b,c) -> b,c)
  )
;;



let make_figure json_fig =
  { members = take_json_cells json_fig "members"
  ; pivot = take_json_cell json_fig "pivot"
  }
;;

let make_initial_repr w h filled =
  let repr = Array.create (w * h) false in
  List.iter filled ~f:(fun jsc ->
    let x,y = json_cell jsc in
    repr.(y * w + x) <- true
  );
  repr
;;

let readjust_figures state =

  let readjust_figure fig =
    offset_fig fig (initial_figure_offset state fig)
  in


  { state with figures = List.map state.figures ~f:readjust_figure }


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
    remaining = source_length;
    sourcelength = source_length;
    initial_seed = 0;
    seed = 0;
    diff = [];
    repr = make_initial_repr w h filled
  } |> readjust_figures  in

  List.map source_seeds ~f:(fun (seed) ->
    { base_state with
      seed = to_int seed;
      initial_seed = to_int seed;
    }

  )
)


let first_state_of_json something = states_of_json something |> List.hd_exn

let lock_with_move ((state:state_t),(move:ext_move_t)) =
  if fst move = NOP then state
  else match state.diff with
  | (LivePlacement (fig, moves) :: rest) ->
      { state with diff = (LockedPlacement (fig, (move :: moves))) :: rest } |> maybe_drop |> pick_new_or_finalize
  | _ -> failwith "lock_with_move, unexpected diff/state"
;;


let rec put_figure_on_board_and_go depth power_words (states:state_t list) : state_t =

  let all_finals_to_consider = 
    List.fold states ~init:[] ~f:(fun accum state -> List.append accum (process_state ~power_words state ))
  in

  (* List.iter all_finals_to_consider ~f:(fun (s,m) -> ignore(print_state s)); *)

  let best_finals = take_some_states 10 all_finals_to_consider in
  let next_states = List.map best_finals ~f:lock_with_move in

  (match List.hd next_states with
  | None -> failwith "ok"
  | Some s ->
      if s.sourcelength <> 100 
      then eprintf "\r%3d%% %d / %d%!" (depth * 100 / s.sourcelength) depth s.sourcelength
      else eprintf "\r%3d%%%!" depth;

      (* print_state s; *)
      if not (is_terminal_state s) then
        put_figure_on_board_and_go (succ depth) power_words next_states
      else (
        eprintf "\r                              \n";
        s
      )
  )
;;


let move_of_char c = match c with
  | 'p' | '\''| '!' | '.' | '0' | '3' -> MOVE_W, String.make 1 c
  | 'b' | 'c' | 'e' | 'f' | 'y' | '2' -> MOVE_E, String.make 1 c
  | 'a' | 'g' | 'h' | 'i' | 'j' | '4' -> MOVE_SW, String.make 1 c
  | 'l' | 'm' | 'n' | 'o' | ' ' | '5' -> MOVE_SE, String.make 1 c
  | 'd' | 'q' | 'r' | 'v' | 'z' | '1' -> TURN_CW, String.make 1 c
  | 'k' | 's' | 't' | 'u' | 'w' | 'x' -> TURN_CCW, String.make 1 c
  | _ -> NOP, String.make 1 c
;;

let process_power_word word = 
  word, List.map ~f:move_of_char (List.rev (String.to_list_rev (String.lowercase word)))
;;

let process_power_words words = List.map words ~f:process_power_word
;;


let solve ?(power_words=[]) state =
  eprintf "Solving seed %04x\n%!" state.initial_seed;
  (* ignore(print_state state); *)
  put_figure_on_board_and_go 1 (process_power_words power_words) [ state |> pick_new_or_finalize ]
;;


