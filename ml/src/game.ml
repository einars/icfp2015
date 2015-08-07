(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Printf
open Core.Std


type field_t = bool array array

type cell_t = int * int

type figure_t =
  { members: cell_t list
  ; pivot: cell_t
  }



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
  ; seed: int
  ; remaining: int (* inv sourceLength *)
  ; current_fig: figure_t option
}


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


let make_figure json_fig =

  { members = take_json_cells json_fig "members"
  ; pivot = take_json_cell json_fig "pivot"
  }



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
    seed = 0;
  } in

  List.map source_seeds ~f:(fun (seed) ->
    { base_state with seed = Yojson.Basic.Util.to_int seed }
  )
;;


let first_state_of_json something = states_of_json something |> List.hd_exn

