open Yojson

type field_t = bool array array

type cell_t = int * int

type figure_t =
  { members: cell_t list
  ; pivot: cell_t
  }

type move_t = MOVE_E | MOVE_W | MOVE_SE | MOVE_SW | TURN_CW | TURN_CCW

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


val figure_bounds: figure_t -> int*int*int*int
val initial_figure_offset: state_t -> figure_t -> int * int
val next_random: int -> int * int

val put_figure_on_board_and_go: state_t -> state_t
val print_state: state_t -> unit

val first_state_of_json: Basic.json -> state_t
val states_of_json: Basic.json -> state_t list

val turn_cw: int*int -> int*int -> int*int
val turn_ccw: int*int -> int*int -> int*int

val s_of_moves: move_t list -> string
