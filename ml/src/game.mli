open Yojson
open Gametypes

val figure_bounds: figure_t -> int*int*int*int
val initial_figure_offset: state_t -> figure_t -> int * int
val next_random: int -> int * int

val solve: ?power_words:string list -> state_t -> state_t
val print_state: state_t -> state_t

val first_state_of_json: Basic.json -> state_t
val states_of_json: Basic.json -> state_t list

val get_solution: state_t -> string

val pt_solid : state_t -> int * int -> bool
val maybe_drop : state_t -> state_t
