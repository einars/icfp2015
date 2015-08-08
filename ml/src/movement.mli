open Gametypes

val move_e: int * int -> int * int
val move_w: int * int -> int * int
val move_sw: int * int -> int * int
val move_se: int * int -> int * int

val turn_cw: int*int -> int*int -> int*int
val turn_ccw: int*int -> int*int -> int*int

val moved_fig: figure_t -> move_t -> figure_t
