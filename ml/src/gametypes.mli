type cell_t = int * int

type figure_t =
  { members: cell_t list
  ; perimeter: cell_t list
  ; pivot: cell_t
  }

type move_t = MOVE_E | MOVE_W | MOVE_SE | MOVE_SW | TURN_CW | TURN_CCW | NOP

type ext_move_t = move_t * string

type diff_t = LivePlacement of figure_t * ext_move_t list
  | LockedPlacement of figure_t * ext_move_t list
  | ColumnDrop of int
  | Finish of bool


type state_t =
  { id: int
  ; figures: figure_t list
  ; width: int
  ; height: int
  ; initial_seed: int
  ; seed: int
  ; remaining: int (* inv sourceLength *)
  ; sourcelength: int
  ; diff: diff_t list
  ; repr: bool array
  ; height_hint: int
  ; death_points: cell_t list
}

