type field_t = bool array array

type cell_t = int * int

type figure_t =
  { members: cell_t list
  ; pivot: cell_t
  }

type move_t = MOVE_E | MOVE_W | MOVE_SE | MOVE_SW | TURN_CW | TURN_CCW | LOCK_MARK

type diff_t = LivePlacement of figure_t * move_t list * string list (* moves, normalized_masks *)
  | LockedPlacement of figure_t * move_t list
  | ColumnDrop of int
  | Finish of bool


type state_t =
  { id: int
  ; figures: figure_t list
  ; width: int
  ; height: int
  ; field: field_t
  ; initial_seed: int
  ; seed: int
  ; remaining: int (* inv sourceLength *)
  ; diff: diff_t list
}

