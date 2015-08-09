(* vim: set ts=2 tw=0 foldmethod=marker : *)

open Core.Std
open Gametypes

let move_e (x,y) = x + 1, y
let move_w (x,y) = x - 1, y
let move_sw (x,y) = x + (if y % 2 = 1 then 0 else -1), y + 1
let move_se (x,y) = x + (if y % 2 = 1 then  1 else 0), y + 1

let move_nw (x,y) = x + (if y % 2 = 1 then 0 else -1), y - 1
let move_ne (x,y) = x + (if y % 2 = 1 then  1 else 0), y - 1

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
  | NOP -> failwith "Unexpected NOP"


