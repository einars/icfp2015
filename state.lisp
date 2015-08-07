(defpackage :icfp/state
  (:nicknames :state)
  (:use :cl)
  (:export :*board-width* :*board-height*
	   :board :board-grid :board-pivot :board-active-cells
	   :make-pos :pos-x :pos-y
	   :empty-board))

(in-package :icfp/state)

(defvar *board-width*)
(defvar *board-height*)

(defstruct pos x y)

(defstruct board grid pivot active-cells)

(defun empty-board ()
  (let ((board (make-board)))
    (setf (board-grid board) (make-array (list *board-width* *board-height*))
	  (board-pivot board) nil
	  (board-active-cells board) nil)
    board))
