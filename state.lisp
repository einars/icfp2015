(defpackage :icfp/state
  (:nicknames :state)
  (:use :cl)
  (:export :*board-width* :*board-height* :*total-moves*
	   :board :board-grid :board-pivot :board-active-cells
	   :make-pos :pos-x :pos-y :pos-add :pos-sub
	   :make-piece :copy-piece :piece-pivot :piece-offset
	   :piece-turn :piece-number :piece-config
	   :empty-grid :empty-board :copy-board))

(in-package :icfp/state)

(defvar *board-width*)
(defvar *board-height*)
(defvar *total-moves*)

(defstruct pos x y)

(defun pos-add (a b)
  (make-pos :x (+ (pos-x a) (pos-x b))
	    :y (+ (pos-y a) (pos-y b))))

(defun pos-sub (a b)
  (make-pos :x (- (pos-x a) (pos-x b))
	    :y (- (pos-y a) (pos-y b))))

(defstruct piece pivot config number offset (turn 0))

(defstruct board grid pivot active-cells)

(defun empty-grid ()
  (make-array (list *board-width* *board-height*)))

(defun empty-board ()
  (make-board :grid (empty-grid)))
