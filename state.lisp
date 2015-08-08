(defpackage :icfp/state
  (:nicknames :state)
  (:use :cl)
  (:export :*board-width* :*board-height* :*total-moves* :last-move
	   :board :board-grid :board-pivot :board-active-cells :board-pieces
	   :make-pos :pos-x :pos-y :pos-add :pos-sub :adjust-pos :copy-pos
	   :make-piece :copy-piece :piece-pivot :piece-offset :active-cells
	   :piece-turn :piece-number :piece-config
	   :empty-grid :empty-board :copy-board
	   :board-update :board-update-board :board-update-msg
	   :continue-processing))

(in-package :icfp/state)

(defvar *board-width*)
(defvar *board-height*)
(defvar *total-moves*)

(define-condition board-update ()
  ((new-board :initarg :new-board
	      :reader board-update-board)
   (debug-msg :initarg :debug-msg
	      :reader board-update-msg
	      :initform "")))

(defstruct pos x y)

(defun pos-add (a b)
  (make-pos :x (+ (pos-x a) (pos-x b))
	    :y (+ (pos-y a) (pos-y b))))

(defun pos-sub (a b)
  (make-pos :x (- (pos-x a) (pos-x b))
	    :y (- (pos-y a) (pos-y b))))

(defstruct piece pivot config offset (turn 0) (number 0))

(defstruct board grid pieces)

(defun last-move (board)
  (first (board-pieces board)))

(defun convert-pos (pos)
  (cons (pos-x pos) (pos-y pos)))

(defun adjust-pos (pos offset)
  (let ((result (pos-add pos offset)))
    (when (and (oddp (pos-y offset)) (oddp (pos-y pos))) (incf (pos-x result)))
    (convert-pos result)))

(defun board-pivot (board)
  (let ((last (last-move board)))
    (adjust-pos (piece-pivot last) (piece-offset last))))

(defun active-cells (piece)
  (mapcar (lambda (x) (adjust-pos x (piece-offset piece)))
	  (aref (piece-config piece) (piece-turn piece))))

(defun board-active-cells (board)
  (active-cells (last-move board)))

(defun empty-grid ()
  (make-array (list *board-width* *board-height*)))

(defun empty-board ()
  (make-board :grid (empty-grid)))
