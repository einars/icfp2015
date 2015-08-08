(defpackage :icfp/state
  (:nicknames :state)
  (:use :cl)
  (:export :*board-width* :*board-height* :*total-moves* :last-move
	   :board :board-grid :board-pivot :board-active-cells :board-pieces
	   :make-pos :pos-x :pos-y :pos-add :pos-sub :adjust-pos :copy-pos
	   :make-piece :copy-piece :piece-pivot :piece-offset :active-cells
	   :score-size :score-lines :score-history :make-score :board-done
	   :piece-turn :piece-number :piece-config :board-log :board-spot
	   :empty-grid :empty-board :copy-board :board-cmd :board-stats
	   :board-update :board-update-board :board-update-msg
	   :continue-processing))

(in-package :icfp/state)

(defvar *board-width*)
(defvar *board-height*)
(defvar *total-moves*)

(define-condition board-update ()
  ((new-board :initarg :new-board
	      :reader board-update-board
	      :initform nil)
   (debug-msg :initarg :debug-msg
	      :reader board-update-msg
	      :initform nil)))

(defstruct pos x y)

(defun pos-add (a b)
  (make-pos :x (+ (pos-x a) (pos-x b))
	    :y (+ (pos-y a) (pos-y b))))

(defun pos-sub (a b)
  (make-pos :x (- (pos-x a) (pos-x b))
	    :y (- (pos-y a) (pos-y b))))

(defstruct piece pivot config offset (turn 0) (number 0))

(defstruct score (size 0) (lines 0) history)

(defstruct board grid pieces log cmd done spot stats)

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
