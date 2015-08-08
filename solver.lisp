(defpackage :icfp/solver
  (:nicknames :solver)
  (:use :cl :alexandria :icfp/graph :icfp/dijkstra :icfp/decoupled-tetris :icfp/gui :icfp/state)
  (:export :solve))

(in-package :icfp/solver)

(defvar *search-depth*)
(defvar *all-vertices*)

(defun solve (number &key (with-gui t) (search-depth 0))
  (let ((*search-depth* search-depth))
    (solve-problem number :solver #'solve1 :with-gui with-gui)))

(defun solve1 (id seed board with-gui)
  (run-gui board (lambda ()
		   (with-current-board board
		     (solve2)))))

(defun solve2 ()
  (with-next-unit unit
    (let* ((*all-vertices* nil)
	   (curr-vertice (make-vertice :unit unit))
	   (best-vertice (find-best-vertice curr-vertice)))
      (apply-path (nreverse (get-path best-vertice)) (vertice-unit best-vertice))
      (solve2))))

(defun get-path (vertice)
  (when-let ((backlink (vertice-closest-vertice vertice)))
    (let ((move (cond
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :E)) :E)
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :W)) :W)
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :SE)) :SE)
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :SW)) :SW)
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :R+)) :R+)
		  ((equalp (vertice-unit vertice) (move-unit (vertice-unit backlink) :R-)) :R-)
		  (t (error "Habala")))))
      (cons move (get-path backlink)))))

(defun find-best-vertice (start-vertice &key (depth 0))
  (let ((best-vertice nil)
	best-vertice-score)
    (setf *all-vertices* (map-paths start-vertice))
    (dolist (vertice *all-vertices*)
      (when (vertice-placeable-p vertice)
	(let ((score (if (> depth *search-depth*)
			 (+ (score-vertice vertice)
			    (with-unit-applied (vertice-unit vertice)
			      (with-next-unit next-unit
				(multiple-value-bind (next-vertice score) (find-best-vertice (make-vertice :unit next-unit) :depth (1+ depth))
				  (declare (ignore next-vertice))
				  score))))
			 (score-vertice vertice))))
	  (when (or (null best-vertice)
		    (> score best-vertice-score))
	    (setf best-vertice vertice
		  best-vertice-score score)))))
    (values best-vertice best-vertice-score)))

(defun score-vertice (vertice)
  (with-unit-applied (vertice-unit vertice)
    (let ((height-penalty 0))
      (dotimes (x *board-width*)
	(dotimes (y *board-width*)
	  (when (pos-filled (cons x y))
	    (decf height-penalty (- *board-height* y)))))
      (+ (* 100 (last-application-deleted-rows)) height-penalty))))
