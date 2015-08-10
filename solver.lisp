(defpackage :icfp/solver
  (:nicknames :solver)
  (:use :cl :alexandria :icfp/graph :icfp/dijkstra :icfp/decoupled-tetris :icfp/gui :icfp/state :icfp/simulator)
  (:export :solve))

(in-package :icfp/solver)

(defvar *search-depth*)
(defvar *all-vertices*)

(defvar *mapped-vertices*)
(defvar *last-mapped-board*)

(defvar *with-gui*)
(defvar *try-depth*)
(defvar *last-clear*)

(defun solve (number &key (with-gui t) (search-depth 0) try-depth)
  (let ((*search-depth* search-depth)
	(*mapped-vertices* nil)
	(*last-mapped-board* nil)
	(*with-gui* with-gui)
	(*try-depth* try-depth)
	(*last-clear* nil)
	(*found-power-words* nil))
    (solve-problem number
		   :solver (if with-gui
			       (gen-simulator #'solve1)
			       (lambda (id seed board with-gui)
				 (declare (ignore id seed with-gui))
				 (solve1 board)))
		   :with-gui with-gui)))

(defun solve1 (board)
  (handler-case
    (with-current-board board
      (let ((*watershed* 0))
	(solve2)))
    (done (c) (done-board c))))

(defun solve2 ()
  (with-next-unit unit
    (let* ((*all-vertices* nil)
	   (curr-vertice (make-vertice :unit unit))
	   (best-vertice (find-best-vertice curr-vertice)))
      (let ((highest-height *board-height*))
	(dotimes (x *board-width*)
	  (dotimes (y *board-height*)
	    (when (and (pos-filled (cons x y))
		       (< y highest-height))
	      (setf highest-height y))))
	(when (> highest-height *watershed*)
	  (setf *watershed* highest-height)))
      (if *with-gui*
	  (apply-path (nreverse (get-path best-vertice)) (vertice-unit best-vertice) :apply-fun #'play-move)
	  (apply-path (nreverse (get-path best-vertice)) (vertice-unit best-vertice)))
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

(defun smart-map (start-vertice)
  (setf *last-mapped-board* decoupled-tetris::*base-board*)
	(setf *mapped-vertices* (map-paths start-vertice))
	*mapped-vertices*)

(defun find-removed-vertices (vertices removed-units)
  (loop as vertice in vertices
     when (find (vertice-unit vertice) removed-units)
       collect vertice))

(defun find-best-vertice (start-vertice &key (depth 0))
  (let ((best-vertice nil)
	(mapped-vertices (smart-map start-vertice))
	best-vertice-score)
    (setf *all-vertices* (remove-if-not #'vertice-placeable-p mapped-vertices))
    (when (and *try-depth* (eql depth 0))
      (setf *all-vertices* (sort *all-vertices* (lambda (a b) (> (score-vertice a) (score-vertice b))))))
    (dolist (vertice (subseq *all-vertices* 0 (if (and *try-depth* (eql depth 0))
						  (min *try-depth* (length *all-vertices*))
						  (length *all-vertices*))))
      (when (vertice-placeable-p vertice)
	(let ((score (if (< depth *search-depth*)
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
  (+ (with-unit-applied (vertice-unit vertice)
       (+ (prog1 (cond
		   (*last-clear* (* (last-application-deleted-rows) 1000))
		   ((eql (last-application-deleted-rows) 1)
		    (* (- 0.6 (/ (cdar (vertice-unit vertice)) *board-height*)) 30))
		   (t (* (last-application-deleted-rows) 500)))
	    (setf *last-clear* (> (last-application-deleted-rows) 1)))
	  #+nil(reduce (lambda (acc point) (- acc (sqrt (- *board-height* (cdr point)))))
		       (cdr (vertice-unit vertice))
		       :initial-value 0)))
     (reduce (lambda (acc point)
	       (+ acc
		  (score-connected point)
		  (* 0 (score-overhangs point))))
	     (cdr (vertice-unit vertice))
	     :initial-value 0)
     (let ((high-point (loop as point in (cdr (vertice-unit vertice))
			  maximize (cdr point))))
       (declare (ignorable high-point))
       (- (expt (* 1.2 (abs (- (if (zerop *watershed*) *board-height* *watershed*) (cdar (vertice-unit vertice)) ))) 1.7)))))

(defun score-connected (point)
  (loop as point in (list (move-unit (list point) :E)
			  (move-unit (list point) :W)
			  (move-unit (list point) :SE)
			  (move-unit (list point) :SW)
			  (move-unit (list point) :NE)
			  (move-unit (list point) :NW))
     sum (filled-or-walled (car point))))

(defun score-overhangs (point)
  (loop as point in (list (move-unit (list point) :SE)
			  (move-unit (list point) :SW))
     count (destructuring-bind (x . y) (car point)
	     (and (>= x 0)
		  (< x *board-width*)
		  (>= y 0)
		  (< y *board-height*)
		  (not (pos-filled (car point)))))))

(defun filled-or-walled (point)
  (destructuring-bind (x . y) point
    (cond
      ((< y 0) 0)
      ((or (< x 0)
	   (>= x *board-width*)) (if (evenp y) 0.8 0.5))
      ((or (>= y *board-height*)
	   (pos-filled point)) 1)
      (t 0))))
