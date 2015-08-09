(defpackage :icfp/solver
  (:nicknames :solver)
  (:use :cl :alexandria :icfp/graph :icfp/dijkstra :icfp/decoupled-tetris :icfp/gui :icfp/state)
  (:export :solve))

(in-package :icfp/solver)

(defvar *search-depth*)
(defvar *all-vertices*)

(defvar *mapped-vertices*)
(defvar *last-mapped-board*)

(defun solve (number &key (with-gui t) (search-depth 0))
  (let ((*search-depth* search-depth)
	(*mapped-vertices* nil)
	(*last-mapped-board* nil))
    (solve-problem number :solver #'solve1 :with-gui with-gui)))

(defun solve1 (id seed board with-gui)
  (if with-gui
      (run-gui board (lambda ()
		       (with-current-board board
			 (solve2))))
      (with-current-board board
	(solve2))))

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

(defun smart-map (start-vertice)
  (if (and nil (eql *last-mapped-board* decoupled-tetris::*base-board*)
	   (loop as delta in decoupled-tetris::*delta*
	      never (numberp delta)))
      (let ((new-map (remap-with-vertices-removed
		      (mapcar (lambda (vertice)
				(with-slots (visited distance index unit placeable-p closest-vertice force-recalc) vertice
				  (make-vertice :visited visited
						:distance distance
						:index nil
						:unit unit
						:placeable-p placeable-p
						:closest-vertice nil
						:force-recalc t)))
			      *mapped-vertices*)
		      (find-removed-vertices *mapped-vertices* decoupled-tetris::*delta*))))
	(show-placements new-map)
	new-map)
      (progn
	(setf *last-mapped-board* decoupled-tetris::*base-board*)
	(setf *mapped-vertices* (map-paths start-vertice))
	(show-placements *mapped-vertices*)
	*mapped-vertices*)))

(defun show-placements (new-map)
  #+nil(let ((new-board (make-array (list *board-width* *board-height*) :initial-element 0))
	(counter 0))
    (dolist (vertice new-map)
      (when (vertice-placeable-p vertice)
	(incf counter)
	(dolist (point (cdr (vertice-unit vertice)))
	  (setf (aref new-board (car point) (cdr point)) 1))))
    (format t "~%~A" counter)
    (gui::redraw-board (state::make-board :grid new-board))
    (sleep 0.5)))

(defun find-removed-vertices (vertices removed-units)
  (loop as vertice in vertices
     when (find (vertice-unit vertice) removed-units)
       collect vertice))

(defun find-best-vertice (start-vertice &key (depth 0))
  (let ((best-vertice nil)
	(mapped-vertices (smart-map start-vertice))
	best-vertice-score)
    (setf *all-vertices* (remove-if-not #'vertice-placeable-p mapped-vertices))
    (setf *all-vertices* (sort *all-vertices* (lambda (a b) (> (vertice-distance a) (vertice-distance b)))))
    (dolist (vertice (subseq *all-vertices* 0 (min 20 (length *all-vertices*))))
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
    (when (zerop depth)
      (format t "~%~A" best-vertice-score))
    (values best-vertice best-vertice-score)))

(defun score-vertice (vertice)
  (with-unit-applied (vertice-unit vertice)
    (+ (* 100 (last-application-deleted-rows))
       (reduce (lambda (acc point) (- acc (sqrt (- *board-height* (cdr point)))))
	       (cdr (vertice-unit vertice))
	       :initial-value 0))))
