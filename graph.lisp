(defpackage :icfp/graph
  (:nicknames :graph)
  (:use :cl :alexandria :icfp/state :icfp/decoupled-tetris)
  (:export :get-connected-vertices
	   :vertice :make-vertice
	   :vertice-visited :vertice-distance :vertice-index :vertice-unit :vertice-closest-vertice
	   :visited :distance :index :unit :closest-vertice
	   :vertice-placeable-p :placeable-p
	   :with-cached-vertices))

(in-package :icfp/graph)

(defvar *vertices*)

(defstruct vertice visited distance index unit placeable-p closest-vertice)

(defmacro with-cached-vertices (&body body)
  `(let ((*vertices* (make-hash-table :test #'equalp)))
     ,@body))

(defun get-connected-vertices (vertice)
  (mapcar (lambda (unit)
	    (if-let ((old-vertice (gethash unit *vertices*)))
	      old-vertice
	      (setf (gethash unit *vertices*) (make-vertice :unit unit))))
	  (with-slots (unit placeable-p) vertice
	    (let ((all-neighbors (list (move-unit unit :E)
				       (move-unit unit :W)
				       (move-unit unit :SE)
				       (move-unit unit :SW)
				       (move-unit unit :R+)
				       (move-unit unit :R-))))
	      (remove-if (lambda (neighbor) (when-let ((illegal (or (not (legal-position-p neighbor))
								    (equalp neighbor unit))))
					      (setf placeable-p (or placeable-p (not (equalp neighbor unit))))
					      t))
			 all-neighbors)))))

