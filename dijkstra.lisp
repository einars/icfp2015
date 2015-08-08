(defpackage :icfp/dijkstra
  (:nicknames :dijkstra)
  (:use :cl :alexandria :cl-heap :icfp/graph)
  (:export :map-paths))

(in-package :icfp/dijkstra)

;;; Special variables
(defvar *vertices*)
(defvar *heap*)

(defmacro with-special-vars (&body body)
  `(let ((*vertices* nil)
	 (*heap* (make-instance 'fibonacci-heap :key #'vertice-distance)))
     ,@body))

;;; Body
(defun map-paths (starting-vertice)
  (with-cached-vertices
    (with-special-vars
      (register-vertice starting-vertice 0 nil)
      (map-connected starting-vertice)
      *vertices*)))

(defun map-connected (vertice)
  (setf (vertice-visited vertice) t)
  (push vertice *vertices*)
  (let ((neighbor-distance (1+ (vertice-distance vertice))))
    (dolist (neighbor (get-connected-vertices vertice))
      (register-vertice neighbor neighbor-distance vertice))
    (when-let ((next (pop-heap *heap*)))
      (map-connected next))))

(defun register-vertice (vertice new-distance backlink)
  (with-slots (distance visited index closest-vertice) vertice
    (if distance
	(when (< new-distance distance)
	  (setf distance new-distance
		closest-vertice backlink)
	  (unless visited
	    (decrease-key *heap* index new-distance)))
	(progn
	  (setf distance new-distance
		index (add-to-heap *heap* vertice)
		closest-vertice backlink)))))
