(defpackage :icfp/dijkstra
  (:nicknames :dijkstra)
  (:use :cl :alexandria :cl-heap :icfp/graph)
  (:export :map-paths :remap-with-vertices-removed))

(in-package :icfp/dijkstra)

;;; Special variables
(defvar *vertices*)
(defvar *heap*)

(defmacro with-special-vars (&body body)
  `(let ((*vertices* nil)
	 (*heap* (make-instance 'fibonacci-heap :key (lambda (vertice &optional new-value)
						       (when new-value
						         (setf (vertice-distance vertice) new-value))
						       (vertice-distance vertice)))))
     ,@body))

;;; Body
(defun map-paths (starting-vertice)
  (with-cached-vertices nil
    (with-special-vars
      (register-vertice starting-vertice 0 nil)
      (map-connected starting-vertice)
      *vertices*)))

(defun remap-with-vertices-removed (old-vertices removed)
  (with-cached-vertices old-vertices
    (with-special-vars
      (let ((min-depth (- (reduce (lambda (acc vertice)
				     (if (or (not acc)
					     (< (vertice-distance vertice) acc))
					 (vertice-distance vertice)
					 acc))
				   removed
				   :initial-value nil)
			  5)))
	(dolist (vertice old-vertices)
	  (unless (find vertice removed)
	    (if (< (vertice-distance vertice) min-depth)
		(push vertice *vertices*)
		(if (= (vertice-distance vertice) min-depth)
		    (multiple-value-bind (vertice heap-index) (add-to-heap *heap* vertice)
		      (setf (vertice-index vertice) heap-index))
		    (with-slots (distance visited index placeable-p closest-vertice force-recalc) vertice
		      (setf visited nil
			    index nil
			    force-recalc t))))))
	(when-let ((next (pop-heap *heap*)))
	  (map-connected next))
	*vertices*))))

(defun map-connected (vertice)
  (setf (vertice-visited vertice) t)
  (push vertice *vertices*)
  (let ((neighbor-distance (1+ (vertice-distance vertice))))
    (dolist (neighbor (get-connected-vertices vertice))
      (register-vertice neighbor neighbor-distance vertice))
    (when-let ((next (pop-heap *heap*)))
      (map-connected next))))

(defun register-vertice (vertice new-distance backlink)
  (with-slots (distance visited index closest-vertice force-recalc) vertice
    (if (and distance (not force-recalc))
	(when (< new-distance distance)
	  (setf	closest-vertice backlink)
	  (unless visited
	    (decrease-key *heap* index new-distance)))
	(progn
	  (setf distance new-distance)
	  (multiple-value-bind (vertice heap-index) (add-to-heap *heap* vertice)
	    (declare (ignore vertice))
	    (setf force-recalc nil
		index heap-index
		closest-vertice backlink))))))
