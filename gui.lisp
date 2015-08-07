(defpackage :icfp/gui
  (:nicknames :gui)
  (:use :cl :alexandria :ltk :icfp/state)
  (:export :run-gui))

(in-package :icfp/gui)

;;; Geometry
(defparameter *bg-honeycomb-r* 30)
(defparameter *grid-x-step* 30)
(defparameter *grid-y-step* (round (* 0.866 *grid-x-step*)))

(defparameter *placed-honeycomb-r* 26)
(defparameter *pivot-r* 12)

;;; Colors
(defparameter *bg-honeycomb-color* "gray70")
(defparameter *placed-honeycomb-color* "moccasin")
(defparameter *active-honeycomb-color* "gold")
(defparameter *pivot-color* "gray30")

;;; Internal special vars
(defvar *last-figure*)
(defvar *curr-points*)
(defvar *canvas*)
(defvar *filled-cells*)

(defun create-honeycomb (x y r &key (color "gray70"))
  (let* ((height (* 2 r))
	 (width (* 0.866 height))
	 (honeycomb (create-polygon *canvas*
			   (list
			    x (- y (* 0.5 height))
			    (+ (* 0.5 width) x) (- y (* 0.25 height))
			    (+ (* 0.5 width) x) (+ (* 0.25 height) y)
			    x (+ (* 0.5 height) y)
			    (- x (* 0.5 width)) (+ (* 0.25 height) y)
			    (- x (* 0.5 width)) (- y (* 0.25 height))))))
    (itemconfigure *canvas* honeycomb "fill" color)
    honeycomb))

(defun x-to-pixels (x y)
  (+ (* 3 *grid-x-step*) (* 2 x *grid-x-step*) (if (oddp y) *grid-x-step* 0)))

(defun y-to-pixels (x y)
  (declare (ignore x))
  (+ (* 3 *grid-x-step*) (* 2 y *grid-y-step*)))

(defun create-board-background (x y)
  (dotimes (i x)
    (dotimes (j y)
      (create-honeycomb (x-to-pixels i j) (y-to-pixels i j) *bg-honeycomb-r* :color *bg-honeycomb-color*))))

(defun place-honeycobm (coords &key (active t))
  (let ((x (car coords))
	(y (cdr coords)))
    (create-honeycomb
     (x-to-pixels x y)
     (y-to-pixels x y)
     *placed-honeycomb-r*
     :color (if active *active-honeycomb-color* *placed-honeycomb-color*))))

(defun place-pivot (coords)
  (let* ((x (car coords))
	 (y (cdr coords))
	 (x0 (- (x-to-pixels x y) *pivot-r*))
	 (y0 (- (y-to-pixels x y) *pivot-r*))
	 (x1 (+ x0 (* 2 *pivot-r*)))
	 (y1 (+ y0 (* 2 *pivot-r*)))
	 (oval (create-oval *canvas* x0 y0 x1 y1)))
    (itemconfigure *canvas* oval "fill" *pivot-color*)
    oval))

(defun replace-figure (pivot &rest points)
  (format t "~A" *last-figure*)
  (dolist (item *last-figure*)
    (itemdelete *canvas* item))
  (setf *last-figure* (cons (place-pivot pivot)
			    (mapcar #'place-honeycobm points))
	*curr-points* (cons pivot (copy-tree points))))

(defun move-e ()
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (cons (1- (car point)) (cdr point)))
		 *curr-points*)))
(defun move-w ()
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (cons (1+ (car point)) (cdr point)))
		 *curr-points*)))
(defun move-se ()
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (destructuring-bind (x . y) point
		     (cons (if (evenp y) (1- x) x)
			   (1+ y))))
		 *curr-points*)))
(defun move-sw ()
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (destructuring-bind (x . y) point
		     (cons (if (oddp y) (1+ x) x)
			   (1+ y))))
		 *curr-points*)))


(defun redraw-board (new-board)
  (dotimes (x *board-width*)
    (dotimes (y *board-height*)
      (let ((elem-set (aref (board-grid new-board) x y))
	    (board-item (aref *filled-cells* x y)))
	(when (and (zerop elem-set)
		   board-item)
	  (itemdelete *canvas* board-item)
	  (setf (aref *filled-cells* x y) nil))
	(when (and (not (zerop elem-set))
		   (not board-item))
	  (setf (aref *filled-cells* x y) (place-honeycobm (cons x y) :active nil))))))
  (when-let ((pivot (board-pivot new-board)))
    (replace-figure pivot (board-active-cells new-board))))

(defun run-gui (init-board)
  (let ((*last-figure* nil)
	(*canvas* nil)
	(*filled-cells* (make-array (list *board-width* *board-height*) :initial-element nil)))
    (with-ltk ()
      (let* ((board-scrolled-canvas (make-instance 'scrolled-canvas
						   :width 800
						   :height 800))
	     (button-bar (make-instance 'frame
					:width 800))
	     (btn-e (make-instance 'button 
				   :master button-bar
				   :text "   E   "
				   :command (lambda ()
					      (move-e))))
	     (btn-w (make-instance 'button 
				   :master button-bar
				   :text "   W   "
				   :command (lambda ()
					      (move-w))))
	     (btn-se (make-instance 'button 
				   :master button-bar
				   :text "  SE   "
				   :command (lambda ()
					      (move-se))))
	     (btn-sw (make-instance 'button 
				   :master button-bar
				   :text "  SW   "
				   :command (lambda ()
					      (move-sw)))))
	(setf *canvas* (canvas board-scrolled-canvas))
	(create-board-background *board-width* *board-height*)
	(redraw-board init-board)
	(replace-figure '(5 . 0) '(2 . 0) '(3 . 0) '(7 . 0) '(8 . 0))
	(pack board-scrolled-canvas :expand 1 :fill :both)
	(scrollregion *canvas* 0 0 3000 3000)
	(pack button-bar :side :top)
	(pack btn-e :side :left)
	(pack btn-w :side :left)
	(pack btn-se :side :left)
	(pack btn-sw :side :left)))))
