(defpackage :icfp/gui
  (:nicknames :gui)
  (:use :cl :alexandria :ltk :icfp/state)
  (:export :run-gui))

(in-package :icfp/gui)

;;; Timing
(defparameter *auto-play-delay* 0.05)

;;; Geometry
(defparameter *bg-honeycomb-r* 16)
(defparameter *grid-x-step* 16)
(defparameter *grid-y-step* (round (* 0.866 *grid-x-step*)))

(defparameter *placed-honeycomb-r* 14)
(defparameter *pivot-r* 5)

;;; Colors
(defparameter *bg-honeycomb-color* "gray70")
(defparameter *placed-honeycomb-color* "moccasin")
(defparameter *active-honeycomb-color* "gold")
(defparameter *overlap-honeycomb-color* "firebrick")
(defparameter *pivot-color* "gray30")

;;; Internal special vars
(defvar *last-figure*)
(defvar *curr-points*)
(defvar *canvas*)
(defvar *filled-cells*)
(defvar *move-history*)
(defvar *auto-play-moves*)

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

(defun place-honeycobm (coords color)
  (let ((x (car coords))
	(y (cdr coords)))
    (create-honeycomb
     (x-to-pixels x y)
     (y-to-pixels x y)
     *placed-honeycomb-r*
     :color color)))

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

(defun place-active-cell (point)
  (place-honeycobm point (destructuring-bind (x . y) point
			   (if (or (> 0 x)
				   (> 0 y)
				   (>= x *board-width*)
				   (>= y *board-height*)
				   (aref *filled-cells* (car point) (cdr point)))
			       *overlap-honeycomb-color*
			       *active-honeycomb-color*))))

(defun replace-figure (pivot &rest points)
  (dolist (item *last-figure*)
    (itemdelete *canvas* item))
  (let ((figure (mapcar #'place-active-cell points)))
    (setf *last-figure* (cons (place-pivot pivot) figure)
	  *curr-points* (cons pivot (copy-tree points)))))

(defun move-e ()
  (push (copy-tree *curr-points*) *move-history*)
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (cons (1- (car point)) (cdr point)))
		 *curr-points*)))
(defun move-w ()
  (push (copy-tree *curr-points*) *move-history*)
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (cons (1+ (car point)) (cdr point)))
		 *curr-points*)))
(defun move-se ()
  (push (copy-tree *curr-points*) *move-history*)
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (destructuring-bind (x . y) point
		     (cons (if (evenp y) (1- x) x)
			   (1+ y))))
		 *curr-points*)))
(defun move-sw ()
  (push (copy-tree *curr-points*) *move-history*)
  (apply #'replace-figure
	 (mapcar (lambda (point)
		   (destructuring-bind (x . y) point
		     (cons (if (oddp y) (1+ x) x)
			   (1+ y))))
		 *curr-points*)))

(defun pos-2-cube (point)
  (let* ((x (car point))
	 (y (cdr point))
	 (xx (- x (/ (- y (logand y 1)) 2))))
    (list xx (- (- xx) y) y)))

(defun cube-2-pos (cube)
  (let ((xx (first cube))
	(zz (third cube)))
    (cons (+ xx (/ (- zz (logand zz 1)) 2)) zz)))

(defun rotate-cube (cube)
  (list (- (third cube)) (- (first cube)) (- (second cube))))

(defun rotate-point (pivot point)
  (let* ((cube-pivot (pos-2-cube pivot))
	 (normalized (mapcar #'- (pos-2-cube point) cube-pivot))
	 (rotated-cube (mapcar #'+ cube-pivot (rotate-cube normalized))))
    (cube-2-pos rotated-cube)))

(defun rotate-cw ()
  (push (copy-tree *curr-points*) *move-history*)
  (let ((pivot (car *curr-points*)))
    (apply #'replace-figure
	   pivot
	   (mapcar (lambda (point)
		     (rotate-point pivot point))
		   (cdr *curr-points*)))))

(defun rotate-ccw ()
  (push (copy-tree *curr-points*) *move-history*)
  (let ((pivot (car *curr-points*)))
    (apply #'replace-figure
	   pivot
	   (mapcar (lambda (point)
		     (rotate-point
		      pivot
		      (rotate-point
		       pivot
		       (rotate-point
			pivot
			(rotate-point
			 pivot
			 (rotate-point pivot point))))))
		   (cdr *curr-points*)))))

(defun undo-move ()
  (when *move-history*
    (let ((prev-point (pop *move-history*)))
      (apply #'replace-figure prev-point))))

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
	  (setf (aref *filled-cells* x y) (place-honeycobm (cons x y) *placed-honeycomb-color*))))))
  (when-let ((pivot (board-pivot new-board)))
    (apply #'replace-figure pivot (board-active-cells new-board)))
  (setf *move-history* nil))

(defun run-gui (init-board update-fn)  
  (start-wish)
  (let* ((*last-figure* nil)
	(*canvas* nil)
	(*filled-cells* (make-array (list *board-width* *board-height*) :initial-element nil))
	(*move-history* nil)
	(*auto-play-moves* 0)
	(*bg-honeycomb-r* (min (floor 400 *board-height*) (floor 640 *board-width*)))
	(*grid-x-step* *bg-honeycomb-r*)
	(*grid-y-step* (round (* 0.866 *grid-x-step*)))
	(*placed-honeycomb-r* (- *bg-honeycomb-r* 2))
	(*pivot-r* (floor *placed-honeycomb-r* 2)))
    (let* ((board-scrolled-canvas (make-instance 'scrolled-canvas
						 :width 800
						 :height 800))
	   (top-bar (make-instance 'frame
				   :width 800))
	   (debug-text (make-instance 'entry
				      :master top-bar
				      :width 50))
	   (button-bar (make-instance 'frame
				      :width 800))
	   (btn-step (make-instance 'button 
				    :master top-bar
				    :text "   Step   "
				    :command (lambda ()
					       (invoke-restart 'continue-processing))))
	   (btn-step10 (make-instance 'button 
				      :master top-bar
				      :text " Step x10 "
				      :command (lambda ()
						 (setf *auto-play-moves* 10)
						 (invoke-restart 'continue-processing))))
	   (btn-step50 (make-instance 'button 
				      :master top-bar
				      :text " Step x50 "
				      :command (lambda ()
						 (setf *auto-play-moves* 50)
						 (invoke-restart 'continue-processing))))
	   (btn-step-inf (make-instance 'button 
				      :master top-bar
				      :text "   Play!  "
				      :command (lambda ()
						 (setf *auto-play-moves* -1)
						 (invoke-restart 'continue-processing))))
	   (btn-undo (make-instance 'button 
				    :master button-bar
				    :text " Undo! "
				    :command (lambda ()
					       (undo-move))))
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
					     (move-sw))))
	   (btn-cw (make-instance 'button 
				  :master button-bar
				  :text "  CW   "
				  :command (lambda ()
					     (rotate-cw))))
	   (btn-ccw (make-instance 'button 
				   :master button-bar
				   :text "  CCW  "
				   :command (lambda ()
					      (rotate-ccw)))))
      (setf *canvas* (canvas board-scrolled-canvas))
      (create-board-background *board-width* *board-height*)
      (redraw-board init-board)
      (pack top-bar :side :top)
      (pack btn-step :side :left)
      (pack btn-step10 :side :left)
      (pack btn-step50 :side :left)
      (pack btn-step-inf :side :left)
      (pack debug-text :side :left)
      (pack board-scrolled-canvas :expand 1 :fill :both)
      (scrollregion *canvas* 0 0 3000 3000)
      (pack button-bar :side :top)
      (pack btn-undo :side :left)
      (pack btn-e :side :left)
      (pack btn-w :side :left)
      (pack btn-se :side :left)
      (pack btn-sw :side :left)
      (pack btn-cw :side :left)
      (pack btn-ccw :side :left)
      (ignore-errors
	(handler-bind ((board-update (lambda (c)
				       (redraw-board (board-update-board c))
				       (setf (text debug-text) (board-update-msg c))
				       (if (zerop *auto-play-moves*)
					   (mainloop)
					   (progn
					     (decf *auto-play-moves*)
					     (sleep *auto-play-delay*)
					     (invoke-restart 'continue-processing))))))
	  (funcall update-fn))))))
