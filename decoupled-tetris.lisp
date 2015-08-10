(defpackage :icfp/decoupled-tetris
  (:nicknames :decoupled-tetris)
  (:use :cl :alexandria :icfp/state :icfp/tetris)
  (:export :with-current-board
	   :with-next-unit
	   :with-unit-applied
	   :move-unit
	   :board-rank
	   :apply-path
	   :legal-position-p
	   :last-application-deleted-rows
	   :pos-filled
	   :solve-problem))

(in-package :icfp/decoupled-tetris)

(defvar *board*)
(defvar *base-board*)
(defvar *delta*)
(defvar *current-unit-no*)
(defvar *just-removed-rows*)

(defmacro with-current-board (board &body body)
  `(let ((*base-board* (board-grid ,board))
	 (*board* ,board)
	 (*delta* nil)
	 (*current-unit-no* -1))
     ,@body))

(defmacro with-next-unit (unit-var &body body)
  `(if (>= (1+ *current-unit-no*) (length *move-sequence*))
       (signal 'done :board *board*)
       (let* ((*current-unit-no* (1+ *current-unit-no*))
	      (piece (aref *units* (aref *move-sequence* *current-unit-no*)))
	      (,unit-var (cons (adjust-pos (piece-pivot piece) (piece-offset piece))
			       (active-cells piece))))
	 ,@body)))

(defmacro with-unit-applied (unit &body body)
  `(let* ((*delta* (cons ,(copy-tree unit) *delta*))
	  (removed-rows (find-full-rows))
	  (*just-removed-rows* (length removed-rows)))
     (dolist (row removed-rows)
       (push row *delta*))
     ,@body))

(defun find-full-rows ()
  (loop as y from (1- *board-height*) downto 0
     when (loop as x from 0 below *board-width*
	     always (pos-filled (cons x y)))
       collect y))

(defun move-unit (unit move)
  (case move
    (:E (move-horiz unit +1))
    (:W (move-horiz unit -1))
    (:SE (move-vert-e unit +1))
    (:SW (move-vert-w unit +1))
    (:NE (move-vert-e unit -1))
    (:NW (move-vert-w unit -1))
    (:R+ (rotate-cw unit))
    (:R- (rotate-ccw unit))))

(defun legal-position-p (unit)
  (loop as pos in (cdr unit)
     never (destructuring-bind (x . y) pos
	     (or (> 0 x)
		 (> 0 y)
		 (>= x *board-width*)
		 (>= y *board-height*)
		 (pos-filled pos)))))

(defun last-application-deleted-rows ()
  *just-removed-rows*)

(defun pos-filled (coords)
  (destructuring-bind (x . y) coords
    (dolist (change *delta*)
      (if (numberp change)
	  (when (>= y change)
	    (incf y))
	  (when (find (cons x y) (cdr change) :test 'equalp)
	    (return-from pos-filled t))))
    (when (< y *board-height*)
      (not (zerop (aref *base-board* x y))))))


(defparameter *power-patterns*
  '(
     ; height=1: length=3
    ((:SW)    . ("Ei!" :E :SW :W))
    ((:SE :W) . ("Ei!" :E :SW :W))
    ((:W :SE) . ("Ei!" :E :SW :W))

     ; height=5: length=7
    ((:W :SW :SW :SW :SW :SW) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))
    ((:SW :W :SW :SW :SW :SW) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))
    ((:SW :SW :W :SW :SW :SW) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))
    ((:SW :SW :SW :W :SW :SW) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))
    ((:SW :SW :SW :SW :W :SW) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))
    ((:SW :SW :SW :SW :SW :W) . ("Ia! Ia!" :SW :SW :W :SE :SW :SW :W))

     ; height=2: length=6
    ((:SE :SE) . ("R'lyehx" :R+ :W :SE :E :E :SW :R-))

     ; height=4: length=7
    ((:SE :SW :SE :SW) . ("Yuggothzz" :E :R- :SW :SW :SE :R- :SW :R+ :R+))
    ((:SW :SE :SW :SE) . ("Yuggothzz" :E :R- :SW :SW :SE :R- :SW :R+ :R+))
    ((:SE :SE :SW :SW) . ("Yuggothzz" :E :R- :SW :SW :SE :R- :SW :R+ :R+))
    ((:SW :SW :SE :SE) . ("Yuggothzz" :E :R- :SW :SW :SE :R- :SW :R+ :R+))

     ; height=6: length=10
    ((:SE :SW :SE :SW :SE :SE) .
     ("Yogsothothzzz" :E :SE :SW :R- :SE :R- :SW :SE :R- :SW :R+ :R+ :R+))
    ((:SE :SW :SE :SE :SW :SE) .
     ("Yogsothothzzz" :E :SE :SW :R- :SE :R- :SW :SE :R- :SW :R+ :R+ :R+))
    ((:SE :SE :SW :SE :SW :SE) .
     ("Yogsothothzzz" :E :SE :SW :R- :SE :R- :SW :SE :R- :SW :R+ :R+ :R+))
    ((:E :SE :SW :SE :SW :SE :SW) .
     ("Yogsothothzzz" :E :SE :SW :R- :SE :R- :SW :SE :R- :SW :R+ :R+ :R+))
    ((:SE :SW :SE :SW :SE :SW :E) .
     ("Yogsothothzzz" :E :SE :SW :R- :SE :R- :SW :SE :R- :SW :R+ :R+ :R+))
    ))

(defun match-start (word path)
  (cond ((null word) path)
	((or (null path) (not (eq (first word) (first path)))) 'fail)
	(t (match-start (rest word) (rest path)))))

(defun match-single (pattern head tail)
  (let ((match (match-start (car pattern) tail)))
    (cond ((null tail) nil)
	  ((consp match) (list (nreverse head) (cdr pattern) match))
	  (t (match-single pattern (cons (first tail) head) (rest tail))))))

(defvar *power-results*)

(defun glue (lst)
  (apply #'append lst))

(defun gen-further (match bagage tail)
  (gen-powers (first (last match)) (append bagage tail)))

(defun gen-powers (path &optional (bagage nil))
  (dolist (i *power-patterns* *power-results*)
    (let ((match (match-single i nil path)))
      (when match
	(push (glue (append bagage match)) *power-results*)
	(gen-further match bagage (butlast match))
	(gen-further match bagage (list (first match) (car i)))))))

(defun bad-board (board)
  (or (null board)
      (board-done board)
      (not (= (board-rank board) (board-rank *board*)))))

(defun bad-path (path board)
  (cond ((bad-board board))
	((null path) nil)
	(t (bad-path (rest path) (make-move board (first path))))))

(defun good-path (path)
  (not (bad-path path *board*)))

(defun make-power-patterns (path)
  (let ((*power-results* nil))
    (delete-if-not #'good-path (gen-powers path))))

(defun apply-path (path final-unit &key (apply-fun #'make-move))
  (format t "old-path:~A~%" path)
  (let ((power-list (make-power-patterns path)))
    (when power-list (setf path (first (sort power-list #'> :key #'length)))))
  (format t "new-path:~A~%~%" path)
  (setf *delta* nil)
  (dolist (move path)
    (setf *board* (funcall apply-fun *board* move)))
  (cond
    ((not (legal-position-p (move-unit final-unit :E)))
     (setf *board* (funcall apply-fun *board* :E)))
    ((not (legal-position-p (move-unit final-unit :W)))
     (setf *board* (funcall apply-fun *board* :W)))
    ((not (legal-position-p (move-unit final-unit :SE)))
     (setf *board* (funcall apply-fun *board* :SE)))
    ((not (legal-position-p (move-unit final-unit :SW)))
     (setf *board* (funcall apply-fun *board* :SW)))
    ((not (legal-position-p (move-unit final-unit :R+)))
     (setf *board* (funcall apply-fun *board* :R+)))
    ((not (legal-position-p (move-unit final-unit :R-)))
     (setf *board* (funcall apply-fun *board* :R-)))
    (t (error "Invalid final unit ~A" final-unit)))  
  (setf *base-board* (board-grid *board*)))


;;; Movement

(defun move-horiz (unit direction)
  (mapcar (lambda (point)
	    (cons (+ direction (car point)) (cdr point)))
	  unit))

(defun move-vert-e (unit direction)
  (mapcar (lambda (point)
	    (destructuring-bind (x . y) point
	      (cons (if (oddp y) (1+ x) x)
		    (+ direction y))))
	  unit))

(defun move-vert-w (unit direction)
  (mapcar (lambda (point)
	    (destructuring-bind (x . y) point
	      (cons (if (evenp y) (1- x) x)
		    (+ direction y))))
	  unit))

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

(defparameter *rotate-cw-cache* (make-hash-table :test 'equalp))
(defparameter *rotate-ccw-cache* (make-hash-table :test 'equalp))

(defun rotate-cw (unit)
  (if-let ((cached-unit (gethash unit *rotate-cw-cache*)))
    cached-unit
    (progn
      (setf (gethash unit *rotate-cw-cache*) (rotate-cw1 unit))
      (rotate-cw unit))))

(defun rotate-cw1 (unit)
  (let ((pivot (car unit)))
    (cons pivot (mapcar (lambda (point)
			  (rotate-point pivot point))
			(cdr unit)))))

(defun rotate-ccw (unit)
  (if-let ((cached-unit (gethash unit *rotate-ccw-cache*)))
    cached-unit
    (progn
      (setf (gethash unit *rotate-ccw-cache*) (rotate-ccw1 unit))
      (rotate-ccw unit))))

(defun rotate-ccw1 (unit)
  (let ((pivot (car unit)))
    (cons pivot (mapcar (lambda (point)
			  (rotate-point
			   pivot
			   (rotate-point
			    pivot
			    (rotate-point
			     pivot
			     (rotate-point
			      pivot
			      (rotate-point pivot point))))))
			(cdr unit)))))
