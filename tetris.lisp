(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json :icfp/state :icfp/gui)
  (:export :solve-problem
	   :get-next-board
	   :update-gui
	   :get-item
	   :make-move))

(in-package :icfp/tetris)

(defvar *seed* 0)
(defvar *units* nil)
(defvar *break* nil)
(defvar *best* nil)

(defparameter *pool-size* 10)

(defun update-gui (board)
  (funcall *break* board))

(defvar *move-sequence*)

(defun get-item (item data)
  (cdr (assoc item data)))

(defun create-pos (pos-data)
  (make-pos :x (get-item :x pos-data) :y (get-item :y pos-data)))

(defun get-pos (item data)
  (create-pos (get-item item data)))

(defun random-elt (list)
  (elt list (random (length list))))

(defun parse-board (data board)
  (dolist (locked (get-item :filled data))
    (setf (aref (board-grid board)
		(get-item :x locked)
		(get-item :y locked))
	  1)))

(defun pretty-cell (number)
  (case number
    (0 "o")
    (1 "x")
    (2 "*")
    (4 ".")
    (5 "X")
    (6 "@")))

(defun print-raw-board (board)
  (dotimes (y *board-height*)
    (when (oddp y)
      (format t " "))
    (dotimes (x *board-width*)
      (format t "~A " (pretty-cell (aref (board-grid board) x y))))
    (format t "~%")))

(defun cell-on-board (cell)
  (and (<= 0 (car cell) (1- *board-width*))
       (<= 0 (cdr cell) (1- *board-height*))))

(defun print-board (vanilla-board)
  (let* ((board (clone-board vanilla-board))
	 (pivot (board-pivot vanilla-board)))
    (dolist (i (board-active-cells vanilla-board))
      (setf (aref (board-grid board) (car i) (cdr i)) 2))
    (when (cell-on-board pivot)
      (incf (aref (board-grid board) (car pivot) (cdr pivot)) 4))
    (print-raw-board board)))

(defun copy-grid (board)
  (let ((grid (empty-grid)))
    (dotimes (x *board-width*)
      (dotimes (y *board-height*)
	(setf (aref grid x y) (aref (board-grid board) x y))))
    grid))

(defun clone-board (board)
  (let ((copy (copy-board board)))
    (setf (board-grid copy) (copy-grid board))
    copy))

(defun rnd ()
  (prog1 (logand (ash *seed* -16) #x7fff)
    (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 32)))))

(defun adjust-piece-offset (piece x y fn)
  (when (funcall fn (pos-y (piece-offset piece))) (setf x 0))
  (incf (pos-x (piece-offset piece)) x)
  (incf (pos-y (piece-offset piece)) y))

(defun adjust-piece-turn (piece turn)
  (setf (piece-turn piece) (mod (+ (piece-turn piece) turn) 6)))

(defun good-cell (board cell)
  (and (cell-on-board cell)
       (= 0 (aref (board-grid board) (car cell) (cdr cell)))))

(defun is-outside (board cells)
  (cond ((null cells) nil)
	((not (good-cell board (first cells))) t)
	(t (is-outside board (rest cells)))))

(defun is-locking (board)
  (is-outside board (board-active-cells board)))

(defun fetch-next-unit (number)
  (copy-piece (aref *units* (aref *move-sequence* number))))

(defun get-new-piece (board)
  (let ((number (1+ (piece-number (last-move board)))))
    (when (< number *total-moves*)
      (let ((next (fetch-next-unit number)))
	(setf (piece-number next) number)
	next))))

(defun is-row-full (row board &optional (i 0))
  (cond ((= i *board-width*) t)
	((= 0 (aref (board-grid board) i row)) nil)
	(t (is-row-full row board (1+ i)))))

(defun delete-row (row board)
  (let ((grid (board-grid board)))
    (dotimes (x row)
      (dotimes (i *board-width*)
	(setf (aref grid i (- row x)) (aref grid i (- row (+ x 1))))))
    (dotimes (i *board-width*)
      (setf (aref grid i 0) 0))))

(defun test-and-update-if-full (row board score)
  (when (is-row-full row board)
    (incf (score-lines score))
    (delete-row row board)))

(defun sweet-spot (board)
  (dotimes (y *board-height*)
    (dotimes (x *board-width*)
      (when (= 0 (aref (board-grid board) x (- *board-height* y 1)))
	(return-from sweet-spot (cons x (- *board-height* y 1)))))))

(defun lock-piece-and-update-score (board)
  (setf (board-grid board) (copy-grid board))
  (let ((rows nil) (score (make-score :history (board-pieces board))))
    (push score (board-log board))
    (dolist (i (board-active-cells board))
      (setf (aref (board-grid board) (car i) (cdr i)) 1)
      (incf (score-size score))
      (push (cdr i) rows))
    (dolist (i (remove-duplicates rows))
      (test-and-update-if-full i board score))
    (setf (board-spot board) (sweet-spot board))))

(defun lock-down (board)
  (pop (board-pieces board))
  (lock-piece-and-update-score board)
  (let ((new-piece (get-new-piece board)))
    (cond ((null new-piece) (setf (board-done board) t))
	  (t (push new-piece (board-pieces board))
	     (when (is-locking board)
	       (pop (board-pieces board))
	       (setf (board-done board) t)))))
  board)

(defun same-piece (a b)
  (= (piece-number a) (piece-number b)))

(defun test-moves (head tail)
  (cond
    ((or (null tail) (not (same-piece head (first tail)))) nil)
    ((equalp (active-cells head) (active-cells (first tail))) t)
    (t (test-moves head (rest tail)))))

(defun is-bad-move (board)
  (let ((pieces (board-pieces board)))
    (test-moves (first pieces) (rest pieces))))

(defun make-move (board move)
  (let* ((new-board (copy-board board))
	 (next (copy-piece (last-move board))))
    (setf (piece-offset next) (copy-pos (piece-offset next)))
    (push next (board-pieces new-board))
    (push move (board-cmd new-board))
    (case move
      (:W  (adjust-piece-offset next -1 0 #'null))
      (:E  (adjust-piece-offset next  1 0 #'null))
      (:SW (adjust-piece-offset next -1 1 #'oddp))
      (:SE (adjust-piece-offset next  1 1 #'evenp))
      (:R+ (adjust-piece-turn next  1))
      (:R- (adjust-piece-turn next -1)))
    (cond ((is-locking new-board) (lock-down new-board))
	  ((is-bad-move new-board) nil)
	  (t new-board))))

(defun distance (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(defun distances-to-sweetspot (board)
  (let ((sweet (board-spot board)))
    (mapcar (lambda (p) (distance p sweet))
	    (board-active-cells board))))

(defun goodness (board)
  (- (+ *board-height* *board-width*)
     (reduce #'min (distances-to-sweetspot board))

(defun try-move (board)
  (when (not (board-done board))
    (make-move board (random-elt '(:W :E :SW :SE :R+ :R-)))))

(defun best-goodness (index)
  (goodness (aref *best* index)))

(defun find-candidate (fn)
  (let ((candidate 0))
    (dotimes (index *pool-size* candidate)
      (when (funcall fn (best-goodness index) (best-goodness candidate))
	(setf candidate index)))))

(defun find-best ()
  (aref *best* (find-candidate #'>)))

(defun find-worst-index ()
  (find-candidate #'<))

(defun update (victim)
  (let* ((old-board (aref *best* victim))
	 (new-board (try-move old-board)))
    (when (and new-board (> (goodness new-board) (goodness old-board)))
      (setf (aref *best* (find-worst-index)) new-board))
    (update-gui old-board)
    (find-solution)))

(defun find-victim ()
  (let ((offset (random *pool-size*)))
    (dotimes (i *pool-size* nil)
      (let ((victim (mod (+ i offset) *pool-size*)))
	(when (not (board-done (aref *best* victim)))
	  (return-from find-victim victim))))))

(defun find-solution ()
  (let ((victim (find-victim)))
    (if (null victim)
	(find-best)
	(update victim))))

(defun get-solution (board)
  (let ((*best* (make-array *pool-size* :initial-element board)))
    (find-solution)))

(defun git-commit-cmd ()
  "git log -n1 --format=oneline --abbrev-commit --format=\"format:%h\"")

(defun format-commit ()
  (format nil "~A" (asdf::run-program (git-commit-cmd) :output :string)))

(defun get-tag ()
  #-windows-host(format-commit)
  #+windows-host"")

(defun print-solution (board)
  (format nil "~A" (board-cmd board)))

(defun format-solution (id seed board)
  (format t "[ { \"problemId\": ~A~%" id)
  (format t "  , \"seed\": ~A~%" seed)
  (format t "  , \"tag\": \"~A\"~%" (get-tag))
  (format t "  , \"solution\": \"~A\"~%" (print-solution board))
  (format t "  }~%")
  (format t "]~%"))

(defun read-problem (number)
  (with-open-file (problem (format nil "problems/problem_~A.json" number))
    (json:decode-json problem)))

(defun pos-2-cube (point)
  (let* ((x (pos-x point))
	 (y (pos-y point))
	 (xx (- x (/ (- y (logand y 1)) 2))))
    (list xx (- (- xx) y) y)))

(defun cube-2-pos (cube)
  (let ((xx (first cube))
	(zz (third cube)))
    (make-pos :x (+ xx (/ (- zz (logand zz 1)) 2)) :y zz)))

(defun rotate-cube (cube)
  (list (- (third cube)) (- (first cube)) (- (second cube))))

(defun rotate-point (pivot point)
  (let* ((cube-pivot (pos-2-cube pivot))
	 (normalized (mapcar #'- (pos-2-cube point) cube-pivot))
	 (rotated-cube (mapcar #'+ cube-pivot (rotate-cube normalized))))
    (cube-2-pos rotated-cube)))

(defun generate-rotations (pivot config)
  (dotimes (i 5)
    (setf (aref config (1+ i))
	  (mapcar (lambda (point)
		    (rotate-point pivot point))
		  (aref config i)))))

(defun generate-config (pivot members)
  (let ((config (make-array 6)))
    (setf (aref config 0) (mapcar (lambda (x) (create-pos x)) members))
    (generate-rotations pivot config)
    config))

(defun horizontal-offset (left right)
  (- (floor (- *board-width* (1+ (- right left))) 2) left))

(defun calculate-start (config)
  (let ((right 0)
	(left *board-width*)
	(top *board-height*))
    (dolist (point (aref config 0))
      (setf top (min top (pos-y point)))
      (setf left (min left (pos-x point)))
      (setf right (max right (pos-x point))))
    (make-pos :x (horizontal-offset left right) :y (- top))))

(defun parse-units (data)
  (let* ((units (get-item :units data))
	 (result (make-array (length units))))
    (dotimes (i (length result) result)
      (let* ((element (elt units i))
	     (pivot (get-pos :pivot element))
	     (members (get-item :members element))
	     (config (generate-config pivot members))
	     (start (calculate-start config))
	     (piece (make-piece :pivot pivot :config config :offset start)))
	(setf (aref result i) piece)))))

(defun generate-move-sequence ()
  (let ((sequence (make-array *total-moves*)))
    (dotimes (i *total-moves* sequence)
      (setf (aref sequence i) (mod (rnd) (length *units*))))))

(defun init-board-pieces (board number)
  (setf (board-pieces board) (list (aref *units* number))))

(defun debug-board (board)
  (print-board board)
  (format t "~%")
  (sleep 0.2))

(defun make-break-function (with-gui)
  (if (not with-gui)
      ; #'identity
      #'debug-board
      (lambda (board)
	(with-simple-restart (continue-processing "Continue?")
	  (signal 'board-update :new-board board)))))

(defun solution-with-gui (board with-gui)
  (if (not with-gui)
      (get-solution board)
      (run-gui board (lambda () (get-solution board)))))

(defun default-solver (id seed board with-gui)
  (format-solution id seed (solution-with-gui board with-gui)))

(defun solve-problem (number &key with-gui (solver #'default-solver))
  (let* ((data (read-problem number))
	 (*board-width* (get-item :width data))
	 (*board-height* (get-item :height data))
	 (*total-moves* (get-item :source-length data))
	 (*break* (make-break-function with-gui))
	 (*units* (parse-units data))
	 (id (get-item :id data))
	 (new-board (empty-board)))
    (parse-board data new-board)
    (setf (board-spot new-board) (sweet-spot new-board))
    (dolist (source-seed (get-item :source-seeds data))
      (let ((*seed* source-seed))
	(let ((*move-sequence* (generate-move-sequence)))
	  (init-board-pieces new-board (aref *move-sequence* 0))
	  (funcall solver id source-seed new-board with-gui))))))
