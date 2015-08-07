(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json :icfp/state :icfp/gui)
  (:export))

(in-package :icfp/tetris)

(defvar *seed* 0)
(defvar *units* nil)
(defvar *break* nil)

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
    (6 "@")))

(defun print-raw-board (board)
  (dotimes (y *board-height*)
    (when (oddp y)
      (format t " "))
    (dotimes (x *board-width*)
      (format t "~A " (pretty-cell (aref (board-grid board) x y))))
    (format t "~%")))

(defun print-board (vanilla-board)
  (let* ((board (clone-board vanilla-board))
	 (pivot (board-pivot vanilla-board)))
    (dolist (i (board-active-cells vanilla-board))
      (setf (aref (board-grid board) (car i) (cdr i)) 2))
    (incf (aref (board-grid board) (car pivot) (cdr pivot)) 4)
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

(defun make-move (board move)
  (let* ((new-board (copy-board board))
	 (next (copy-piece (last-move board))))
    (push next (board-pieces new-board))
    (case move
      (W  (adjust-piece-offset next -1 0 #'null))
      (E  (adjust-piece-offset next  1 0 #'null))
      (SW (adjust-piece-offset next -1 1 #'oddp))
      (SE (adjust-piece-offset next  1 1 #'evenp))
      (R+ (adjust-piece-turn next  1))
      (R- (adjust-piece-turn next -1)))
    new-board))

(defun get-solution (board)
  (update-gui board)
  (get-solution (make-move board (random-elt '(W E SW SE R+ R-)))))

(defun git-commit-cmd ()
  "git log -n1 --format=oneline --abbrev-commit --format=\"format:%h\"")

(defun format-commit ()
  (format nil "~A" (asdf::run-program (git-commit-cmd) :output :string)))

(defun get-tag ()
  #-windows-host(format-commit)
  #+windows-host"")

(defun format-solution (id seed solution)
  (format t "[ { \"problemId\": ~A~%" id)
  (format t "  , \"seed\": ~A~%" seed)
  (format t "  , \"tag\": \"~A\"~%" (get-tag))
  (format t "  , \"solution\": \"~A\"~%" solution)
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

(defun generate-move-sequence (&optional (i *total-moves*))
  (when (> i 0)
    (cons (mod (rnd) (length *units*)) (generate-move-sequence (- i 1)))))

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

(defun solve-problem (number &key with-gui)
  (let* ((data (read-problem number))
	 (*board-width* (get-item :width data))
	 (*board-height* (get-item :height data))
	 (*total-moves* (get-item :source-length data))
	 (*break* (make-break-function with-gui))
	 (*units* (parse-units data))
	 (id (get-item :id data))
	 (new-board (empty-board)))
    (parse-board data new-board)
    (dolist (*seed* (get-item :source-seeds data))
      (let ((*move-sequence* (generate-move-sequence)))
	(init-board-pieces new-board (first *move-sequence*))
	(format-solution id *seed* (solution-with-gui new-board with-gui))))))
