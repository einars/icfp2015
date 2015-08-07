(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json :icfp/state)
  (:export))

(in-package :icfp/tetris)

(defvar *seed* 0)
(defvar *units* nil)

(defun get-item (item data)
  (cdr (assoc item data)))

(defun create-pos (pos-data)
  (make-pos :x (get-item :x pos-data) :y (get-item :y pos-data)))

(defun get-pos (item data)
  (create-pos (get-item item data)))

(defun parse-board (data board)
  (dolist (locked (get-item :filled data))
    (setf (aref (board-grid board)
		(get-item :x locked)
		(get-item :y locked))
	  1)))

(defstruct piece pivot start config)

(defun pretty-cell (number)
  (case number
    (0 "o")
    (1 "x")))

(defun print-board (board)
  (dotimes (y *board-height*)
    (when (oddp y)
      (format t " "))
    (dotimes (x *board-width*)
      (format t "~A " (pretty-cell (aref (board-grid board) x y))))
    (format t "~%")))

(defun rnd ()
  (prog1 (logand (ash *seed* -16) #x7fff)
    (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 32)))))

(defun get-solution () ; this needs board & units as arguments
  "Ei!")

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

(defun generate-rotations (config)
  config)

(defun generate-config (members)
  (let ((config (make-array 6)))
    (setf (aref config 0) (mapcar (lambda (x) (create-pos x)) members))
    (generate-rotations config)
    config))

(defun parse-units (data)
  (let* ((units (get-item :units data))
	 (result (make-array (length units))))
    (dotimes (i (length result) result)
      (let* ((element (elt units i))
	     (pivot (get-pos :pivot element))
	     (config (generate-config (get-item :members element)))
	     (start nil)
	     (piece (make-piece :pivot pivot :config config :start start)))
	(setf (aref result i) piece)))))

(defun solve-problem (number)
  (let* ((data (read-problem number))
	 (*board-width* (get-item :width data))
	 (*board-height* (get-item :height data))
	 (*units* (parse-units data))
	 (id (get-item :id data))
	 (new-board (empty-board)))
    (parse-board data new-board)
    (format t "~A~%" *units*)
    (dolist (*seed* (get-item :source-seeds data))
      (format-solution id *seed* (get-solution)))))
