(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json)
  (:export))

(in-package :icfp/tetris)

(defvar *seed* 0)
(defvar *units* nil)
(defvar *width* nil)
(defvar *height* nil)

(defstruct board grid)

(defstruct piece pivot start cfg)

(defun pretty-cell (number)
  (case number
    (0 "o")
    (1 "x")))

(defun print-board (board)
  (dotimes (y *height*)
    (when (oddp y)
      (format t " "))
    (dotimes (x *width*)
      (format t "~A " (pretty-cell (aref (board-grid board) x y))))
    (format t "~%")))

(defun empty-board ()
  (let ((board (make-board)))
    (setf (board-grid board) (make-array (list *width* *height*)))
    board))

(defun get-item (item data)
  (cdr (assoc item data)))

(defun parse-board (data board)
  (dolist (locked (get-item :filled data))
    (setf (aref (board-grid board)
		(get-item :x locked)
		(get-item :y locked))
	  1)))

(defun rnd ()
  (prog1 (logand (ash *seed* -16) #x7fff)
    (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 32)))))

(defun get-solution () ; this needs board & units as arguments
  "Ei!")

(defun git-commit-cmd ()
  "git log -n1 --format=oneline --abbrev-commit --format=\"format:%h\"")

(defun get-tag ()
  (format nil "~A" (asdf::run-program (git-commit-cmd) :output :string)))

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

(defun parse-units (data)
  (let* ((units (get-item :units data))
	 (result (make-array (length units))))
    (dotimes (i (length result) result)
      (let* ((element (elt units i))
	     (pivot (get-item :pivot element)))
	(setf (aref result i) (make-piece :pivot pivot))))))

(defun solve-problem (number)
  (let* ((data (read-problem number))
	 (*width* (get-item :width data))
	 (*height* (get-item :height data))
	 (*units* (parse-units data))
	 (id (get-item :id data))
	 (new-board (empty-board)))
    (parse-board data new-board)
    (dolist (*seed* (get-item :source-seeds data))
      (format-solution id *seed* (get-solution)))))
