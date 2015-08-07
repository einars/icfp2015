(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json)
  (:export))

(in-package :icfp/tetris)

(defvar *width* nil)
(defvar *height* nil)

(defstruct board grid)

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

(defun read-problem (number)
  (with-open-file (problem (format nil "problems/problem_~A.json" number))
    (let* ((data (json:decode-json problem))
	   (*width* (get-item :width data))
	   (*height* (get-item :height data))
	   (new-board (empty-board)))
      (parse-board data new-board)
      (print-board new-board))))
