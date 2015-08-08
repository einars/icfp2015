(defpackage :icfp/simulator
  (:nicknames :simulator)
  (:use :cl :alexandria :cl-json :icfp/state :icfp/gui :icfp/tetris)
  (:export :play))

(in-package :icfp/simulator)

(defvar *last-log*)
(defvar *score*)

(defun read-simulation (file)
  (with-open-file (problem file)
    (json:decode-json problem)))

(defun play (file result-num)
  (let* ((result (nth result-num (read-simulation file))))
    (solve-problem (get-item :problem-id result) :solver (gen-simulator (get-item :seed result) (get-item :solution result)) :with-gui t)))

(defun gen-simulator (simultation-seed solution)
  (lambda (id seed board with-gui)
    (declare (ignore id with-gui))
    (let ((*last-log* nil)
	  (*score* 0))
      (when (eql seed simultation-seed)
	(run-gui board (lambda () (read-cmds board solution 0)))))))

(defun decode-move (solution index)
  (let ((move-code (aref solution index)))
    (cond
      ((find move-code "p'!.03P") (values :W (1+ index)))
      ((find move-code "becfy2BECFY") (values :E (1+ index)))
      ((find move-code "aghij4AGHIJ") (values :SW (1+ index)))
      ((find move-code "lmno 5LMNO") (values :SE (1+ index)))
      ((find move-code "dqrvz1DQRVZ") (values :R+ (1+ index)))
      ((find move-code "kstuwxKSTUWX") (values :R- (1+ index)))
      ((find move-code (list #\Tab #\Linefeed #\Return)) (decode-move solution (1+ index)))
      (t (error "Unknown move ~A" move-code)))))

(defun read-cmds (board solution index)  
  (update-gui board)
  (multiple-value-bind (move next-index) (decode-move solution index)
    (let* ((next-board (make-move board move))
	   (log (board-log next-board)))
      (when (and log (not (eq log *last-log*)))
	(setf *last-log* log)
	(incf *score* (score-size (car log)))
	(with-simple-restart (continue-processing "Continue?")
	  (error 'board-update :debug-msg (format nil "~A" *score*))))
      (read-cmds next-board solution next-index))))
