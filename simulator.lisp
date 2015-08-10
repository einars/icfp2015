(defpackage :icfp/simulator
  (:nicknames :simulator)
  (:use :cl :alexandria :cl-json :icfp/state :icfp/gui :icfp/tetris)
  (:export :play :gen-simulator :play-move))

(in-package :icfp/simulator)

(defparameter *power-words*
  (list "ia! ia!"
	"r'lyeh"
	"yogsothoth"
	"yuggoth"
	"ei!"))

(defvar *last-log*)
(defvar *score*)
(defvar *last-bonus*)
(defvar *last-clear-no*)
(defvar *last-clear-lines*)
(defvar *curr-unit-no*)

(defvar *power-moves*)
(defvar *last-full-move*)
(defvar *found-words*)
(defvar *last-power-bonus*)

(defun read-simulation (file)
  (with-open-file (problem file)
    (json:decode-json problem)))

(defun play (file result-num)
  (handler-case
      (let* ((result (nth result-num (read-simulation file))))
	(solve-problem (get-item :problem-id result)
		       :solver (gen-simulator (lambda (board)
						(read-cmds board (get-item :solution result) 0))
					      :filter-seed (get-item :seed result))
		       :with-gui t))
    (done () nil)))

(defun gen-simulator (solver &key filter-seed)
  (lambda (id seed board with-gui)
    (declare (ignore id with-gui))
    (let ((*last-log* nil)
	  (*score* 0)
	  (*last-bonus* 0)
	  (*last-clear-no* -100)
	  (*last-clear-lines* 0)
	  (*curr-unit-no* 0)
	  (*last-full-move* nil)
	  (*found-words* nil)
	  (*power-moves* (mapcar #'decode-word *power-words*)))
      (when (or (not filter-seed) (eql seed filter-seed))
	(run-gui board (lambda () (funcall solver board)))))))

(defun decode-word (word)
  (loop as i below (length word)
    collect (decode-move word i)))

(defun decode-move (solution index)
  (when (>= index (length solution))
    (signal 'done))
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
  (multiple-value-bind (move next-index) (decode-move solution index)    
    (read-cmds (play-move board move) solution next-index)))

(defun play-move (board move)
  (update-gui board)
  (let* ((next-board (make-move board move))
	 (log (board-log next-board)))
    (push move *last-full-move*)
    (when (and log (not (eq log *last-log*)))
      (setf *last-log* log)
      (incf *curr-unit-no*)
      (let* ((score-entry (first log))
	     (clear-bonus (* 50 (1+ (score-lines score-entry)) (score-lines score-entry)))
	     (points (+ (score-size score-entry) clear-bonus))
	     (line-bonus (if (and (not (zerop clear-bonus))
				  (eql *last-clear-no* (1- *curr-unit-no*)))
			     (floor (* (+ points *score*) (1- *last-clear-lines*) 0.1))
			     0))
	     (power-bonus (calc-power-bonus)))
	(setf *last-full-move* nil
	      *last-bonus* (+ line-bonus clear-bonus)
	      *last-power-bonus* power-bonus)
	(unless (zerop clear-bonus)
	  (setf *last-clear-no* *curr-unit-no*
		*last-clear-lines* (score-lines score-entry)))
	(incf *score* (+ points line-bonus power-bonus)))
      (with-simple-restart (continue-processing "Continue?")
	(signal 'board-update :debug-msg (format nil "N: ~3D     PW: ~D     S: ~D     LB: ~D     PB: ~D"
						 *curr-unit-no* (length *found-words*) *score* *last-bonus* *last-power-bonus*))))
    next-board))

(defun calc-power-bonus ()
  (let ((power-bonus 0)
	(fw-move (reverse *last-full-move*)))
    (dolist (power-word *power-moves*)
      (let ((repeats (find-ocurrences power-word fw-move)))
	(incf power-bonus (* 2 repeats (length power-word)))
	(when (and (> repeats 0)
		   (not (find power-word *found-words*)))
	  (push power-word *found-words*)
	  (incf power-bonus 300))))
    power-bonus))

(defun find-ocurrences (word move &optional (found 0) (last-pos 0))
  (if-let ((found-pos (search word move :start2 last-pos)))
    (find-ocurrences word move (1+ found) (1+ found-pos))
    found))
