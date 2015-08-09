(defpackage :icfp/tetris
  (:nicknames :tetris)
  (:use :cl :cl-json :icfp/state :icfp/gui)
  (:export :solve-problem
	   :get-next-board
	   :update-gui
	   :get-item
	   :make-move
	   :*move-sequence*
	   :*units*))

(in-package :icfp/tetris)

(defvar *seed* 0)
(defvar *units* nil)
(defvar *break* nil)
(defvar *best* nil)

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

(defun update-board-height (board &key absolute diff)
  (when absolute (setf (first (board-stats board)) absolute))
  (when diff (incf (first (board-stats board)) diff)))

(defun get-board-height (board)
  (first (board-stats board)))

(defun parse-board (data board)
  (update-board-height board :absolute *board-height*)
  (dolist (locked (get-item :filled data))
    (let ((x (get-item :x locked))
	  (y (get-item :y locked)))
      (update-board-height board :absolute y)
      (setf (aref (board-grid board) x y) 1))))

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

(defun duplicate-internals (board)
  (setf (board-stats board) (copy-list (board-stats board)))
  (setf (board-grid board) (copy-grid board))
  board)

(defun clone-board (board)
  (duplicate-internals (copy-board board)))

(defun rnd ()
  (prog1 (logand (ash *seed* -16) #x7fff)
    (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 32)))))

(defun adjust-piece-offset (piece x y fn)
  (when (funcall fn (pos-y (piece-offset piece))) (setf x 0))
  (incf (pos-x (piece-offset piece)) x)
  (incf (pos-y (piece-offset piece)) y))

(defun adjust-piece-turn (piece turn)
  (setf (piece-turn piece) (mod (+ (piece-turn piece) turn) 6)))

(defun free-cell (board cell)
  (= 0 (aref (board-grid board) (car cell) (cdr cell))))

(defun good-cell (board cell)
  (and (cell-on-board cell) (free-cell board cell)))

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
  (incf (board-lines board))
  (let ((grid (board-grid board)))
    (dotimes (x row)
      (dotimes (i *board-width*)
	(setf (aref grid i (- row x)) (aref grid i (- row (+ x 1))))))
    (dotimes (i *board-width*)
      (setf (aref grid i 0) 0))))

(defun test-and-update-if-full (row board score)
  (when (is-row-full row board)
    (update-board-height board :diff 1)
    (incf (score-lines score))
    (delete-row row board)))

(defun lock-piece-and-update-score (board)
  (duplicate-internals board)
  (let ((rows nil) (score (make-score :history (board-pieces board))))
    (push score (board-log board))
    (dolist (i (board-active-cells board))
      (setf (aref (board-grid board) (car i) (cdr i)) 1)
      (let ((height (min (get-board-height board) (cdr i))))
	(update-board-height board :absolute height))
      (incf (score-size score))
      (push (cdr i) rows))
    (dolist (i (sort (remove-duplicates rows) #'<))
      (test-and-update-if-full i board score))))

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

(defun try-move (board move)
  (update-gui board)
  (when (not (board-done board))
    (make-move board move)))

(defvar *rank* nil)
(defvar *patterns* nil)

(defun board-rank (board)
  (piece-number (last-move board)))

(defun advance-try (board cmds turns)
  (try-cmds (try-move board (first cmds)) (rest cmds) turns))

(defun try-turn (board cmds turns)
  (let ((try (try-move board :R+)))
    (if (and try (= (board-rank try) *rank*))
	(try-cmds try cmds (1- turns))
	(advance-try board cmds turns))))

(defun try-cmds (board cmds turns)
  (cond ((or (null board) (null cmds)) nil)
	((> (board-rank board) *rank*) board)
	((> turns 0) (try-turn board cmds turns))
	(t (advance-try board cmds turns))))

(defun generate-movement-patterns ()
  (let ((patterns nil)
	(eastward nil)
	(westward nil)
	(downwardE nil)
	(downwardW nil))
    (dotimes (i (1+ *board-height*))
      (push (if (oddp i) :SE :SW) downwardE)
      (push (if (oddp i) :SW :SE) downwardW))
    (dotimes (i (1+ (ceiling *board-width* 2)))
      (push :E eastward)
      (push :W westward)
      (push (append eastward downwardE) patterns)
      (push (append westward downwardE) patterns)
      (push (append eastward downwardW) patterns)
      (push (append westward downwardW) patterns))
    patterns))

(defun try-all-moves (board)
  (let ((collect nil))
    (dotimes (turns 5 (remove nil collect))
      (labels ((try (cmds) (try-cmds board cmds turns)))
	(setf collect (append (mapcar #'try *patterns*) collect))))))

(defun NW-cell (x y)
  (cons (- x (- 1 (mod y 2))) (- y 1)))

(defun NE-cell (x y)
  (cons (+ x (mod y 2)) (- y 1)))

(defun count-holes (board)
  (let ((count 0))
    (dotimes (x *board-width*)
      (dotimes (y *board-height*)
	(when (and (free-cell board (cons x y))
		   (good-cell board (NW-cell x y))
		   (good-cell board (NE-cell x y)))
	  (incf count))))
    (setf (second (board-stats board)) count)))

(defun hole-count (board)
  (second (board-stats board)))

(defun leave-best (pool predicate key)
  (let* ((sorted-by (sort pool predicate :key key))
	 (best-result (funcall key (first sorted-by))))
    (remove-if-not (lambda (x) (= (funcall key x) best-result)) sorted-by)))

(defun sift (pool conditions)
  (dolist (i conditions pool)
    (setf pool (leave-best pool (car i) (cdr i)))))

(defun best-of (pool)
  (when pool
    (mapc #'count-holes pool)
    (first (sift pool (list (cons #'> #'board-lines)
			    (cons #'> #'get-board-height)
			    (cons #'> #'hole-count))))))

(defun get-solution (board)
  (dotimes (*rank* *total-moves* board)
    (let* ((all-moves (try-all-moves board))
	   (best-move (best-of all-moves)))
      (if best-move
	  (setf board best-move)
	  (return-from get-solution board)))))

(defun git-commit-cmd ()
  "git log -n1 --format=oneline --abbrev-commit --format=\"format:%h\"")

(defun format-commit ()
  (format nil "~A" (asdf::run-program (git-commit-cmd) :output :string)))

(defun get-tag ()
  #-windows-host(format-commit)
  #+windows-host"")

(defun pretty-cmd (cmd)
  (case cmd
    (:W  "p")
    (:E  "b")
    (:SW "a")
    (:SE "l")
    (:R+ "d")
    (:R- "k")))

(defun print-solution (s board)
  (dolist (cmd (reverse (board-cmd board)))
    (format s "~A" (pretty-cmd cmd))))

(defun format-solution (id seed board)
  (with-open-file  (s "test.json" :direction :output :if-exists :supersede)
    (format s "[ { \"problemId\": ~A~%" id)
    (format s "  , \"seed\": ~A~%" seed)
    (format s "  , \"tag\": \"~A\"~%" (get-tag))
    (format s "  , \"solution\": \"")
    (print-solution s board)
    (format s "\"~%  }~%")
    (format s "]~%")))

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
      #'identity
      ; #'debug-board
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
	 (*patterns* (generate-movement-patterns))
	 (*break* (make-break-function with-gui))
	 (*units* (parse-units data))
	 (id (get-item :id data))
	 (new-board (empty-board)))
    (parse-board data new-board)
    (dolist (source-seed (get-item :source-seeds data))
      (let ((*seed* source-seed))
	(let ((*move-sequence* (generate-move-sequence)))
	  (init-board-pieces new-board (aref *move-sequence* 0))
	  (funcall solver id source-seed new-board with-gui))))))
