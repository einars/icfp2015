(load "icfp.asd")
(ql:quickload 'icfp)

(defvar *outfile* "a.out")

(defun main ()
  (destructuring-bind (exe file num) sb-ext:*posix-argv*
    (declare (ignore exe))
    (simulator:play file num)))

(save-lisp-and-die *outfile* :executable t :toplevel 'main)
