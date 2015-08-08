(load "icfp.asd")
(ql:quickload 'icfp)

(defvar *outfile* "a.out")

(defun main ()
  (destructuring-bind (file num) sb-ext:*posix-argv*
    (simulator:play file num)))

(save-lisp-and-die *outfile* :executable t :toplevel 'main)
