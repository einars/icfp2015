(defpackage :icfp/gui
  (:nicknames :gui)
  (:use :cl :alexandria :ltk)
  (:export))

(in-package :icfp/gui)

(defun honeycomb)

(defun hello-1()
  (with-ltk ()
    (let* ((board-scrolled-canvas (make-instance 'scrolled-canvas
					))
	   (board-canvas (canvas board-scrolled-canvas))
	   (button-bar (make-instance 'frame
				      :width 300))
	   (click (make-instance 'button 
				 :master button-bar
				 :text "Press Me"
				 :command (lambda ()
					    (format t "Hello World!~&")))))
      (pack board-scrolled-canvas)
      (pack button-bar :side :top)
      (pack click :side :left))))
