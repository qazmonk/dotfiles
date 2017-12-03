(load "~/quicklisp/setup.lisp")
(ql:quickload :inferior-shell)
(load "script-utils.lisp")

(defpackage :playlist
  (:use :cl :script-utils)
  (:export :main))

(in-package :playlist)

(defparameter +copy-if-cmdstr+
  "if [ ! -f \"~a\" ]; then cp \"~a\" \"~a\"; fi")
(defparameter +copy-if-cmdstr-background+
  "if [ ! -f \"~a\" ]; then cp \"~a\" \"~a\"& fi")



(defun main (args)
  (declare (optimize (debug 3) (speed 0)))
  (with-args ((("v" "verbose") :boolean verbose)
	      ("tmp-dir" :value tmp-dir pathname)) 
    args
    (flet ((make-tmp-filename (filename)
	     (make-pathname
	      :defaults (pathname-as-directory tmp-dir)
	      :name (pathname-name filename) 
	      :type (pathname-type filename))))
     (when (and tmp-dir)
       (inferior-shell:run (format nil +copy-if-cmdstr+ 
				   (make-tmp-filename (car args)) 
				   (namestring (pathname (car args)))
				   (make-tmp-filename (car args)))
			   :on-error nil)
       (dolist (filename (rest args))
	 (inferior-shell:run (format nil +copy-if-cmdstr-background+ 
				     (make-tmp-filename filename) 
				     filename (make-tmp-filename filename)
				     :on-error nil))))
     (let ((last-filename))
      (dolist (filename args)
	(when verbose
	  (format t "vlc --fullscreen --play-and-exit \"~a\"" filename))
	(if tmp-dir
	    (progn
	      (when last-filename
		(inferior-shell:run 
		 (format nil "rm \"~a\""
			 (make-tmp-filename last-filename))))
	      (inferior-shell:run 
	       (format nil "vlc --fullscreen --play-and-exit \"~a\""
		       (make-tmp-filename filename)))
	      (setq last-filename filename))
	    
	    (inferior-shell:run 
	     (format nil "vlc --fullscreen --play-and-exit \"~a\"" 
		     filename))))
      (when last-filename
		(inferior-shell:run 
		 (format nil "rm \"~a\""
			 (make-tmp-filename last-filename))))))))


