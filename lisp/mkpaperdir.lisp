(load "~/quicklisp/setup.lisp")
(ql:quickload :inferior-shell)
(load "utils/script-utils.lisp")

(defpackage :mkpaperdir
  (:use :cl :script-utils)
  (:export :main))
(in-package :mkpaperdir)

(defparameter +mkdir-cmd+ "mkdir ~a")
(defparameter +mv-cmd+ "mv ~a ~a")
(defparameter +mkorg+ "touch ~a")
(defparameter +open-pdf-cmd+ "okular ~a &")
(defparameter +kill-last-job+ "kill %1")

(defun main (args)
  (with-args
      (("mv" :value top-dir nil)
       ("irename" :boolean irename)) args
    (dolist (filename args)
      (let ((new-name (file-namestring filename)))
       (when irename
	 (format t "New filename: ")
	 (inferior-shell:run (format nil +open-pdf-cmd+ filename))
	 (setq new-name (read-line)))
       (let* ((dir (pathname-as-directory 
		    (make-pathname
		     :defaults filename
		     :directory (if top-dir
				    top-dir
				    (directory-namestring filename))
		     :name (pathname-name (pathname new-name))
		     :type nil)))
	      (new-pathname (merge-pathnames
			     dir
			     (make-pathname
			      :name (pathname-name (pathname new-name))
			      :type (pathname-type (pathname filename))
			      :directory nil)))
	      (notes-pathname (merge-pathnames
			       dir
			       "notes.org")))
	 (format t +mkdir-cmd+ (namestring dir))
	 (inferior-shell:run (format nil +mkdir-cmd+ (namestring dir)))
	 (format t "~%")
	 (format t +mv-cmd+ filename new-pathname)
	 (inferior-shell:run (format nil +mv-cmd+ filename new-pathname))
	 (format t "~%")
	 (format t +mkorg+ notes-pathname)
	 (inferior-shell:run (format nil +mkorg+ notes-pathname))
	 (format t "~%"))))))

(use-package :mkpaperdir :cl-user)
