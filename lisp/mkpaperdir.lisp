(load "~/quicklisp/setup.lisp")
(ql:quickload :inferior-shell)


(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) 
			      (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defparameter +mkdir-cmd+ "mkdir ~a")
(defparameter +mv-cmd+ "mv ~a ~a")
(defparameter +mkorg+ "touch ~a")

(defun main (args)
  (dolist (filename (rest args))
    (let* ((dir (pathname-as-directory 
		 (make-pathname
		  :defaults filename
		  :type nil)))
	   (new-pathname (merge-pathnames
			  dir
			  (make-pathname
			   :defaults filename
			   :directory nil)))
	   (notes-pathname (merge-pathnames
			    dir
			    "notes.org")))
      (inferior-shell:run (format nil +mkdir-cmd+ (namestring dir)))
      (inferior-shell:run (format nil +mv-cmd+ filename new-pathname))
      (inferior-shell:run (format nil +mkorg+ notes-pathname)))))
