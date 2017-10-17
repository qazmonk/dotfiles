(load "~/quicklisp/setup.lisp")
(ql:quickload :inferior-shell)

(defmacro with-gensyms ((&rest names) &body body)
  (let ((varlist nil))
    (dolist (name names)
      (push `(,name (gensym)) varlist))
    `(let ,varlist ,@body)))
(defmacro with-args ((&rest arg-slots) args &body body)
  (with-gensyms (remaining-args)
    (labels ((get-sym (as)
	       (if (listp  (caddr as))
		   (car (caddr as))
		   (caddr as)))
	     (get-dflt (as)
	       (when (listp (caddr as))
		 (cadr (caddr as))))
	     (get-name-list (as)
	       (mapcar (lambda (s) (format nil "~:[-~;--~]~a" 
					   (> (length s) 1)
					   s))
		       (if (listp (car as))
			   (car as)
			   (list (car as)))))
	     (get-type (as)
	       (cadr as))
	     (get-parsefm (as)
	       (if  (listp (cadddr as))
		    (cadddr as)
		    `(,(cadddr as) ,(get-sym as)))))
      `(let (,@(loop for as in arg-slots
		  collecting `(,(get-sym as) ,(get-dflt as))))
	 (do ((,remaining-args ,args))
	     ((not ,remaining-args))
	   (cond ,@(loop for as in arg-slots
		      collecting
			`((or ,@(loop for name in (get-name-list as)
				   collecting 
				     `(string= ,name 
					       (car ,remaining-args))))
			  (setq ,(get-sym as) ,(if (eq :value (get-type as))
						   `(cadr ,remaining-args)
						   't))
			  ,(when (get-parsefm as)
				 `(setq ,(get-sym as) ,(get-parsefm as)))
			  ,(if (eq :value (get-type as))
			       `(setq ,remaining-args (cddr ,remaining-args))
			       `(setq ,remaining-args (cdr ,remaining-args)))
			  (setq ,args ,remaining-args)))
		 (t (setq ,remaining-args (cdr ,remaining-args)))))
	 ,@body))))

(defparameter +copy-if-cmdstr+
  "if [ ! -f \"~a\" ]; then cp \"~a\" \"~a\"; fi")
(defparameter +copy-if-cmdstr-background+
  "if [ ! -f \"~a\" ]; then cp \"~a\" \"~a\"& fi")

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


