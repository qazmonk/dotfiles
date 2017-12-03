(defpackage :script-utils
  (:use :cl)
  (:export
   :with-gensyms
   :with-args
   :component-present-p
   :directory-pathname-p
   :pathname-as-directory))

(in-package :script-utils)

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
