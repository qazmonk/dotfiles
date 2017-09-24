(load "~/quicklisp/setup.lisp")
(ql:quickload :inferior-shell)





(defmacro with-gensyms ((&rest names) &body body)
  (let ((varlist nil))
    (dolist (name names)
      (push `(,name (gensym)) varlist))
    `(let ,varlist ,@body)))


(defmacro with-args ((&rest arg-slots) args &body body)
   (with-gensyms (remaining-args)
     (flet ((get-sym (as)
	      (if (listp  (caddr as))
		  (car (caddr as))
		  (caddr as)))
	    (get-dflt (as)
	      (when (listp (caddr as))
		(cadr (caddr as))))
	    (get-name-list (as)
	      (mapcar (lambda (s) (format nil "~:[-~;--~]~a" (> (length s) 1)
					  s))
	       (if (listp (car as))
		   (car as)
		   (list (car as)))))
	    (get-type (as)
	      (cadr as)))
      (let ((syms.dflts (loop for arg-slot in arg-slots 
			   collecting (cons (get-sym arg-slot)
					    (get-dflt arg-slot))))
	    (names-typs-syms (loop for arg-slot in arg-slots 
				collecting (list (get-name-list arg-slot)
						 (get-type arg-slot)
						 (get-sym arg-slot)))))
	`(let (,@(loop for sym.dflt in syms.dflts
		    collecting `(,(car sym.dflt) ,(cdr sym.dflt))))
	   (do ((,remaining-args ,args))
	       ((not ,remaining-args))
	     (cond ,@(loop for names-typ-sym in names-typs-syms
			collecting
			  `((or ,@(loop for name in (car names-typ-sym)
				     collecting `(string= ,name (car ,remaining-args))))
			    (setq ,(caddr names-typ-sym) ,(if (eq :value (cadr names-typ-sym))
							      `(cadr ,remaining-args)
							      't))
			    ,(if (eq :value (cadr names-typ-sym))
				 `(setq ,remaining-args (cddr ,remaining-args))
				 `(setq ,remaining-args (cdr ,remaining-args)))))
		   (t (setq ,remaining-args (cdr ,remaining-args)))))
	   ,@body)))))


(defun main (args)
  (with-args ((("v" "verbose") verbose)
	      ("tmp-dir" tmp-dir))
    (dolist (file (rest args))
      (format t "vlc --fullscreen --play-and-exit \"~a\"" file)
      (inferior-shell:run (format nil "vlc --fullscreen --play-and-exit \"~a\"" file)))))


