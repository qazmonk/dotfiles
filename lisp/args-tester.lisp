(defmacro with-gensyms ((&rest names) &body body)
  (let ((varlist nil))
    (dolist (name names)
      (push `(,name (gensym)) varlist))
    `(let ,varlist ,@body)))

(with-args ((("v" "verbose") :boolean verbose)
	    ("do-thing" :boolean dont-do-thing (not dont-do-thing))
	    ("do-this-much" :value do-this-much parse-integer)) 
  args 
  (dostuff))

(defmacro with-args ((&rest arg-slots) args &body body)
  (with-gensyms (remaining-args)
    (labels ((get-sym (as)
	       (if (listp  (caddr as))
		   (car (caddr as))
		   (caddr as)))
	     (get-dflt (as)
	       (when (listp (caddr a))
		 (cadr (caddr as))))
	     (get-name-list (as)
	       (mapcar (lambda (s) (format nil "~:[-~;--~]~a" (> (length s) 1)
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
      (let ((syms.dflts (loop for arg-slot in arg-slots 
			   collecting (cons (get-sym arg-slot)
					    (get-dflt arg-slot)))))
	`(let (,@(loop for sym.dflt in syms.dflts
		    collecting `(,(car sym.dflt) ,(cdr sym.dflt))))
	   (do ((,remaining-args ,args))
	       ((not ,remaining-args))
	     (cond ,@(loop for as in arg-slots
			collecting
			  `((or ,@(loop for name in (get-name-list as)
				     collecting `(string= ,name (car ,remaining-args))))
			    (setq ,(get-sym as) ,(if (eq :value (get-type as))
						     `(cadr ,remaining-args)
						     't))
			    ,(when (get-parsefm as)
				   `(setq ,(get-sym as) ,(get-parsefm as)))
			    ,(if (eq :value (get-type as))
				 `(setq ,remaining-args (cddr ,remaining-args))
				 `(setq ,remaining-args (cdr ,remaining-args)))))
		   (t (setq ,remaining-args (cdr ,remaining-args)))))
	   ,@body)))))

(defun main (args)
  "Put shit here to test how command line args are passed"
  (with-args ((("v" "verbose") :boolean verbose)
	      ("do-thing" :boolean doing-thing)
	      ("do-this-much" :value (amount-to-do 0))) args
	      (if doing-thing
		  (format t "I'm doing ~a of the thing~%" amount-to-do)
		  (format t "I'm not doing anything~%"))
	      (when verbose
		(format t "...doing the thing(s)...~%"))))