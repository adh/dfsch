(provide 'lib)

(define-macro (define-struct name slots)
  (define num-slots (length slots))
  
  (define slot-names (map 
                      (lambda (slot-desc) 
                        (if (pair? slot-desc)
                            (car slot-desc)
                            slot-desc))
                      slots))

  (define slot-defaults (map 
                         (lambda (slot-desc)
                           (if (pair? slot-desc)
                               (car (cdr slot-desc))
                               #n))
                         slots))

  `(begin 
     (define (,(string->symbol (string-append "make-" (symbol->string name)))
             . args)
       (define self (list->vector ',slot-defaults))
       (let loop ((i args))
	 (if (pair? i)
	     (let ((name (car i)) (value (car (cdr i))))
	       (vector-set! self 
			    (case name
			      ,@(let ((offset 0))
				  (map 
				   (lambda (field)
				     (define cls `((,field) ,offset))
				     (set! offset (+ 1 offset))
				     cls)
				   slot-names))
                             (else (throw 'struct:no-such-slot name)))
			    value)
	       (loop (cdr (cdr i))))
	     ()))
       self)
     ,@(let ((offset 0))
	 (map 
	  (lambda (field)
	    (define func
	      `(define (,(string->symbol (string-append (symbol->string name)
							"-"
							(symbol->string field)
							"-set!"))
			struct value)
		 (vector-set! struct ,offset value)))
	    (set! offset (+ 1 offset))
	    func)
	  slot-names)
	 )
     ,@(let ((offset 0))
	 (map 
	  (lambda (field)
	    (define func
	      `(define (,(string->symbol (string-append (symbol->string name)
							"-"
							(symbol->string field)
							"-ref"))
			struct)
		 (vector-ref struct ,offset)))
           (set! offset (+ 1 offset))
           func)
	  slot-names)
	 )))

