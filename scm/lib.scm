(define define-macro 
  (make-macro 
   (lambda (env signature . body)
     `((define ,(car signature) 
	 (make-macro
	  (lambda (,(gensym) ,@(cdr signature))
	    ,@body)))))))

(define-macro (define-struct name slots)
  (define num-slots (length slots))
  `((define (,(string->symbol (string-append "make-" (symbol->string name))))
      (make-vector ,num-slots #n))
    ,@(let ((offset 0))
	(map 
	 (lambda (i)
	   (define func
	     `(define (,(string->symbol (string-append (symbol->string name)
						       "-"
						       (symbol->string i)
						       "-set!"))
		       struct value)
		(vector-set! struct ,offset value)))
	   (set! offset (+ 1 offset))
	   func)
	 slots)
	)
    ,@(let ((offset 0))
	(map 
	 (lambda (i)
	   (define func
	     `(define (,(string->symbol (string-append (symbol->string name)
						       "-"
						       (symbol->string i)
						       "-ref"))
		       struct)
		(vector-ref struct ,offset)))
	   (set! offset (+ 1 offset))
	   func)
	 slots)
	)))
				   
