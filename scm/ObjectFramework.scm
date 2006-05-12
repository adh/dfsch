(define define-macro 
  (make-macro 
   (lambda (env signature . body)
     `((define ,(car signature) 
	 (make-macro
	  (lambda (,(gensym) ,@(cdr signature))
	    ,@body)))))))


				   
