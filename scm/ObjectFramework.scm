;;; Simple message-delegation based object oriented framework
;;; 
;;; It looks like good example to me.


;; Proposed root of class hierarchy
(define (Object) (lambda (selector) 'doesNotUnderstand))

;; Utility function for dispatching messages
(define (message-dispatch parent-object message-list)
  (lambda (selector)
    (define (loop message-list)
      (cond ((null? message-list) (parent-object selector))
	    ((= selector (car (car message-list))) (car (cdr (car message-list))))
	    (else (loop (cdr message-list)))))
    (loop message-list)))


