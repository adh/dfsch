;;; Simple message-delegation based object oriented framework
;;; 
;;; It looks like good example to me.


;; Proposed root of class hierarchy
(define (Object) (lambda (selector) 
		   (throw (DoesNotUnderstand selector))))

;; Utility function for dispatching messages
(define (message-dispatch parent-object message-list)
  (lambda (selector)
    (define (loop message-list)
      (cond ((null? message-list) (parent-object selector))
	    ((= selector (car (car message-list))) (car (cdr (car message-list))))
	    (else (loop (cdr message-list)))))
    (loop message-list)))


;; Exception handling

(define (of-throw what)
  (throw 'ObjectFrameworkException what))

(define (Exception data)
  (define (data)
    data)
  (define (setData: new)
    (set! data new))
  (define (identify)
    'Exception)
  (message-dispatch (Object) (list (list 'data data)
				   (list 'setData: setData:)
				   (list 'identify identify))))

