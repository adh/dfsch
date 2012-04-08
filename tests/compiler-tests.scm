(define-evaluation-test let-shadowing (:language :compiler)
  ((let ((exp :local))
     exp)   ===> :local))

(define-evaluation-test letrec-shadowing (:language :compiler)
  ((letrec ((exp :local))
     exp)   ===> :local))

(define-evaluation-test named-let-shadowing (:language :compiler)
  ((let ((my-var :outer))
     (let my-var ()
       (procedure? my-var)))     ===> #t)
  ((let pi ()
     (procedure? pi))            ===> #t))

(define-evaluation-test argument-shadowing (:language :compiler)
  (((lambda (exp) exp) :local) ===> :local)
  (((lambda (&aux (exp :local)) exp)) ===> :local))

(define-evaluation-test local-shadowing (:language :compiler)
  ((begin
     (define exp :local)
     exp) ===> :local))

(define-evaluation-test destructuring-bind-shadowing (:language :compiler)
  ((destructuring-bind (exp) '(:local)
                       exp) ===> :local))

(define-test declare (:language :compiler)
  (define foo 1)
  (declare foo :type <fixnum>)
  (assert-true #t))
