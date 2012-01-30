(define-evaluation-test let-shadowing (:language :compiler)
  ((let ((exp :local))
     exp)   ===> :local))

(define-evaluation-test argument-shadowing (:language :compiler)
  (((lambda (exp) exp) :local) ===> :local))

(define-evaluation-test local-shadowing (:language :compiler)
  ((begin
     (define exp :local)
     exp) ===> :local))

(define-evaluation-test destructuring-bind-shadowing (:language :compiler)
  ((destructuring-bind (exp) '(:local)
                       exp) ===> :local))