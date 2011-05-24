(require :dfsch-unit)
(use-package :dfsch-unit)

(define-macro (define-evaluation-test name categories &rest exprs)
  `(let ()
     (define-test ,name ,categories
       ,@(map (lambda (e)
                (if (and (pair? e) (cdr e) (eq? (cadr e) '===>))
                    (begin
                      (when (not (cddr e))
                            (error "Expected result missing" :clause e))
                      (when (cdddr e)
                            (error "Too many expressions in clause" :clause e))
                      `(assert-equal ,(car e) ',(caddr e)))
                    e))
              exprs))))
 
(require :language-tests)
(require :r5rs-tests)
(require :fix-regression-tests)

(test-toplevel)