;;; dfsch - Scheme-like Lisp dialect
;;;   Pattern matching
;;; Copyright (c) 2010 Ales Hakl
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(provide :match)
(define-package :match
  :uses '(:dfsch)
  :exports '(:match
             :ensure-variable
             :expand-clause
             :register-clause-symbol!))
(in-package :match)

(define-constant *uninitialized* (gensym))

(define-class <variable-list> ()
  ((variables :reader variable-list :initform ())))

(define-method (ensure-variable (vl <variable-list>) name)
  (unless (memq name (variable-list vl))
          (slot-set! vl :variables
                     (cons name (variable-list vl)))))

(define *clause-expanders* (make-identity-hash))

(define (register-clause-expander! sym proc)
  (map-set! *clause-symbols* proc))

(define-method (expand-clause clause object variables)
  `(equal? ',clause ,object))

(define-method (expand-clause (clause <symbol>) object variables)
  (cond ((eq? clause :?) #t)
        ((keyword? clause) `(equal? ',clause ,object))
        (else (ensure-variable variables clause)
              `(cond ((eq? ',*uninitialized* ,clause) 
                      (set! ,clause ,object) 
                      #t)
                     ((equal? ,clause ,object) #t)
                     (else #f)))))

(define-method (expand-clause (clause <empty-list>) object variables)
  `(null? ,object))

(define-method (expand-clause (clause <list>) object variables)
  (let ((expander (map-ref *clause-expanders* (car clause))))
    (if expander
        (expander clause object variables)
        (with-gensyms (tmp)
          `(let ((,tmp ,object))
             (and (list? ,tmp)
                  ,@(mapcan (lambda (cls)
                              (with-gensyms (item)
                                `((pair? ,tmp)
                                  (let ((,item (car ,tmp)))
                                    ,(expand-clause cls item variables))
                                  (begin (set! ,tmp (cdr ,tmp))
                                         #t))))
                            clause)))))))

(define-macro (match object &rest clauses)
  (with-gensyms (obj tag)
    `(catch ',tag
       (let ((,obj ,object))
         ,@(map (lambda (clause)
                  (unless (pair? clause)
                    (error "Pattern expected" :object clause))
                  (let ((pattern (car clause))
                        (body (cdr clause)))
                    (letrec ((vars (make-instance <variable-list>))
                             (expansion (expand-clause pattern obj vars)))
                      `(let ,(map (lambda (v) `(,v ',*uninitialized*))
                                  (variable-list vars))
                         (when ,expansion (throw ',tag 
                                                 (begin ,@body)))))))
                clauses)))))

                   
        
