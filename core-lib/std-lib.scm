;;; dfsch - Scheme-like Lisp dialect
;;;   Standard macros
;;; Copyright (c) 2010, 2011 Ales Hakl
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

;; General state of interpreter is unknown when this gets loaded
(dfsch:define-package :dfsch%implementation 
                      :uses '(:dfsch :dfsch%internal)
                      :exports '()
                      :documentation 
                      "Internal package used by self-hosted standard library")

(dfsch:in-package :dfsch%implementation)

;; Exported symbols are explicitly placed into dfsch package, 
;; dfsch%implementation is not intended to be directly used by user code
;; dfsch%internal presents low level functionality required to implement some
;; core functionality in scheme code

(define-macro (dfsch:with-gensyms gensyms &body body)
  `@(let ,(map (lambda (name) 
                 `(,name (make-symbol ',(symbol-name name)))) 
               gensyms)
      ,@body))

(define-macro (dfsch:loop &body exprs)
  (with-gensyms (tag)
    `@(catch ',tag
             (let ()
               (define (dfsch:break value) (throw ',tag value))
               (%loop ,@exprs)))))

(define-macro (dfsch:multiple-value-bind variables values-form &body body)
  `(destructuring-bind (&optional ,@variables &rest ,(gensym))
                       (%get-values ,values-form)
                       ,@body))
