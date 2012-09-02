#!/usr/bin/env dfsch-repl

(require 'gcollect)

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(define-macro (measure-time name . body)
  (let ((start-run (gensym)) (start-real (gensym)) (start-bytes (gensym)))
    `(let ((,start-real (get-internal-real-time))
           (,start-run (get-internal-run-time))
           (,start-bytes (gc-total-bytes)))
       (print ">>> " ',name)
       ,@body
       (print "<<< " ',name 
              " real: " (* 1.0 (/ (- (get-internal-real-time)
                              ,start-real)
                           internal-time-units-per-second))
              " run: " (* 1.0 (/ (- (get-internal-run-time)
                                ,start-run)
                          internal-time-units-per-second))
              " cons'd: " (- (gc-total-bytes)
                             ,start-bytes)))))

(define-macro (without-gc &rest thunk)
  `(begin
     (gc-disable!)
     (unwind-protect
      (begin
        ,@thunk)
      (gc-enable!)
      (gc-collect!))))
   
(define (many-add-fix-fun)
  (for-each (lambda (x) (+ x x)) (make-number-sequence :to 10000000)))

(measure-time "many-add-fix" (for-each (lambda (x) (+ x x)) (make-number-sequence :to 10000000)))
(measure-time "many-add-fix-fun" (many-add-fix-fun))
(measure-time "many-add-fix-fun" (many-add-fix-fun))
(measure-time "many-add-flo" (for-each (lambda (x) (+ x pi)) (make-number-sequence :to 10000000)))
