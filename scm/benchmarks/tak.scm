#!/usr/bin/env dfsch-repl

(require 'gcollect)

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (tak-inline x y z)
  (#.if (#.not (#.< y x))
      z
      (tak-inline (tak-inline (#.- x 1) y z)
                  (tak-inline (#.- y 1) z x)
                  (tak-inline (#.- z 1) x y))))


(define-macro (measure-time name . body)
  (let ((start-run (gensym)) (start-real (gensym)) (start-bytes (gensym)))
    `(let ((,start-real (get-internal-real-time))
           (,start-run (get-internal-run-time))
           (,start-bytes (gcollect:total-bytes)))
       (print ">>> " ',name)
       ,@body
       (print "<<< " ',name 
              " real: " (* 1.0 (/ (- (get-internal-real-time)
                              ,start-real)
                           internal-time-units-per-second))
              " run: " (* 1.0 (/ (- (get-internal-run-time)
                                ,start-run)
                          internal-time-units-per-second))
              " cons'd: " (- (gcollect:total-bytes)
                             ,start-bytes)))))

(define-macro (without-gc . thunk)
  `(begin
     (gcollect:disable!)
     (unwind-protect
      (begin
        ,@thunk)
      (gcollect:enable!)
      (gcollect:gcollect!))))
   

(measure-time "tak" (tak 24 16 8))
(measure-time "takfp" (tak 24.0 16.0 8.0))
(measure-time "tak-inline" (tak-inline 24 16 8))
(without-gc 
 (measure-time "nogc-tak" (tak 24 16 8)))
(without-gc 
 (measure-time "nogc-takfp" (tak 24.0 16.0 8.0)))
(without-gc 
 (measure-time "nogc-tak-inline" (tak-inline 24 16 8)))