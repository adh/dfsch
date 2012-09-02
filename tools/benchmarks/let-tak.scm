#!/usr/bin/env dfsch-repl

(require 'gcollect)
(use-package :dfsch%internal)

(define (print . args)
  (for-each (lambda (i) (display i)) args)
  (newline))

(define (tak x0 y0 z0)
  (let ((x x0) (y y0) (z z0))
    (if (not (< y x))
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y)))))

(define (%tak x0 y0 z0)
  (%let ((x x0) (y y0) (z z0))
        (if (not (< y x))
            z
            (%tak (%tak (- x 1) y z)
                  (%tak (- y 1) z x)
                  (%tak (- z 1) x y)))))

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


(measure-time "let-tak" (tak 24 16 8))
(measure-time "%let-tak" (%tak 24 16 8))