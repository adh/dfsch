#!/usr/bin/env dfsch-repl

(require 'gcollect)
(require 'threads)

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define tak-inline 
  (define (tak-inline x y z) ;; XXX: stupid trick
    (#.if (#.not (#.< y x))
          z
          (tak-inline (tak-inline (#.- x 1) y z)
                      (tak-inline (#.- y 1) z x)
                      (tak-inline (#.- z 1) x y)))))



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

(define (run-threads n proc)
  (if (> n 0)
      (cons
       (thread:create proc)
       (run-threads (- n 1) proc))
      ()))

(define (join-threads tl)
  (if (null? tl)
      ()
      (begin
        (thread:join (car tl))
        (join-threads (cdr tl)))))

(define (tak-thread)
  (tak 24 16 8))
(define (tak-inline-thread)
  (tak-inline 24 16 8))

(measure-time tak-0 (tak-thread))
(measure-time tak-inline-0 (tak-inline-thread))

(measure-time tak-1 (join-threads (run-threads 1 tak-thread)))
(measure-time tak-inline-1 (join-threads (run-threads 1 tak-inline-thread)))

(measure-time tak-2 (join-threads (run-threads 2 tak-thread)))
(measure-time tak-inline-2 (join-threads (run-threads 2 tak-inline-thread)))

(measure-time tak-3 (join-threads (run-threads 3 tak-thread)))
(measure-time tak-inline-3 (join-threads (run-threads 3 tak-inline-thread)))

(measure-time tak-4 (join-threads (run-threads 4 tak-thread)))
(measure-time tak-inline-4 (join-threads (run-threads 4 tak-inline-thread)))

