#!/usr/bin/env dfsch-repl

(require :gcollect)
(require :threads)
(use-package :threads)

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (tak-inline x y z)
  (define (tak-impl x y z)
    (#.if (#.not (#.< y x))
          z
          (tak-impl (tak-impl (#.- x 1) y z)
                    (tak-impl (#.- y 1) z x)
                    (tak-impl (#.- z 1) x y))))
  (tak-impl x y z))



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

(define (run-threads n proc)
  (if (> n 0)
      (cons
       (thread-create proc)
       (run-threads (- n 1) proc))
      ()))

(define (join-threads tl)
  (if (null? tl)
      ()
      (begin
        (thread-join (car tl))
        (join-threads (cdr tl)))))

(define (tak-thread)
  (tak 24 16 8))
(define (tak-inline-thread)
  (tak-inline 24 16 8))

(set-current-output-port! *standard-output-port*)

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
(measure-time tak-5 (join-threads (run-threads 5 tak-thread)))
(measure-time tak-inline-5 (join-threads (run-threads 5 tak-inline-thread)))
(measure-time tak-6 (join-threads (run-threads 6 tak-thread)))
(measure-time tak-inline-6 (join-threads (run-threads 6 tak-inline-thread)))
(measure-time tak-7 (join-threads (run-threads 7 tak-thread)))
(measure-time tak-inline-7 (join-threads (run-threads 7 tak-inline-thread)))
(measure-time tak-8 (join-threads (run-threads 8 tak-thread)))
(measure-time tak-inline-8 (join-threads (run-threads 8 tak-inline-thread)))

