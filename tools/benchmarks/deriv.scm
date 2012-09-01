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

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (error #f "No derivation method available"))))

(define-constant *deriv-map*
  (make-identity-hash))

(map-set! *deriv-map* '+
          (lambda (a)
            (cons '+
                  (map deriv (cdr a)))))

(map-set! *deriv-map* '-
          (lambda (a)
            (cons '-
                  (map deriv (cdr a)))))

(map-set! *deriv-map* '*
          (lambda (a)
            (list '*
                  a
                  (cons '+
                        (map (lambda (a) (list '/ (deriv a) a)) (cdr a))))))

(map-set! *deriv-map* '/
          (lambda (a)
            (list '-
                  (list '/
                        (deriv (cadr a))
                        (caddr a))
                  (list '/
                        (cadr a)
                        (list '*
                              (caddr a)
                              (caddr a)
                              (deriv (caddr a)))))))
          

(define (dderiv a)
  (if (not (pair? a))
      (if (eq? a 'x) 1 0)
      ((map-ref *deriv-map* (car a)) a)))
  

(define (nth-deriv expr n)
  (let ((tmp expr))
    (for-each (lambda (x)
                (set! tmp (deriv tmp)))
              (make-number-sequence :to n))
    tmp))
(define (nth-dderiv expr n)
  (let ((tmp expr))
    (for-each (lambda (x)
                (set! tmp (dderiv tmp)))
              (make-number-sequence :to n))
    tmp))

(define-constant inputs 
  '(1
    x
    (+ x x)
    (- 4 x)
    (* 3 x)
    (/ 1 x)
    (* x x)
    (+ (* x y) (/ x y))))

(measure-time "deriv" (for-each (lambda (expr) (nth-deriv expr 6)) inputs))
(measure-time "dderiv" (for-each (lambda (expr) (nth-dderiv expr 6)) inputs))




