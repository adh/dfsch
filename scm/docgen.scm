#!/usr/bin/env dfsch-repl
; documentation generator for dfsch

(require 'introspect)
(require 'sxml)

(define *clean-toplevel* (make-default-environment))

(define (get-toplevel-variables)
  (get-variables *clean-toplevel*))

(define (get-object-documentation object)
  (cond
   ((instance? object <function>)
    (slot-ref object 'documentation))
   ((instance? object <form>)
    (slot-ref object 'documentation))
   ((instance? object <macro>)
    (get-object-documentation (slot-ref object 'procedure)))
   (else "Unknown object")))

(define (variables->name+doc lyst)
  (map 
   (lambda (x) 
     (let ((name (car x))
           (value (cadr x)))
       (list (symbol->string name) 
             (get-object-documentation value))))
   lyst))

(define (sort-alist alist)
  (sort-list! alist
             (lambda (x y)
               (string<? (car x)
                         (car y)))))

(define (make-index list)
  `(ul ,(map (lambda (item)
               (let ((name (car item)))
                 `(li (a (@ href ,(string-append "#" name))
                         ,name))))
             list)))