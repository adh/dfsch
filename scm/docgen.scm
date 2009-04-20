#!/usr/bin/env dfsch-repl
; documentation generator for dfsch

(require 'introspect)
(require 'sxml)
(require 'cmdopts)

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
             (type-name (type-of value))
             (get-object-documentation value))))
   lyst))

(define (sort-alist alist)
  (sort-list! alist
             (lambda (x y)
               (string<? (car x)
                         (car y)))))

(define (make-index-list list link-to)
  `(ul ,(map (lambda (item)
               (let ((name (car item)))
                 `(li (a (@ href ,(string-append link-to "#" name))
                         ,name))))
             list)))

(define (make-documentation-body lyst)
  (map (lambda (item)
         (let ((name (car item))
               (type (cadr item))
               (documentation (caddr item)))
           `(div (@ (id ,name) (title ,name))
                 (h1 ,(string-append type " " name))
                 (p ,documentation))))
       lyst))

