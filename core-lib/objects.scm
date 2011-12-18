;;; dfsch - Scheme-like Lisp dialect
;;;   Standard macros - object system related
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

(define (dfsch:make-instance class &rest init-args)
  (let ((inst (allocate-instance class)))
    (apply initialize-instance inst init-args)
    inst))

(define (dfsch:make-simple-method-combination operator)
  (lambda (methods function)
    (lambda (&rest args)
      (operator (map (lambda (meth)
                       (call-method meth () args))
                     (get-primary-methods methods))))))

(define-macro (dfsch:define-custom-specializer name args &body code)
  `@(%define-canonical-constant ,name
                                (make-type-specializer (%lambda ,name ,args 
                                                                ,@code))))

(define-macro (dfsch:define-has-slot-specializer name slot 
                                                 &optional (doc () doc?))
  `@(define-custom-specializer ,name (type)
      ,@(when doc? (list doc))
      (ignore-errors (find-slot type ',slot) #t)))

(define-has-slot-specializer dfsch:<<documented>> :documentation
  "All objects with :documentation slot")

(define-has-slot-specializer dfsch:<<documented-synopsis>> :synopsis
  "All objects with :synopsis slot")

(define-macro (add-method-to-generic-function! name spec-list proc)
  `(add-method! (define-generic-function ,name) 
                (make-method (cons ',name ',spec-list)
                             #n
                             (list ,@spec-list)
                             ,proc)))

(define-macro (dfsch:define-slot-accessor name class slot-name)
  `(add-method-to-generic-function! ,name 
                                    (,class)
                                    (make-slot-accessor ,class  ,slot-name)))
(define-macro (dfsch:define-slot-reader name class slot-name)
  `(add-method-to-generic-function! ,name
                                    (,class)
                                    (make-slot-reader ,class  ,slot-name)))
(define-macro (dfsch:define-slot-writer name class slot-name)
  `(add-method-to-generic-function! ,name
                                    (,class)
                                    (make-slot-writer ,class  ,slot-name)))

(define-macro (dfsch:define-class name superklass slots &rest class-opts)
  (let ((class-slots (map 
                      (lambda (desc)
                        (letrec ((name (if (pair? desc) (car desc) desc))
                                 (opts (if (pair? desc) (cdr desc) ()))
                                 (opt-expr (plist-remove-keys opts 
                                                              '(:accessor 
                                                                :reader 
                                                                :writer
                                                                :initform)))
                                 (init-form (plist-get opts :initform)))
                          `@(list ',name ,@opt-expr 
                                  ,@(when init-form
                                      `(:initfunc 
                                        (lambda () 
                                          "slot initializer" 
                                          ,(car init-form)))))))
                      slots)))
    `@(begin 
        (%define-canonical-constant ,name (make-class ',name 
                                                      ,superklass 
                                                      (list ,@class-slots)
                                                      ,@class-opts))
        ,@(mapcan 
           (lambda (desc)
             (letrec ((sname (if (pair? desc) (car desc) desc))
                      (opts (if (pair? desc) (cdr desc) ()))
                      (accessor (plist-get opts :accessor))
                      (writer (plist-get opts :writer))
                      (reader (plist-get opts :reader)))
               (append 
                (when accessor
                  `((define-slot-accessor ,(car accessor) ,name ',sname)))
                (when reader
                  `((define-slot-reader ,(car reader) ,name ',sname)))
                (when writer
                  `((define-slot-writer ,(car writer) ,name ',sname))))))
           slots))))


