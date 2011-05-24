;;; dfsch - Scheme-like Lisp dialect
;;;   Standard macros - condition system related
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

(define-macro (dfsch:ignore-errors &rest forms)
  (with-gensyms (tag)
    `@(catch ',tag
             (handler-bind ((<error> (lambda (err)
                                       (throw ',tag (values () err)))))
                           ,@forms))))

(define-macro (dfsch:detect-errors &rest forms)
  (with-gensyms (tag)
    `@(catch ',tag
             (handler-bind ((<error> (lambda (err)
                                       (throw ',tag 
                                              (list () err)))))
                           (list (begin ,@forms))))))

(define-macro (dfsch:handler-case form &rest handlers)
  (with-gensyms (tag handler-id result)
    `@(let* ((,handler-id :no-error)
             (,result (catch ',tag
                             (handler-bind 
                              ,(map (lambda (handler)
                                      `@(,(car handler)
                                         (lambda (err)
                                           (set! ,handler-id
                                                 ',(car handler))
                                           (throw ',tag err))))
                                    handlers)
                              ,form))))
        (case ,handler-id 
          ,.(map (lambda (handler)
                   `@((,(car handler)) 
                      (let ((,(caadr handler) ,result))
                        ,@(cddr handler))))
                 handlers)
          (else ,result)))))

(define-macro (dfsch:with-simple-restart name description &rest forms)
  (with-gensyms (tag)
                `@(catch ',tag
                         (restart-bind ((,name (lambda ()
                                                 (throw ',tag ()))
                                               ,description))
                                       ,@forms))))


(define-macro (dfsch:restart-case form &rest restarts)
  (with-gensyms (tag result restart-args restart-id)
    `@(let* ((,restart-id ())
             (,restart-args ())
             (,result (catch ',tag
                             (restart-bind 
                              ,(map (lambda (restart)
                                      `@(',(car restart)
                                         (lambda (&rest args)
                                           (set! ,restart-id 
                                                 ',restart)
                                           (throw ',tag ()))
                                         ',(when (string? (caddr 
                                                           restart))
                                                 (caddr restart))))
                                    restarts)
                              ,form))))
        (case ,restart-id 
          ,.(map (lambda (restart)
                   `((,restart)
                     (apply (lambda ,(cadr restart)
                              ,@(cddr restart))
                            ,restart-args)))
                 restarts)
          (else ,result)))))

