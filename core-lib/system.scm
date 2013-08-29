;;; dfsch - Scheme-like Lisp dialect
;;;   System-related macros
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

;; General state of interpreter is unknown when this gets loaded
(dfsch:define-package :dfsch%implementation 
                      :uses '(:dfsch :dfsch%internal :dfsch-lang)
                      :exports '()
                      :documentation 
                      "Internal package used by self-hosted standard library")

(dfsch:in-package :dfsch%implementation)

(define-macro (dfsch:with-open-file variable args &body body)
  (with-gensyms (result)
  `(let ((,variable (dfsch:open-file-port ,@args)))
     (unwind-protect (begin ,@body)
                     (dfsch:close-file-port! ,variable)))))

(define-macro (dfsch:with-input-from-port port &body body)
  `(bind-and-rebind %set-current-input-port! 
                    (current-input-port) 
                    ,port
                    ,@body))

(define-macro (dfsch:with-output-to-port port &body body)
  `(bind-and-rebind %set-current-output-port! 
                    (current-output-port) 
                    ,port
                    ,@body))

(define-macro (dfsch:with-input-from-string string &body body)
  `(with-input-from-port (string-input-port ,string)
     ,@body))

(define-macro (%with-output-to-string string? &body body)
  (with-gensyms (port)
    `(let ((,port (string-output-port)))
       (with-output-to-port ,port
         ,@body)
       (string-output-port-value ,port ,string?))))

(define-macro (dfsch:with-output-to-string &body body)
  `(%with-output-to-string #t ,@body))
(define-macro (dfsch:with-output-to-byte-vector &body body)
  `(%with-output-to-string #f ,@body))

