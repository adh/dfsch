;;; dfsch - Scheme-like Lisp dialect
;;;   Helper functions for markdown handling
;;; Copyright (c) 2011 Ales Hakl
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

(provide :markdown-tools)
(require :markdown)

(define-package :markdown-tools
  :uses '(:markdown)
  :exports '(:split-file
             :make-outlining-header-renderer))

(define (split-file port)
  (letrec ((cur (list () ""))
           (res (list cur)))
    (for-each (lambda (line)
                (if (and (< 0 (string-length line))
                         (equal? (string-ref line 0) #\#))
                    (let ((name (string-trim " \t\n\r#" (substring line 1))))
                      (set! cur (list name ""))
                      (nconc res (list cur)))
                    (set-car! (cdr cur)
                              (string-append (cadr cur)
                                             line))))
              (make-port-line-iterator port))
    res))