;;; dfsch - Scheme-like Lisp dialect
;;;   SQL database interface
;;; Copyright (c) 2010 Ales Hakl
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

(provide :sql)
(require :sql-support)
(define-package :sql
  :uses '(:dfsch :sql-support)
  :exports '(:close-database!
             :exec-string!
             :query-string
             :close-result!
             :column-names
             :column-types
             :exec!
             :query))
(in-package :sql)

;;; Fallback implementation (for sqlite3)
(define-method (column-types res)
  (map type-of
       (iter-this res)))

(define-generic-function convert-sql-value)

(define (build-query string values &optional db)
  (construct-string string values
                    :convert-all convert-sql-value
                    :escape-character #\:))

(define-method (exec! db statement &rest args)
  (exec-string! db (build-query statement args db)))

(define-method (query db statement &rest args)
  (query-string db (build-query statement args db)))


(define-method (convert-sql-value (value <proto-string>))
  (sql-support:escape-string value))
(define-method (convert-sql-value (value <number>))
  (number->string value))
(define-method (convert-sql-value (value <<collection>>))
  (string-append
   "("
   (string-join (map convert-sql-value value) ", ")
   ")"))
                                  
