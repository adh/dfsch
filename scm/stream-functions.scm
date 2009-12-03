;;; dfsch - Scheme-like Lisp dialect
;;;   Stream manipulation functions
;;; Copyright (c) 2009 Ales Hakl
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

(in-package :dfsch%internal)
(provide :stream-functions)

(define (dfsch:stream-filter fn stream)
  (if stream
      (let ((element (stream-car stream)))
        (if (fn element)
            (stream-cons element
                         (dfsch:stream-filter fn (stream-cdr stream)))
            (filter-stream fn (stream-cdr stream))))
      ()))
 
(define (dfsch:stream-while fn stream)
  (if stream
      (let ((element (stream-car stream)))
        (if (fn element)
            (stream-cons element
                         (dfsch:stream-while fn (stream-cdr stream)))
            ()))
      ()))
 
(define (dfsch:stream-reduce fn stream)
  (if stream
      (let loop ((i (stream-cdr stream)) (val (stream-car stream)))
        (if i
            (loop (stream-cdr i) (fn val (stream-car i)))
            val))
      ()))