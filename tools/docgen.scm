#!/usr/bin/env dfsch-repl
;;;; Documentation generator for dfsch
;;;
;;; Copyright (c) 2008 - 2011 Ales Hakl
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

(require :introspect)
(require :shtml)
(require :inet)
(require :cmdopts)
(require :os)
(require :markdown)

(define-package :docgen
  :uses '(:dfsch :markdown :os :shtml)
  :exports '())

(define *stylesheet*
  "
.menu-bar {
  display: block; 
  background-color: #abcdef; 
  border-width: 1px; 
  border-color: black; 
  border-style: solid;
  padding-left: 1em
}
.menu-bar li {
  display: inline; 
  margin-left: 0.5em; 
  margin-right: 0.5em
}
.char-bar {
  display: block; 
  border-width: 1px; 
  border-color: #abcdef; 
  border-style: solid;
  padding-left: 0.2em
}
.char-bar li {
  display: inline; 
  margin-left: 0.3em; 
  margin-right: 0.3em
}

")

(define (directory? path)
  (let ((stat (os:stat path)))
    (if (null? stat)
        ()
        (stat :isdir))))

(define (ensure-directory path)
  (unless (directory? path)
          (os:mkdir path)))

(define *clean-toplevel* (make-top-level-environment))

(define (sort-by-names! lyst)
  (sort-list! lyst
              (lambda (x y)
                (string<? (symbol-name (car x))
                          (symbol-name (car y))))))


(define (get-toplevel-variables)
  (sort-by-names! (get-variables *clean-toplevel*)))

(define (get-module-variables module)
  (letrec ((toplevel (make-top-level-environment))
           (start-state (get-variables toplevel)))
    (load-into-environment! toplevel module *load-path*)
    (for-each (lambda (x) 
                (let ((name (car x)))
                  (unset-from-environment! name toplevel)))
              start-state)
    (sort-by-names! (get-variables toplevel))))

(define (convert-documentation-block string)
  (markdown:markdown string :html))

(define-generic-function get-object-documentation
  :method-combination 
  (make-simple-method-combination (lambda (res)
                                    (apply nconc (reverse res)))))

(define (format-markdown-docstring str)
  `((:literal-output ,(convert-documentation-block str))))
(define (format-documentation-slot object &key supress-head)
  (let ((str (slot-ref object :documentation)))
    (when str
          `(,@(unless supress-head '((:h2 "Documentation slot")))
            ,@(format-markdown-docstring str)))))

(define-method (get-object-documentation (object <<documented>>) &key supress-head)
  (format-documentation-slot object :supress-head supress-head))
                                         
(define-method (get-object-documentation (object <macro>) &key supress-head)
  (format-documentation-slot (slot-ref object :proc) :supress-head supress-head))

(define-method (get-object-documentation (object <standard-type>) &key supress-head)
  `(,@(unless supress-head '((:h2 "Slots")))
    (:ul ,@(map (lambda (slot) 
                  `(:li ,(slot-ref slot :name) ": " 
                        ,(slot-ref slot :documentation)))
                (get-slots object)))))

(define-method (get-method-documentation (meth <method>))
  `((:h3 "Specialized on:")
    (:pre ,(format "~y" (slot-ref meth :specializers)))
    (:h3 "Implementation:")
    ,@(get-object-documentation (slot-ref meth :function) :supress-head #t)))


(define-method (get-object-documentation (object <standard-generic-function>) &key supress-head)
  `(,@(unless supress-head '((:h2 "Methods")))
    (:ul ,@(map (lambda (method) 
                  `(:li ,@(get-method-documentation method)))
                (generic-function-methods object)))))
  
(define-method (get-object-documentation object &allow-other-keys)
  `((:pre ,(format "~y" object))))



(define-generic-function get-object-name)

(define-method (get-object-type-name object)
  (slot-ref (type-of object) :name))

(define-method (get-object-name object name)
  (string-append (get-object-type-name object) " " 
                 (symbol-qualified-name name)))

(define-generic-function get-object-categories
  :method-combination 
  (make-simple-method-combination (lambda (res)
                                    (or (apply append (reverse res))
                                        '("Other objects")))))


(define-method (get-object-categories object)
  '())

(define-method (get-object-categories (object <form>))
  (list "Special forms"))

(define-method (get-object-categories (object <macro>))
  (list "Macros"))

(define-method (get-object-categories (object <function>))
  (list "Functions"))

(define-method (get-object-categories (object <generic-function>))
  (list "Generic functions"))

(define-method (get-object-categories (object <standard-type>))
  (list "Types"))


(define (html-boiler-plate title main-title infoset)
  `(:html 
    :xmlns "http://www.w3.org/1999/xhtml"
    (:head (:title ,(if title
                        (string-append title " - " main-title)
                        main-title))
           (:style ,*stylesheet*))
    (:body (:h1 ,(or title main-title))
          ,@infoset
          (:hr)
          (:p "Generated by docgen.scm running on dfsch " ,*dfsch-version*
              " (" ,*dfsch-build-id* ")"))))

(define (category-name cat)
  (car cat))

(define (category-entries cat)
  (cdr cat))

(define (category-index-name cat)
  (string-append "cat_" 
                 (string->safe-filename (category-name cat) #t) 
                 ".html"))

(define (menu-bar categories current)
  `(:ul 
    :class "menu-bar"
    (:li ,(if current
              '(:a :href "index.html"
                   "All")
              '(:strong "All")))
    ,@(map (lambda (cat)
             (if (eq? cat current)
                 `(:li (:strong ,(category-name cat)))
                 `(:li (:a :href ,(category-index-name cat)
                           ,(category-name cat)))))
           categories)))

(define (char-name ch)
  (format "idx-~a" (car ch)))

(define (char-bar chars current)
  `(:ul 
    :class "char-bar"
    ,@(map (lambda (ch)
             (if (eq? ch current)
                 `(:li (:strong ,(string (car ch))))
                 `(:li (:a :href ,(string-append "#" (char-name ch))
                           ,(string (car ch))))))
           chars)))


(define (make-one-entry entry)
  (let ((object (cadr entry)))
    (get-object-documentation object)))

(define (entry-name entry)
  (get-object-name (cadr entry)
                   (car entry)))

(define (entry-filename entry)
  (string-append (string->safe-filename (symbol-qualified-name (car entry)))
                 ".html"))

(define (emit-one-entry entry directory title categories)
  (shtml:emit-file (html-boiler-plate (entry-name entry)
                                      title
                                      `(,(menu-bar categories #t)
                                        ,@(make-one-entry entry)))
                   (string-append directory "/"
                                  (entry-filename entry))))

(define (make-entry-list lyst)
  `(:ul :class "entry-list"
    ,@(map (lambda (entry)
             `(:li ,(get-object-type-name (cadr entry))
                   " "
                   (:a :href ,(entry-filename entry)
                       ,(symbol-qualified-name (car entry)))))
           lyst)))

(define (group-by lyst keys)
  (let ((m (make-hash)))
    (for-each (lambda (entry)
                (for-each (lambda (k)
                            (map-set! m k (cons entry (map-ref m k ()))))
                          (keys entry)))
              lyst)
    (map (lambda (cat)
           (cons (car cat) (reverse (cadr cat))))
         (collection->reversed-list m))))

(define (entry-get-categories entry)
  (get-object-categories (cadr entry)))

(define (make-index-list lyst)
  (let ((chars (sort-list! (group-by lyst 
                                     (lambda (ent)
                                       (list (char-upcase (seq-ref (symbol-name (car ent))
                                                                   0)))))
                           (lambda (x y)
                             (< (car x) (car y))))))
    (mapcan (lambda (ch)
              `((:a :name ,(char-name ch))
                ,(char-bar chars ch)
                ,(make-entry-list (cdr ch))))
            chars)))

(define (emit-documentation lyst directory title)
  (let ((categories (sort-list! (group-by lyst entry-get-categories)
                                (lambda (x y)
                                  (string<? (car x) (car y))))))
    (ensure-directory directory)
    (shtml:emit-file (html-boiler-plate () title 
                                        `(,(menu-bar categories ())
                                          ,@(make-index-list lyst)))
                     (string-append directory "/index.html"))
    (for-each (lambda (cat)
                (shtml:emit-file (html-boiler-plate (category-name cat) 
                                                    title 
                                                    `(,(menu-bar categories 
                                                                 cat)
                                                      ,(make-entry-list 
                                                        (category-entries cat))))
                                 (string-append directory "/"
                                                (category-index-name cat))))
              categories)
                
    (for-each (lambda (entry)
                (emit-one-entry entry directory title categories))
              lyst)))


(define (emit-core-documentation directory)
  (emit-documentation 
   (get-toplevel-variables)
   directory
   "Default dfsch top-level environment"))

(define (emit-module-documentation directory module)
  (emit-documentation 
   (get-module-variables module)
   directory
   (string-append (object->string module) " module")))


;(emit-core-documentation "out")
