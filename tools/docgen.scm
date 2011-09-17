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

;; Index searches are highly sub-optimal, but still, this thing is
;; actually IO-bound :)

(require :introspect)
(require :shtml)
(require :inet)
(require :cmdopts)
(require :os)
(require :markdown)
(require :markdown-tools)

(define-package :docgen
  :uses '(:dfsch :markdown :markdown-tools :os :shtml)
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

.pkg-menu {
  display: block;
  float: right;
}
.pkg-label {
  display: none;
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

.chapter-list {
  margin-top: 0em;
  background-color: #cdabef;
  border-width: 1px; 
  border-color: black; 
  border-style: solid;
  float: right;
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

(define (sort-by-names lyst)
  (sort-list lyst
             (lambda (x y)
               (string<? (symbol-name (car x))
                         (symbol-name (car y))))))


(define (get-toplevel-variables)
  (sort-by-names (get-variables *clean-toplevel*)))

(define (get-module-variables module)
  (letrec ((toplevel (make-top-level-environment))
           (start-state (get-variables toplevel)))
    (load-into-environment! toplevel module *load-path*)
    (for-each (lambda (x) 
                (let ((name (car x)))
                  (unset-from-environment! name toplevel)))
              start-state)
    (sort-by-names (get-variables toplevel))))

(define (convert-documentation-block string)
  (markdown:markdown string :html :emphasis-chars "*_|"
                     :emphasis (lambda (str ch)
                                 (if (eq? ch #\|)
                                     (shtml:emit-string (name-string-link str))
                                     (string-append "<em>" str "</em>")))))

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

(define-method (get-object-documentation (object <<documented>>) 
                                         &key supress-head  &allow-other-keys)
  (format-documentation-slot object :supress-head supress-head))

(define-method (get-object-documentation (object <<documented-synopsis>>) 
                                         &key supress-head &allow-other-keys)
  (let ((synopsis (slot-ref object :synopsis)))
    (when synopsis
          `(,@(unless supress-head '((:h2 "Synopsis")))
            (:pre ,synopsis)))))
                                         
(define-method (get-object-documentation (object <macro>) 
                                         &key supress-head &allow-other-keys)
  (get-object-documentation (slot-ref object :proc) 
                            :supress-head supress-head
                            :supress-value #t))

(define-method (get-object-documentation (object <function-type-specializer>) 
                                         &key supress-head &allow-other-keys)
  (get-object-documentation (slot-ref object :proc) 
                            :supress-head #t
                            :supress-value #t))

(define-method (get-object-documentation (object <standard-type>) 
                                         &key supress-head &allow-other-keys)
  `(,(if supress-head 
         '(:strong "Superclass:") 
         '(:h2 "Superclass"))
    ,(value-link (superclass object))
    ,@(unless supress-head '((:h2 "Slots")))
    (:ul ,@(map (lambda (slot) 
                  `(:li (:strong ,(slot-ref slot :name)) ": " 
                        ,(or (slot-ref slot :documentation) "")))
                (get-slots object)))
    ,(if supress-head 
         '(:strong "Direct subclasses")
         '(:h2 "Direct subclasses"))
    ,(matching-values-list (lambda (type)
                             (and (instance? type <standard-type>)
                                  (eq? object (superclass type)))))))

(define-method (get-object-documentation (object <standard-function>) 
                                         &key supress-head &allow-other-keys)
  `(,(if supress-head 
         '(:strong "Arguments:")
         '(:h2 "Arguments"))
    (:pre ,(format "~a" (slot-ref object :orig_args)))))


(define-method (get-method-documentation (meth <method>))
  `((:h2 "Specialized on:")
    (:pre ,(format "~y" (slot-ref meth :specializers)))
    (:h3 "Implementation:")
    ,@(get-object-documentation (slot-ref meth :function) :supress-head #t)))


(define-method (get-object-documentation (object <standard-generic-function>) &key supress-head)
  `(,@(unless supress-head '((:h2 "Methods")))
    (:ul ,@(map (lambda (method) 
                  `(:li ,@(get-method-documentation method)))
                (generic-function-methods object)))))
  
(define-method (get-object-documentation (object <type-specializer>))
  (unless (instance? object <standard-type>)
          `((:h3 "Implemented by")
            ,(matching-values-list 
              (lambda (type) 
                (and (instance? type <standard-type>)
                     (specializer-matches-type? object type)))))))

(define-method (get-object-documentation object 
                                         &key supress-value &allow-other-keys)
  (unless supress-value
          `((:pre ,(format "~y" object)))))



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
                                        '("uncategorized")))))


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

(define-method (get-object-categories (object <type-specializer>))
  (unless (instance? object <standard-type>)
          (list "Type specializers")))


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
          (:p (:address 
               "Generated by docgen.scm running on dfsch " ,*dfsch-version*
              " (" ,*dfsch-build-id* ")")))))

(define (category-name cat)
  (car cat))

(define (category-entries cat)
  (cdr cat))

(define (category-index-name cat)
  (string-append "categories/" 
                 (string->safe-filename (category-name cat) #t #\Space) 
                 ".html"))

(define (package-index-name pkg)
  (string-append "packages/" 
                 (string->safe-filename (slot-ref pkg :name)) 
                 ".html"))

(define (menu-bar categories current base packages)
  `(:ul 
    :class "menu-bar"
    (:li ,(if current
              `(:a :href ,(string-append base "index.html")
                   "All")
              '(:strong "All")))
    (:li ,(if (eq? current :hierarchy)
               '(:strong "Type hierarchy")
               `(:a :href ,(string-append base "hierarchy.html")
                    "Type hierarchy")))
    ,@(map (lambda (cat)
             (if (eq? cat current)
                 `(:li (:strong ,(category-name cat)))
                 `(:li (:a :href ,(string-append base 
                                                 (category-index-name cat))
                           ,(category-name cat)))))
           categories)
    (:li (:strong :class "pkg-label" "Packages:")
         (:ul :class "pkg-menu"
              ,@(map 
                 (lambda (pkg)
                   (if (eq? pkg current)
                       `(:li :class "pkg-nav"
                             (:strong ,(slot-ref pkg :name)))
                       `(:li :class pkg-nav
                             (:a :href ,(string-append base 
                                                       (package-index-name pkg))
                                 ,(slot-ref pkg :name)))))
                 packages)))))

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
  (let ((object (cadr entry))
        (name (car entry)))
    (get-object-documentation object)))

(define (entry-name entry)
  (get-object-name (cadr entry)
                   (car entry)))

(define (make-filename name)
  (string-append "entries/"
                 (string->safe-filename (symbol-qualified-name name))
                 ".html"))

(define (entry-filename entry)
  (make-filename (car entry)))

(define-variable *global-index* ())

(define (index-put uri-base name &optional value)
  (set! *global-index*
        (cons (list name value uri-base)
              *global-index*)))
(define (index-put-entry uri-base entry)
  (index-put uri-base (car entry) (and (cdr entry) (cadr entry))))

(define (index-put-all uri-base entries)
  (for-each (lambda (entry)
              (index-put-entry uri-base entry))
            entries))

(define (build-uri entry)
  (when entry
        (string-append (caddr entry) (make-filename (car entry)))))

(define (build-uri entry)
  (when entry
        (string-append (caddr entry) (make-filename (car entry)))))


(define (index-entry-for-name name)
  (find-if (lambda (entry) (eq? (car entry) name))
           *global-index*))

(define (index-entry-for-value value)
  (and value ; ignore nil
       (find-if (lambda (entry) (eq? (cadr entry) value))
                *global-index*)))

(define (index-entries-matching-value filter-func)
  (filter (lambda (entry) (filter-func (cadr entry)))
          *global-index*))

(define (index-name-for-value value)
  (let ((entry (index-entry-for-value value)))
    (when entry
          (symbol-name (car entry)))))

(define (name-string-link name-string &optional name)
  (let ((entry (index-entry-for-name (ignore-errors 
                                      (intern-symbol name-string)))))
    (if entry
        `(:a :href ,(build-uri entry)
             ,(symbol-qualified-name (car entry)))
        (or name `(:code ,name-string)))))


(define (value-link value &optional name)
  (let ((entry (index-entry-for-value value)))
    (if entry
        `(:a :href ,(build-uri entry)
             ,(symbol-qualified-name (car entry)))
        (or name `(:pre ,(format "~y" value))))))

(define (simple-value-link value &optional name)
  (let ((entry (index-entry-for-value value)))
    (if entry
        `(:a :href ,(entry-filename entry)
             ,(symbol-qualified-name (car entry)))
        (or name `(:pre ,(format "~y" value))))))


(define (matching-values-list filter-func)
  `(:ul
    ,@(map (lambda (entry)
             `(:li 
               (:a :href ,(build-uri entry)
                   ,(symbol-qualified-name (car entry)))))
               (index-entries-matching-value filter-func))))

(define (emit-one-entry entry directory title categories packages)
  (shtml:emit-file (html-boiler-plate (entry-name entry)
                                      title
                                      `(,(menu-bar categories 
                                                   #t 
                                                   "../" 
                                                   packages)
                                        ,@(make-one-entry entry)))
                   (string-append directory "/"
                                  (entry-filename entry))))

(define (make-entry-list lyst base)
  `(:ul :class "entry-list"
    ,@(map (lambda (entry)
             `(:li ,(get-object-type-name (cadr entry))
                   " "
                   (:a :href ,(string-append base 
                                             (entry-filename entry))
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

(define (unique lyst key)
  (let ((m (make-hash)))
    (for-each (lambda (entry)
                (map-set! m (key entry) entry))
              lyst)
    (collection->reversed-list (map-keys m))))


(define (build-type-hierarchy types)
  (letrec ((res (list ()))
           (tm (identity-hash () res)))
    (define (put-type type)
      (let ((r (map-ref tm type ())))
        (if r
            r
            (let ((nr (list type))
                  (pr (put-type (superclass type))))
              (map-set! tm type nr)
              (nconc pr (list nr))
              nr))))
    (for-each put-type types)
    res))

(define (compare-objects-by-name x y)
  (let ((xn (index-name-for-value x))
        (yn (index-name-for-value y)))
    (cond ((and xn yn) (string<? xn yn))
          (xn #t)
          (yn #f)
          (else (< (id x) (id y))))))

(define (type-subclass-tree lyst)
  `(,@(when (car lyst) (list (simple-value-link (car lyst))))
    (:ul
     ,@(map (lambda (sub)
              `(:li
                ,@(type-subclass-tree sub)))
            (sort-list (cdr lyst)
                       (lambda (x y) 
                         (compare-objects-by-name (car x) (car y))))))))

(define (make-type-hierarchy-page entries)
  (let ((hier (build-type-hierarchy (filter (lambda (obj)
                                              (instance? obj <standard-type>))
                                            (map cadr entries)))))
    (type-subclass-tree hier)))

(define (entry-get-categories entry)
  (get-object-categories (cadr entry)))

(define (make-index-list lyst base)
  (let ((chars (sort-list (group-by lyst 
                                    (lambda (ent)
                                      (list (char-upcase (seq-ref (symbol-name (car ent))
                                                                  0)))))
                          (lambda (x y)
                            (< (car x) (car y))))))
    (mapcan (lambda (ch)
              `((:a :name ,(char-name ch))
                ,(char-bar chars ch)
                ,(make-entry-list (cdr ch) base)))
            chars)))

(define (package-index pkg lyst)
  `(,@(format-documentation-slot pkg)
    ,@(make-index-list (filter (lambda (entry) 
                                 (eq? (symbol-package (car entry))
                                      pkg)) 
                               lyst)
                       "../")))

(define (read-markdown-file filename)
  (with-open-file f (filename "r")
                  (markdown-tools:split-file f)))

(define (chapter-file-name chapter &optional (base "chapters/"))
  (string-append base 
                 (string->safe-filename (car chapter) #t #\Space)
                 ".html"))
                                         

(define (chapter-index chapters)
  (when chapters
        `((:literal-output
           ,(convert-documentation-block (cadar chapters)))
          (:ul
           ,@(map (lambda (chapter)
                    `(:li (:a :href ,(chapter-file-name chapter)
                              ,(car chapter))))
                  (cdr chapters)))
          ,@(unless (equal? "" (cadar chapters))
                    '((:h2 "Defined symbols:"))))))

(define (chapter-content chapter chapters)
  `((:ul :class "chapter-list"
         ,@(map (lambda (ch)
                  `(:li ,(if (eq? ch chapter)
                             `(:strong ,(car ch))
                             `(:a :href ,(chapter-file-name ch "./")
                                  ,(car ch)))))
                (cdr chapters)))
    (:literal-output
     ,(convert-documentation-block (cadr chapter)))))

(define (emit-documentation lyst directory title
                            &key notes chapters)
  (index-put-all "../" lyst)
  (let ((categories (sort-list (group-by lyst entry-get-categories)
                               (lambda (x y)
                                 (string<? (car x) (car y)))))
        (packages (sort-list (unique lyst 
                                     (lambda (entry)
                                       (symbol-package (car entry))))
                             (lambda (a b)
                               (string<? (slot-ref a :name)
                                         (slot-ref b :name)))))
        (note-list (when notes (read-markdown-file notes)))
        (chapter-list (when chapters (read-markdown-file chapters))))

    (ensure-directory directory)
    (ensure-directory (string-append directory "/entries"))
    (ensure-directory (string-append directory "/categories"))
    (ensure-directory (string-append directory "/packages"))
    (when chapter-list
          (ensure-directory (string-append directory "/chapters")))

    (shtml:emit-file (html-boiler-plate () title 
                                        `(,(menu-bar categories 
                                                     () 
                                                     "./"
                                                     packages)
                                          ,@(chapter-index chapter-list)
                                          ,@(make-index-list lyst "./")))
                     (string-append directory "/index.html"))
    (shtml:emit-file (html-boiler-plate "Type hierarchy" 
                                        title 
                                        `(,(menu-bar categories 
                                                     :hierarchy 
                                                     "./"
                                                     packages)
                                          ,@(make-type-hierarchy-page lyst)))
                     (string-append directory "/hierarchy.html"))

    (for-each (lambda (chapter)
                (shtml:emit-file 
                 (html-boiler-plate (car chapter) 
                                    title 
                                    `(,(menu-bar categories 
                                                 #t "../" packages)
                                      ,@(chapter-content chapter chapter-list)))
                 (string-append directory "/"
                                (chapter-file-name chapter))))
              (cdr chapter-list))


    (for-each (lambda (cat)
                (shtml:emit-file 
                 (html-boiler-plate (category-name cat) 
                                    title 
                                    `(,(menu-bar categories 
                                                 cat "../" packages)
                                      ,(make-entry-list 
                                        (category-entries cat) "../")))
                 (string-append directory "/"
                                (category-index-name cat))))
              categories)

    (for-each (lambda (pkg)
                (shtml:emit-file 
                 (html-boiler-plate (string-append "Package "
                                                   (slot-ref pkg :name))
                                    title
                                    `(,(menu-bar categories pkg "../" packages)
                                      ,@(package-index pkg lyst)))
                 (string-append directory "/" 
                                (package-index-name pkg))))
              packages)
    
                
    (for-each (lambda (entry)
                (emit-one-entry entry directory title categories packages))
              lyst)))


(define (emit-core-documentation directory &key notes chapters)
  (emit-documentation 
   (get-toplevel-variables)
   directory
   "Default dfsch top-level environment"
   :notes notes
   :chapters chapters))

(define (emit-module-documentation directory module &key notes chapters)
  (emit-documentation 
   (get-module-variables module)
   directory
   (string-append (object->string module) " module")
   :notes notes
   :chapters chapters))

(when-toplevel
 (define module-name ())
 (define directory-name ())
 (define chapters-file-name ())
 (define notes-file-name ())

 (let ((parser (cmdopts:make-parser)))
   (cmdopts:add-option parser 
                       (lambda (p v)
                         (set! module-name (string->object v)))
                       :long-option "module"
                       :has-argument #t)
   (cmdopts:add-option parser
                       (lambda (p v)
                         (set! notes-file-name v))
                       :long-option "notes"
                       :has-argument #t)
   (cmdopts:add-option parser
                       (lambda (p v)
                         (set! chapters-file-name v))
                       :long-option "chapters"
                       :has-argument #t)
   (cmdopts:add-argument parser
                         (lambda (p v)
                           (set! directory-name v))
                         :required #t)
   (cmdopts:parse-list parser (cdr *posix-argv*)))
 
 (if module-name
     (emit-module-documentation directory-name module-name
                                :chapters chapters-file-name
                                :notes notes-file-name)
     (emit-core-documentation directory-name
                              :chapters chapters-file-name
                                :notes notes-file-name)))

;(emit-core-documentation "out")
