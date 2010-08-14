;;; dfsch - Scheme-like Lisp dialect
;;;   Highlevel wrapper for Tk
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

(require :tk-gui-interface)
(define-package :tk-gui :uses '(:dfsch :tk-gui%interface))
(in-package :tk-gui)

(define *waited-window* ())

(define-class <context> ()
  ((interpreter :reader context-interpreter)
   (top-level-window :reader context-toplevel-window)))

(define-macro (make-context &rest args)
  `(make-instance <context> (create-interpreter)
                  ,@args))

(define-method (initialize-instance (context <context>) 
                                    interpreter 
                                    &key withdraw-toplevel)
  (slot-set! context :interpreter interpreter)
  (slot-set! context :top-level-window
             (make-instance <window> context "." ()))
  (window-on-delete (context-toplevel-window context)
                    (lambda (window)
                      (withdraw-window window)))
  (when withdraw-toplevel
        (withdraw-window (context-toplevel-window context))))  

(define-method (wait-for-toplevel (context <context>))
  (wait-for-window (context-toplevel-window context)))

(define-class <widget> ()
  ((path :reader widget-path :initarg :path)
   (context :reader widget-context :initarg :context)
   (window :reader widget-window :initarg :window)))

(define-method (initialize-instance (widget <widget>) parent type path args)
  (call-next-method widget 
                    :path (translate-widget-path parent path)
                    :context (widget-context parent)
                    :window (widget-window parent))
      
  (tcl-eval-list (widget-interpreter widget)
                 (append (list type (widget-path widget))
                         args)))

(define unique-widget-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format ".win~16r" counter))))

(define-macro (make-widget parent type &rest args)
  `(make-instance <widget> ,parent ,type (unique-widget-name) (list ,@args)))

(define-method (translate-widget-path (widget <widget>) path)
  (string-append (widget-path widget) path))
(define-method (widget-interpreter (widget <widget>))
  (context-interpreter (widget-context widget)))

(define-macro (define-geometry-manager-method name manager)
  `(define-method (,name (widget <widget>) &rest args)
     (tcl-eval-list (widget-interpreter widget)
                    (append (list ,manager (widget-path widget))
                            args))
     widget))

(define-macro (define-geometry-manager-method-in name manager)
  `(define-method (name (widget <widget>) in &rest args)
     (tcl-eval-list (widget-interpreter widget)
                    (append (list ,manager (widget-path widget) 
                                  :in (widget-path in))
                            args))
     widget))

(define-geometry-manager-method pack-widget "pack")
(define-geometry-manager-method-in pack-widget-in "pack")

(define-geometry-manager-method grid-widget "grid")
(define-geometry-manager-method-in grid-widget-in "grid")

(define-geometry-manager-method place-widget "place")
(define-geometry-manager-method-in place-widget-in "place")

(define-method (grid-configure (widget <widget>) direction index &rest args)
  (tcl-eval-list (widget-interpreter widget)
                 (append (list "grid" 
                               (case direction 
                                 ((:row) "rowconfigure")
                                 ((:column) "columnconfigure")
                                 (else (error "Unknown direction" 
                                              :object direction)))
                               (widget-path widget)
                               index)
                         args)))
                               
  

(define-method (widget-command-list (widget <widget>) args)
  (tcl-eval-list (widget-interpreter widget)
                 (cons (widget-path widget)
                       args)))

(define-method (widget-command (widget <widget>) &rest args)
  (widget-command-list widget args))

(define-method (configure-widget (widget <widget>) &rest args)
  (widget-command-list widget (cons "configure" args))
  widget)

(define-method (validate-event-name (widget <widget>) name)
  #t)

(define-method (bind-event (widget <widget>) (event <symbol>) 
                           proc &optional args)
  (configure-widget widget event (bind-command (widget-window widget)
                                               proc)))

(define-method (bind-event (widget <widget>) (event <list>) 
                           proc &key args append)
  )


(define-class <window> <widget>
   ((command-list)
    (variable-list)
    (on-delete :accessor window-on-delete)
    (destroyed? :initform ()
                :reader window-destroyed?)))



(define unique-command-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format "dfsch_cmd~16r" counter))))

(define-macro (make-window context &rest args)
  `(make-instance <window> ,context (unique-widget-name) (list ,@args)))

(define-method (initialize-instance (win <window>) context path args)
  (slot-set! win :path path)
  (slot-set! win :context context)
  (slot-set! win :window win)
  (create-command! (widget-interpreter win)
                   (window-delete-command-name win)
                   (lambda ()
                     (if (window-on-delete win)
                         ((window-on-delete win) win)
                         (destroy-window win))))
  (unless (string=? path ".")
          (tcl-eval-list(widget-interpreter win)
                        (cons "toplevel" 
                              (cons path args))))
  (tcl-eval (widget-interpreter win)
        "wm" "protocol" path "WM_DELETE_WINDOW" 
        (window-delete-command-name win)))

(define-method (destroy-window (win <window>))
  (for-each (lambda (command)
              (delete-command! (widget-interpreter win) command))
            (slot-ref win :command-list))
  (for-each (lambda (variable)
              (unset-variable! (widget-interpreter win) variable))
            (slot-ref win :variable-list))
  (delete-command! (widget-interpreter win) 
                   (window-delete-command-name win))
  (tcl-eval (widget-interpreter win) "destroy" (widget-path win))
  (slot-set! win :destroyed? #t)
  (when (eq? win *waited-window*)
        (throw 'waited-window-destroyed win)))

(define-method (bind-command (win <window>) proc)
  (let ((cmd-name (unique-command-name)))
    (create-command! (widget-interpreter win) cmd-name proc)
    (slot-set! win :command-list (cons cmd-name (slot-ref win :command-list)))
    cmd-name))

(define-method (window-delete-command-name (win <window>))
  (format "dfsch_delete_handler_~a" (widget-path win)))

(define-method (wait-for-window (win <window>))
  (when (window-destroyed? win)
        (error "Cannot wait for destroyed window" :window win))
  (set! *waited-window* win)
  (catch 'waited-window-destroyed
         (event-loop))
  (set! *waited-window* ()))

(define-method (window-title (win <window>) &optional new)
  (if new
      (tcl-eval (widget-interpreter win) "wm" "title" (widget-path win) new)
      (tcl-eval (widget-interpreter win) "wm" "title" (widget-path win))))

(define-method (withdraw-window (win <window>))
  (tcl-eval (widget-interpreter win) "wm" "withdraw" (widget-path win)))

(define (wait)
  (event-loop))

;;;; Variables

(define unique-variable-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format "dfsch_var~16r" counter))))

(define-method (make-variable (ctx <context>))
  (let ((name (unique-variable-name)))
    (set-variable! (context-interpreter ctx) name "")
    name))

(define-method (make-variable (win <window>))
  (let ((name (make-variable (widget-context win))))
    (slot-set! win :variable-list
               (cons name
                     (slot-ref win :variable-list)))
    name))
  

                       
;;;; Simple wrappers

(define-macro (define-simple-wrapper name command)
  `(define (,name context &rest args)
     (tcl-eval-list (context-interpreter context)
                    (cons ,command args))))

(define-simple-wrapper message-box "tk_messageBox")
(define-simple-wrapper open-dialog "tk_getOpenFile")
(define-simple-wrapper save-dialog "tk_getSaveFile")
(define-simple-wrapper directory-dialog "tk_chooseDirectory")
(define-simple-wrapper color-dialog "tk_chooseColor")

(define (button-dialog context buttons 
                       &key 
                       (title "") (text "") (bitmap "question") (default 0))
  (tcl-eval-list (context-interpreter context)
                 (append (list "tk_dialog" (unique-widget-name) 
                               title text bitmap default)
                         buttons)))


;;;; Widgets definition macro

(define *manager-table*
  '((:pack pack-widget)
    (:grid grid-widget)
    (:place place-widget)))

(define *widget-type-table* ())

(define (register-widget-type type expander)
  (set! *widget-type-table* (cons (list type expander) *widget-type-table*)))

(define-macro (register-simple-widget-type type)
  `(register-widget-type ',type
                         (lambda (parent args)
                           (list 'make-instance 
                                 ',type 
                                 parent 
                                 (cons 'list args)))))


(define (get-manager-proc mgr)
  (let ((entry (assq mgr *manager-table*)))
    (unless entry
            (error "No such manager" :manager mgr))
    (cadr entry)))

(define-method (get-widget-construction-expander (type <symbol>))
  (if (keyword? type)
      (get-widget-construction-expander (keyword-name type))
      (let ((entry (assq type *widget-type-table*)))
        (if entry
            (cadr entry)
            (error "Unknown widget type" :type type)))))

(define-method (get-widget-construction-expander (type <string>))
  (lambda (parent args) 
    `(make-widget ,parent ,type ,@args)))
  

(define-macro (define-widget parent type args mgr mgr-args 
                &key variable contents events)
  (let ((tmp-widget (gensym)))
    `(begin
       (define ,tmp-widget ,((get-widget-construction-expander type)
                             parent args))
       (,(get-manager-proc mgr) ,tmp-widget ,@mgr-args)
       ,@(when variable
               `((define ,variable ,tmp-widget)))
       ,@(when contents
               `((define-widgets ,tmp-widget ,@contents)))
       ,@(map (lambda (event-spec)
                `(bind-event ,tmp-widget ,@event-spec))
              events)
       (unset! ,tmp-widget))))

(define-macro (define-widgets parent &rest widget-spec)
  (let ((parent-var (gensym)))
    `(begin
       (define ,parent-var ,parent)
       ,@(map (lambda (spec)
                (if (eq? (car spec) :call-with-parent)
                    `(,(cadr spec) ,parent-var ,@(cddr spec))
                    `(define-widget ,parent-var ,@spec)))
              widget-spec)
       (unset! ,parent-var))))

;;;; Entry widget

(define-class <entry> <widget>
  ())

(define-method (initialize-instance (widget <entry>) parent args)
  (call-next-method widget parent "entry" (unique-widget-name) args))

(register-simple-widget-type <entry>)

(define-method (get-value (entry <entry>))
  (widget-command entry "get"))

(define-method (set-value! (entry <entry>) value)
  (widget-command entry "delete" "0" "end")
  (widget-command entry "insert" "0" value))

;;;; Button widget

(define-class <basic-button> <widget>
  ())

(define (flash-button (button <button>))
  (widget-command button "flash"))

(define-class <button> <basic-button>
  ())

(define-method (initialize-instance (button <button>) parent args)
  (call-next-method button parent "button" (unique-widget-name) args))

(register-simple-widget-type <button>)


;;;; Checkbutton widget

(define-class <check-button> <basic-button>
  ((variable)))

(define-method (initialize-instance (button <check-button>) parent args)
  (let ((var (make-variable (widget-window parent))))
    (call-next-method button parent "checkbutton" (unique-widget-name) 
                      (append (list :variable var)
                              args))
    (slot-set! button :variable var)))

(register-simple-widget-type <check-button>)

(define-method (get-value (check <check-button>))
  (string=? (ref-variable (widget-interpreter check)
                          (slot-ref check :variable))
            "1"))

(define-method (set-value! (check <check-button>) value)
  (set-variable (widget-interpreter check)
                (slot-ref check :variable)
                (if value "1" "0")))


;;;; Radiobutton widget

(define-class <radio-button> <basic-button>
  ())

(define-method (initialize-instance (button <radio-button>) parent args)
  (call-next-method button parent "radiobutton" (unique-widget-name) args))

(register-simple-widget-type <radion-button>)
