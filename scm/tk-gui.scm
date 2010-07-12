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
             (make-instance <window> context "."))
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
   (window :reader widget-window)))

(define-method (initialize-instance (widget <widget>) parent type path 
                                    &rest args)
  (slot-set! widget :path (translate-widget-path parent path))
  (slot-set! widget :context (widget-context parent))
  (slot-set! widget :window (widget-window parent))
      
  (tcl-eval-list (widget-interpreter widget)
                 (append (list type (widget-path widget))
                         args)))

(define unique-widget-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format ".win~16r" counter))))

(define-macro (make-widget parent type &rest args)
  `(make-instance <widget> ,parent ,type (unique-widget-name) ,@args))

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
    (on-delete :accessor window-on-delete)
    (destroyed? :initform ()
                :reader window-destroyed?)))



(define unique-command-name
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (format "dfsch_cmd~16r" counter))))

(define-macro (make-window context &rest args)
  `(make-instance <window> ,context (unique-widget-name) ,@args))

(define-method (initialize-instance (win <window>) context path &rest args)
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


                       
;;;; Simple wrappers

(define-macro (define-simple-wrapper name command)
  `(define (,name context &rest args)
     (tcl-eval-list (context-interpreter context)
                    (cons ,command args))))

(define-simple-wrapper message-box "tk_messageBox")
(define-simple-wrapper open-dialog "tk_getOpenFile")
(define-simple-wrapper save-dialog "tk_getSaveFile")
(define-simple-wrapper directory-dialog "tk_chooseDirectory")

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
            (error "Unknown widget type" type)))))

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
                `(define-widget ,parent-var ,@spec))
              widget-spec)
       (unset! ,parent-var))))

