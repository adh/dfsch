(require :tk-gui-interface)
(define-package :tk-gui :dfsch :tk-gui%interface)
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

(define-method (bind-event (widget <widget>) event proc &optional args)
  (configure-widget widget event (bind-command (widget-window widget)
                                               proc)))

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

(define (button-dialog context buttons &rest args)
  (tcl-eval-list (context-interpreter context)
                 (append (list "tk_dialog" (unique-widget-name))
                         args buttons)))




