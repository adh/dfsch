(require :tk-gui)

(use-package :tk-gui)

(define context (make-context :withdraw-toplevel #t))

(define window (make-window context))
(window-on-delete window 
                  (lambda (window)
                    (when (string=? (message-box context
                                                 :message "Really quit?"
                                                 :type "yesno"
                                                 :icon "question")
                                    "yes")
                          (destroy-window window))))

(define entry
   (pack-widget 
    (make-widget window "entry" )))
(widget-command entry "insert" "0" "The Entry")
  

(pack-widget 
 (make-widget window "button" 
              :text "The Button"
              :command (bind-command 
                        window 
                        (lambda ()
                          (message-box context
                                       :message (format "Value is: ~s" 
                                                        (widget-command entry "get")))))))

(wait-for-window window)