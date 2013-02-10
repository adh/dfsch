#!/usr/bin/env dfsch-run

(require :tk-gui)

(set! tk-gui:*do-ttk-mapping* #f)

(use-package :tk-gui)

(define context (make-context :withdraw-toplevel #t))

(define window (make-window context))
;(window-on-delete window 
;                  (lambda (window)
;                    (destroy-window! window))))
(window-title window "dfsch calculator")

(define-variable start-input #t)

(define (number-input str)
  (lambda ()
    (configure-widget entry :state "normal")
    (when start-input
          (set-value! entry "")
          (set! start-input #f))
    (entry-append! entry str)
    (configure-widget entry :state "readonly")
    ""))

(define (set-entry-value! value)
  (configure-widget entry :state "normal")
  (set-value! entry value)
  (set! start-input #t)
  (configure-widget entry :state "readonly")
  "")

(define-variable register "0")
(define-variable op (lambda (x y) x))

(define (parse-number str)
  (if (equal? str "")
      0
      (string->number str)))

(define (do-it!)
  (set-entry-value! (or (ignore-errors
                         (number->string (op (parse-number register)
                                             (parse-number (get-value entry)))))
                        "ERR")))

(define (make-op proc)
  (lambda ()
    (unless (equal? "" (get-value entry))
            (set! register (get-value entry)))
    (set-entry-value! "0")
    (set! op proc)))

(define-widgets window
  (<entry> (:state "readonly") 
           :grid (:row 0 :column 0 :columnspan 4  :sticky "nsew") 
           :variable entry)
  (<button> (:text "7")
            :grid (:row 1 :column 0 :sticky "nsew")
            :variable b7
            :events ((:command (number-input "7"))))
  (<button> (:text "8")
            :grid (:row 1 :column 1 :sticky "nsew")
            :variable b8
            :events ((:command (number-input "8")))) 
  (<button> (:text "9")
            :grid (:row 1 :column 2 :sticky "nsew")
            :variable b9
            :events ((:command (number-input "9"))))
  (<button> (:text "/")
            :grid (:row 1 :column 3 :sticky "nsew")
            :variable bdiv
            :events ((:command (make-op /))))
  (<button> (:text "4")
            :grid (:row 2 :column 0 :sticky "nsew")
            :variable b4
            :events ((:command (number-input "4"))))
  (<button> (:text "5")
            :grid (:row 2 :column 1 :sticky "nsew")
            :variable b5
            :events ((:command (number-input "5")))) 
  (<button> (:text "6")
            :grid (:row 2 :column 2 :sticky "nsew")
            :variable b6
            :events ((:command (number-input "6"))))
  (<button> (:text "*")
            :grid (:row 2 :column 3 :sticky "nsew")
            :variable bmul
            :events ((:command (make-op *))))
  (<button> (:text "1")
            :grid (:row 3 :column 0 :sticky "nsew")
            :variable b1
            :events ((:command (number-input "1"))))
  (<button> (:text "2")
            :grid (:row 3 :column 1 :sticky "nsew")
            :variable b2
            :events ((:command (number-input "2")))) 
  (<button> (:text "3")
            :grid (:row 3 :column 2 :sticky "nsew")
            :variable b3
            :events ((:command (number-input "3"))))
  (<button> (:text "-")
            :grid (:row 3 :column 3 :sticky "nsew")
            :variable bsub
            :events ((:command (make-op -))))
  (<button> (:text "0")
            :grid (:row 4 :column 0 :columnspan 2 :sticky "nsew")
            :variable b0
            :events ((:command (number-input "0"))))
  (<button> (:text ".")
            :grid (:row 4 :column 2 :sticky "nsew")
            :variable bdp
            :events ((:command (number-input ".")))) 
  (<button> (:text "+")
            :grid (:row 4 :column 3 :sticky "nsew")
            :variable badd            
            :events ((:command (make-op +))))


  (<button> (:text "DEL")
            :grid (:row 0 :column 4 :sticky "nsew")
            :variable bdel
            :events ((:command (lambda ()
                                 (ignore-errors
                                  (set-entry-value! 
                                              (substring (get-value entry)
                                                         0 -2)))))))
  (<button> (:text "CLR" :background "darkred")
            :grid (:row 1 :column 4 :sticky "nsew")
            :variable bclr
            :events ((:command (lambda ()
                                 (set-entry-value! "")))))
  (<button> (:text "AC" :background "darkred")
            :grid (:row 2 :column 4 :sticky "nsew")
            :variable bac
            :events ((:command (lambda ()
                                 (set-entry-value! "")
                                 (set! register "0")))))

  (<button> (:text "=" :default "active" :background "darkgreen")
            :grid (:row 3 :rowspan 2 :column 4 :sticky "nsew")
            :variable beq
            :events ((:command do-it!))))

(grid-configure window :column "all" :weight 1)
(grid-configure window :row "all" :weight 1)

(define (make-shortcut-handler btn)
  (lambda () 
;    (flash-button! btn)
    (invoke-button! btn)))

(bind-event window '("1") (make-shortcut-handler b1))
(bind-event window '("2") (make-shortcut-handler b2))
(bind-event window '("3") (make-shortcut-handler b3))
(bind-event window '("4") (make-shortcut-handler b4))
(bind-event window '("5") (make-shortcut-handler b5))
(bind-event window '("6") (make-shortcut-handler b6))
(bind-event window '("7") (make-shortcut-handler b7))
(bind-event window '("8") (make-shortcut-handler b8))
(bind-event window '("9") (make-shortcut-handler b9))
(bind-event window '("0") (make-shortcut-handler b0))

(bind-event window '(".") (make-shortcut-handler bdp))

(bind-event window '("+") (make-shortcut-handler badd))
(bind-event window '("-") (make-shortcut-handler bsub))
(bind-event window '("*") (make-shortcut-handler bmul))
(bind-event window '("/") (make-shortcut-handler bdiv))

(bind-event window '("=") (make-shortcut-handler beq))

(bind-event window '("<BackSpace>") (make-shortcut-handler bdel))
(bind-event window '("<Delete>") (make-shortcut-handler bclr))
(bind-event window '("<Escape>") (make-shortcut-handler bac))
(bind-event window '("<Return>") (make-shortcut-handler beq))

(set-entry-value! "0")

(wait-for-window window)