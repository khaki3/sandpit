#!/usr/bin/env gosh
(use terminal)

;;;
;;; Please set the environment variable TERM which supports 256-colors
;;;
;;;   like this: TERM=xterm-256color ./256color.scm
;;;
(define TERM (load-terminal-capability))
(define (change-background n)
  (tparm (TERM 'set-a-background) n))


(print "System colors:")
(dotimes (i 8 (print (change-background 16)))
  (display #`",(change-background i)  "))
(dotimes (i 8 (print (change-background 16)))
  (display #`",(change-background (+ i 8))  "))

(newline)

(print "Color cube, 6x6x6:")
(dotimes (green 6)
  (dotimes (red 6)
    (dotimes (blue 6)
      (let1 color (+ 16 (* red 36) (* green 6) blue)
        (display #`",(change-background color)  ")
        )
      )
    (display #`",(change-background 16) ")
    )
  (newline)
  )

(print "Grayscale ramp:")
(dotimes (color 24 (print (change-background 16)))
  (display #`",(change-background (+ color 232))  "))
