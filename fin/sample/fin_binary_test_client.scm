(use fin)
(use gauche.net)
(use binary.pack)

(define-method not ((str <string>))
  (pack "I" `(,(logxor #xffffffff (car (unpack "I" :from-string str)))) :to-string? #t))

(define (main args)
  (unless (eq? (gauche-character-encoding) 'none)
    (print "This script was written for testing fin's binary-reading.")
    (print "The test script for all environments was prepared on the top directory.")
    (exit 1))
  (let* ([sock (make-client-socket 'inet "127.0.0.1" 6543)]
         [iport (socket-input-port sock)]
         [oport (socket-output-port sock)]
         [fin   (make-fin iport)])
    (display (read-until fin "WELCOME\n"))
    (print   (read-line  fin))
    
    (display (read-until fin #/ADDR(:|=)/))
    (let1 x (read-nchars fin 4)
      (write x)
      (display (not x) oport)
      (flush oport))

    (display (read-all fin))
    ))
