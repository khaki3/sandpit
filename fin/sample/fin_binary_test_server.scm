(use gauche.net)
(use binary.pack)
(use srfi-27)
(random-source-randomize! default-random-source)

(define (random-address)
  (pack "I" `(,(random-integer #x100000000)) :to-string? #t))

(define-method not ((str <string>))
  (pack "I" `(,(logxor #xffffffff (car (unpack "I" :from-string str)))) :to-string? #t))

(define (main args)
  (let* ([sock  (make-server-socket 'inet 6543)]
         [sock  (socket-accept sock)]
         [iport (socket-input-port sock)]
         [oport (socket-output-port sock)]
         [addr  (random-address)])
    (display "WELCOME\n" oport)
    (display "===========\n" oport)

    (if (= (random-integer 2) 0)
        (display "ADDR:" oport)
        (display "ADDR=" oport))
    (display addr oport)
    (flush oport)
    
    (if (string=? (not (read-string 4 iport)) addr)
        (begin
          (display "OK\n" oport)
          (display "PASSCODE is: " oport)
          (display "HUGAPIYO\n" oport))
        (display "NO\n" oport))
    
    (socket-shutdown sock SHUT_RDWR)
    (socket-close sock)
    ))
