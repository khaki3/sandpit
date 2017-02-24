;;;
;;; test script for fin
;;;

(use gauche.test)
(use gauche.net)
(use srfi-14)
(use srfi-27)
(random-source-randomize! default-random-source)

(test-start "fin")
(use fin)
(test-module 'fin)


(define-constant ONESEC (* 1000 1000))
(define (random-string n :optional (cset #[a-zA-Z0-9]))
  (let* ([src (char-set->string cset)]
         [len (string-length src)])
    (apply string
           (map (^[_] (~ src (random-integer len)))
                (make-list n)))))


(test-section "Setup test environment")
(define port (+ (random-integer (- 65536 49152)) 49152))
(define server-sock (make-server-socket 'inet port))
(define client-sock (make-client-socket 'inet "127.0.0.1" port))
(define server-sock (socket-accept server-sock))
(define server-oport (socket-output-port server-sock :buffering :none))
(define client-iport (socket-input-port client-sock))
(define fin (make-fin client-iport))


(test-section "check read-until (<regexp>)")
(dotimes (i 10)
  (test* #"read-until (<regexp>): ~i" #t
   (and-let* ([sended (random-string 15 #[ -z])]
              [(display sended server-oport)]
              [readed (read-until fin #/[ -z]{15}/ ONESEC)])
     (string=? sended readed)
     )))


(test-section "check read-until (<string>)")
(dotimes (i 10)
  (test* #"read-until (<string>): ~i" #t
   (and-let* ([sended (random-string (+ 1 (* i 10)) #[ -z])]
              [(display sended server-oport)]
              [readed (read-until fin sended ONESEC)])
     (string=? sended readed)
     )))


(test-section "check read-nchars")
(dotimes (i 10)
  (test* #"read-nchars: ~i" #t
   (and-let* ([len    (+ 1 (* i 10))]
              [sended (random-string len #[ -z])]
              [(display sended server-oport)]
              [readed (read-nchars fin len ONESEC)])
     (string=? sended readed)
     )))


(test-section "check read-line")
(dotimes (i 10)
  (test* #"read-line: ~i" #t
   (and-let* ([sended (random-string (+ 1 (* i 10)) #[ -z])]
              [(display sended server-oport)]
              [(newline server-oport)]
              [readed (read-line fin ONESEC)])
     (string=? sended readed)
     )))


(test-section "check read-all")
(test* #"read-all" #t
 (and-let* ([sended (random-string 1000 #[ -z])]
            [(display sended server-oport)]
            [(close-port server-oport)]
            [(socket-shutdown server-sock SHUT_RDWR)]
            [readed (read-all fin)])
     (string=? sended readed)
     ))


(test-section "Close test environment")
(close-port client-iport)
(socket-close server-sock)
(socket-close client-sock)

(test-end)
