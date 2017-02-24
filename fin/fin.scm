;;;
;;; fin - Gauche input-wrapper for flexible reading
;;; ( Inspired by https://github.com/hellman/sock )
;;;
;;; ** This library can be used on byte-string environments for binary-reading **
;;;

(define-module fin
  (use srfi-13)
  (use srfi-19)
  (use gauche.selector)
  (use gauche.uvector)
  (export <fin>
          make-fin
          fin-buffer
          fin-iport
          build-until
          read-until
          read-nchars
          read-line
          read-all
          ))
(select-module fin)

(define-class <fin> ()
  ([buffer   :init-value   ""        :accessor fin-buffer]
   [iport    :init-keyword :iport    :accessor fin-iport]
   [selector :init-keyword :selector :accessor fin-selector]
   ))

(define (make-fin iport)
  (let1 selector (make <selector>)
    (selector-add! selector iport (^[iport flag]) '(r))
    (set! (port-buffering iport) :modest)
    (make <fin>
      :iport iport
      :selector selector
      )))

;; Build the buffer until proc returns a value except for false.  (timeout [us])
;; When build succeeded, returns the value.
(define-method build-until ((self <fin>) proc :optional (timeout #f))
  (define (build!?)
    (let1 x (read-uvector <u8vector> #x1000 (fin-iport self))
      (if (eof-object? x) #f
          (begin 
            (set! (fin-buffer self) (string-append (fin-buffer self) (u8vector->string x)))
            #t)
          )))
  (if timeout
      (let loop ([timeout timeout])
        (let1 start (current-time)
          (cond [(proc (fin-buffer self))]
                [(< timeout 0) #f]
                [(= (selector-select (fin-selector self) timeout) 0) #f]
                [(not (build!?)) #f]
                [else
                 (loop
                  (- timeout 
                     (* (expt 10 6)
                        (time->seconds (time-difference (current-time) start)))))])))
      (let loop ()
        (cond [(proc (fin-buffer self))]
              [(build!?) (loop)]
              [else #f]))
      ))

(define-method read-until ((self <fin>) (re <regexp>) :optional (timeout #f))
  (and-let* ([match (build-until self re timeout)])
    (set! (fin-buffer self) (match 'after))
    (string-append (match 'before) (match))
    ))

(define-method read-until ((self <fin>) (str <string>) :optional (timeout #f))
  (and-let* ([start (build-until self (cut string-contains <> str) timeout)]
             [last (+ start (string-length str))])
    (rlet1 ret (string-take (fin-buffer self) last)
      (set! (fin-buffer self) (string-drop (fin-buffer self) last))
      )))

(define-method read-nchars ((self <fin>) (n <integer>) :optional (timeout #f))
  (and (build-until self (^[buf] (>= (string-length buf) n)) timeout)
       (let ([buffer (fin-buffer self)])
         (set! (fin-buffer self) (string-drop buffer n))
         (string-take buffer n))
       ))

(define-method read-line ((self <fin>) :optional (timeout #f))
  (and-let* ([x (read-until self #/[^\n]*\n/ timeout)])
    (string-drop-right x 1)
    ))

(define-method read-all ((self <fin>) :optional (timeout #f))
  (build-until self (^[_] #f) timeout)
  (rlet1 buffer (fin-buffer self)
    (set! (fin-buffer self) "")))
