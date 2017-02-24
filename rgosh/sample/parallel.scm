;;;
;;; partial parallel Gauche
;;;
(use gauche.threads)

;;; reflection
(define (_eval form)
  (cond [(and (pair? form) (eval (car form) #t)) procedure?
         => (^[head]
              (eval (cons `(quote ,head)
                          (cmap (^[x] `(quote ,(_eval x))) (cdr form)))
                    #t))]
        [else 
         (eval form #t)]
        ))
;;;

(define (cmap proc lst)
  (define (thread-proc x)
    (rlet1 thread (make-thread (^[] (proc x)))
      (thread-start! thread)))
  (define (terminated? t)
    (eq? (thread-state t) 'terminated))
  (let ([tlst (map thread-proc lst)])
    (let loop ()
      (if (every terminated? tlst)
          (map thread-join! tlst)
          (begin
            (sys-nanosleep 0)
            (loop))
          ))))

(define (sleep&print n)
  (sys-sleep n)
  (print n))

(print "sleep sort (1)")
(list
 (sleep&print 7)
 (sleep&print 2)
 (sleep&print 8)
 (sleep&print 9)
 (sleep&print 3)
 (sleep&print 4)
 (sleep&print 5)
 (sleep&print 0)
 (sleep&print 6)
 (sleep&print 1))

(print "\nsleep sort (2)")
(print 
(list (list
 (sleep&print 7)
 (sleep&print 2)
 (sleep&print 8)
 (sleep&print 9)
 (sleep&print 3)
 (sleep&print 4)
 (sleep&print 5)
 (sleep&print 0)
 (sleep&print 6)
 (sleep&print 1))))
