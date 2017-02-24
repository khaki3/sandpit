(define-module fgvm.vm
  (use fgvm.data)
  (use fgvm.util)
  (use util.queue)
  (use util.match)
  (use gauche.record)
  (use gauche.threads)
  (use gauche.sequence)
  (export
   make-fgvm
   fgvm-set-irs
   fgvm-execute
   fgvm-wait
   ))
(select-module fgvm.vm)

(define-record-type fgvm construct-fgvm #t
  queue table (threads) (nrunnings)
  cond-no-tasks mutex-no-tasks (stop))

(define (make-fgvm)
  (construct-fgvm
   (make-mtqueue)
   (make-hash-table)
   '()
   0
   (make-condition-variable)
   (make-mutex)
   #f
   ))

(define (fgvm-set-irs VM IRs)
  (for-each
   (^[ir]
     (hash-table-put! (fgvm-table VM)
       (ir-name ir) ir))
   IRs
   ))

(define (fgvm-execute VM args :optional (cpunum (fetch-cpunum)))
  (when (extract-ir VM (make-packet #f #f 0 'main (list args) #f))
    (set! (fgvm-nrunnings VM) cpunum)
    (set! (fgvm-threads  VM)
      (map (^[_] (thread-start! (make-thread (^[] (thread-work VM)))))
           (iota cpunum)
           ))
    ))

(define (fgvm-wait VM)
  (let ([mu (fgvm-mutex-no-tasks VM)]
        [cv (fgvm-cond-no-tasks VM)])
    (mutex-lock! mu)

    (while (> (fgvm-nrunnings VM) 0)
      (mutex-unlock! mu cv)
      (mutex-lock! mu))

    (set! (fgvm-stop VM) #t)
    (condition-variable-broadcast! cv)
    (mutex-unlock! mu)

    (guard (e [(<thread-exception> e)
               (raise (uncaught-exception-reason e))])
      (for-each thread-join! (fgvm-threads VM)))
    (set! (fgvm-threads  VM) #f)
    (set! (fgvm-nrunnings VM) #f)
    (set! (fgvm-stop     VM) #f)
    ))

(define (fetch-task VM)
  (let ([queue (fgvm-queue VM)])
    (or (dequeue! queue #f)
        (let ([mu (fgvm-mutex-no-tasks VM)]
              [cv (fgvm-cond-no-tasks VM)]
              [ret #f])

          (mutex-lock! mu)
          (dec! (fgvm-nrunnings VM))

          (when (= (fgvm-nrunnings VM) 0)
            (condition-variable-broadcast! cv))

          (until (or (and-let1 x (dequeue! queue #f) (set! ret x) x)
                     (fgvm-stop VM))
            (mutex-unlock! mu cv)
            (mutex-lock! mu))

          (inc! (fgvm-nrunnings VM))
          (mutex-unlock! mu)

          ret

          ))))

(define (set-task VM . packets)
  (let* ([queue (fgvm-queue VM)]
         [x (queue-empty? queue)])
    (apply (pa$ enqueue! queue) packets)
    (when x
      (with-locking-mutex (fgvm-mutex-no-tasks VM)
        (cut condition-variable-broadcast! (fgvm-cond-no-tasks VM)))
      )))

(define (thread-work VM)
  (with-error-handler
   (^[e]
     (with-locking-mutex (fgvm-mutex-no-tasks VM)
      (^[]
        (dec! (fgvm-nrunnings VM))
        (condition-variable-broadcast! (fgvm-cond-no-tasks VM))
        ))
     (raise e))
   (^[]
     (while (fetch-task VM) => packet
       (execute-packet VM packet))
     )))

(define (execute-packet VM packet)
  (or (extract-ir VM packet)
      (extract-if VM packet)
      (let* ([op        (packet-op packet)]
             [args      (packet-args packet)]
             [waiting   (packet-waiting packet)]
             [arg-index (packet-arg-index packet)]
             [result    (apply (eval op (current-module)) args)])
        (when waiting
          (when arg-index
            (set! (~ (packet-args waiting) arg-index) result))

          (with-locking-mutex (packet-mutex-write waiting)
            (cut dec! (packet-nwaitings waiting)))

          (when (= (packet-nwaitings waiting) 0)
            (set-task VM waiting))
          ))))

(define (make-packet-from-template template)
  (make-packet
   (packet-waiting template)
   (packet-arg-index template)
   (packet-nwaitings template)
   (packet-op template)
   (list-copy (packet-args template))
   (make-mutex)))

(define (extract-ir VM packet)
  (and-let* ([op (packet-op packet)]
             [ir (hash-table-get (fgvm-table VM) op #f)])
    (let* ([waiting   (packet-waiting packet)]
           [arg-index (packet-arg-index packet)]
           [args      (packet-args packet)]
           [packet-templates (ir-packet-templates ir)]
           [arg-settings     (ir-arg-settings ir)]
           [packets (map make-packet-from-template packet-templates)])

      ;; Set args
      (for-each-with-index
       (^[i set]
         (for-each
          (match-lambda
           [(packet-index . arg-index)
            (set! (~ (packet-args (~ packets packet-index)) arg-index)
                  (~ args i))])
          set))
       arg-settings)

      ;; Rewrite the destination of the head
      (unless (null? packets)
        (set! (packet-waiting   (car packets)) waiting)
        (set! (packet-arg-index (car packets)) arg-index))

      ;; Set waitings & enqueue packets
      (apply (pa$ set-task VM)
        (filter
         (^[pac]
           (and-let* ([waiting-rel (packet-waiting pac)]
                      [(integer? waiting-rel)])
             (set! (packet-waiting pac) (~ packets waiting-rel)))

           (and-let* ([op   (packet-op pac)]
                      [(eq? op 'if)]
                      [args (packet-args pac)])
             (let ([t-rel (~ args 1)]
                   [f-rel (~ args 2)])
               (and (integer? t-rel) (set! (~ args 1) (~ packets t-rel)))
               (and (integer? f-rel) (set! (~ args 2) (~ packets f-rel)))
               ))

           ;; whether or not args are prepared
           (= (packet-nwaitings pac) 0))

         packets))

      )))

(define (extract-if VM packet)
  (and-let* ([(eq? (packet-op packet) 'if)]
             [args (packet-args packet)]
             [next (if (~ args 0) (~ args 1) (~ args 2))])
    (set! (packet-waiting next) (packet-waiting packet))
    (set! (packet-arg-index next) (packet-arg-index packet))
    (set-task VM next)
    ))
