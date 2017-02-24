;;;
;;; Multilayer Feedforward Neural Network
;;;
;;; References
;;;   https://www.iwanami.co.jp/.BOOKS/01/3/0103560.html
;;;   http://arctrix.com/nas/python/bpnn.py
;;;   

(define-module ffnn
  (use srfi-11)
  (use srfi-27)
  (use math.const)
  (use util.combinations)
  (use gauche.sequence)
  (export <ffnn>
          make-ffnn
          train
          run
          ))
(select-module ffnn)

(define-class <ffnn> ()
  ([sizes   :init-keyword :sizes   :accessor ffnn-sizes]
   [weights :init-keyword :weights :accessor ffnn-weights]))

(define (make-ffnn . args)
  (define (random-weight) ; (-1, 1)
    (+ (- (random-integer 2) 1) (random-real)))
  (define (generate-weights)
    (let loop ([sizes args] [weights '()])
      (if (< (length sizes) 2) (reverse weights)
          (let ([src-size (car sizes)]
                [dest-size (cadr sizes)])
            (loop (cdr sizes)
                  (cons 
                   (fold (^[lst knil]
                           (acons (cons (car lst) (cadr lst)) (random-weight) knil))
                         '() (cartesian-product `(,(iota src-size) ,(iota dest-size))))
                   weights)
                  )))))
  (if (< (length args) 3) #f
      (make <ffnn> :sizes args :weights (generate-weights))
      ))

(define-syntax sum
  (syntax-rules ()
    [(_ pred lst ...) (apply + (map pred lst ...))]))

(define-method propagate ((self <ffnn>) input)
  (define sigmoid tanh)
  (let loop ([output (map sigmoid input)]
             [weights (ffnn-weights self)]
             [sizes (ffnn-sizes self)]
             [outputs '()])
    (if (null? weights) (values output (reverse (cons output outputs)))
        (let ([src-dest-weights (car weights)]
              [src-size (car sizes)]
              [dest-size (cadr sizes)])
          (define (calc k) ; calculate k-th of dest-output
            (sigmoid
             (sum
              (^[i]
                (let1 weight (assoc-ref src-dest-weights (cons i k))
                  (* (~ output i) weight)))
              (iota src-size))))
          (loop
           (map calc (iota dest-size))
           (cdr weights)
           (cdr sizes)
           (cons output outputs)
           ))
        )))

;; `outputs` means actual outputs of each layer
;; `target` means ideal output corresponding to `outputs`
(define-method backpropagate ((self <ffnn>) outputs target)
  (define alpha 0.01)
  (define (dsigmoid x)
    (- 1 (expt x 2)))
  (define (calculate-deltas)
    (let ([outputs (reverse outputs)]
          [weights (reverse (ffnn-weights self))]
          [sizes (reverse (ffnn-sizes self))])
      (let loop ([deltas
                  (cons (map (^[o t] (* (dsigmoid o) (- t o)))
                             (car outputs) target)
                        '())]
                 [outputs (cdr outputs)]
                 [weights weights]
                 [sizes sizes])
        (if (null? outputs) deltas
            (let ([src-dest-weights (car weights)]
                  [src-output (car outputs)]
                  [dest-size (car sizes)]
                  [dest-deltas (car deltas)])
              (loop
               (cons 
                (map-with-index
                 (^[i o]
                   (* (dsigmoid o)
                      (sum
                       (^[j d] (* d (assoc-ref src-dest-weights (cons i j))))
                       (iota dest-size)
                       dest-deltas)))
                 src-output)
                deltas)
               (cdr outputs)
               (cdr weights)
               (cdr sizes))
              )))))
  (let loop ([deltas (calculate-deltas)]
             [outputs outputs]
             [weights (ffnn-weights self)])
    (unless (length<=? deltas 1)
     (let1 src-dest-weights (car weights)
      (for-each-with-index
       (^[j d]
         (for-each-with-index
          (^[i o]
            (assoc-set!
             src-dest-weights (cons i j)
             (+ (assoc-ref src-dest-weights (cons i j)) (* alpha d o))))
          (car outputs)))
       (cadr deltas))
      (loop (cdr deltas) (cdr outputs) (cdr weights))
      ))))

;; supervisor = '(input . output)
(define-method train-one ((self <ffnn>) supervisor)
  (let ([input (car supervisor)]
        [target (cdr supervisor)])
    (let-values ([(output outputs) (propagate self input)])
      (backpropagate self outputs target)
      (/ (sum (^[o t] (expt (- t o) 2)) output target) 2))
    ))

(define-method train ((self <ffnn>) supervisors)
  (define threshold 0.01)
  (let loop ([supervisors (shuffle supervisors)])
    (unless (< (sum (cut train-one self <>) supervisors) threshold)
      (loop (shuffle supervisors)))))

(define-method run ((self <ffnn>) input)
  (let-values ([(output _) (propagate self input)])
    output))
