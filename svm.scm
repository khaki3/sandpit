;;; Buggy Implementation of Nonlinear SVM
;;; 
;;; References
;;;   http://www.coronasha.co.jp/np/isbn/9784339027518/
;;;   http://www.neuro.sfc.keio.ac.jp/~masato/study/SVM/SVM_3_1.htm
(use srfi-11)
(use math.const)
(use gauche.record)

(define-record-type svm construct-svm #t data ws b) ; data is training data

;; Utility
(define-syntax sum
  (syntax-rules ()
    [(_ pred lst ...) (apply + (map pred lst ...))]))

;; Polynomial kernel
;; x1, x2 : vector (<list>)
;; d,  r  : kernel parameter
(define-constant d 2)
(define-constant r 1)
(define (polynomial-kernel x1 x2)
  (expt (+ r (sum * x1 x2)) d))

;; Radial Basis Function kernel (RBF kernel)
;; x1, x2 : vector (<list>)
;; sigma : kernel parameter
(define-constant sigma 50)
(define (rbf-kernel x1 x2)
  (expt e (- (/ (sum (^[i j] (expt (- i j) 2)) x1 x2) (* 2 (expt sigma 2))))))

;; kernel setting
(define kernel polynomial-kernel)
;(define kernel rbf-kernel)

;; vec := (x_1 x_2 ..)
;; inssig := #t | #f
;; data := ((vec_1 . inssig_1) ..)
(define (make-svm data :optional (lrate 0.05) (conv 0.01))
  ;; #t => 1, #f => -1
  (define (boolean->inssig b) (if b 1 -1))
  ;; normalize returns the initial values of the Lagrange multipliers.
  (define (normalize data)
    (let ([inssigs (map cdr data)])
      (let-values ([(pos neg) (partition identity inssigs)])
        (map (^x (if x (length neg) (length pos))) inssigs))))
  ;; calculate the Lagrange multipliers.
  (define (calc-multipls multipls)
    (define (displace datum)
      (* lrate (- 1
      (sum (^[m d] (* (boolean->inssig (cdr datum))
                      m (boolean->inssig (cdr d))
                      (kernel (car datum) (car d))))
           multipls data))))
    (let ([displacements (map displace data)]
          [new (map + data displacements)])
      (if (every (^[x] (< x conv)) displacements) new
          (calc-multipls new))))
  (define (calc-b data multipls ws)
    (let ([sv (any (^[d m] (and (> m 0) d)) data multipls)])
      (- (boolean->inssig (cdr sv))
         (sum (^[vec w] (* w (kernel vec (car sv)))) (map car data) ws))))
  (let* ([multipls (normalize data)]
         [ws (map * (map (.$ boolean->inssig cdr) data) multipls)]
         [b (calc-b data multipls ws)])
    (construct-svm data ws b)))

;; classify a vector
(define-method object-apply ([s svm] [input-vector <list>])
  (let ([vecs (map car (svm-data s))] [ws (svm-ws s)] [b (svm-b s)])
    (define (g vec w) (* (kernel vec input-vector) w))
    (positive? (+ b (sum g vecs ws)))))



;;;
;;; make the corpus
;;; 
(use text.csv)
(use gauche.sequence)
(use srfi-27)
(random-source-randomize! default-random-source)

;; Utility for making the corpus
(define (without-last lst)
    (take lst (- (length lst) 1)))
(define (number->boolean num)
    (not (= num 0)))

(define corpus
  (let* ([reader (make-csv-reader #\,)]
         ;; http://archive.ics.uci.edu/ml/datasets/Spambase
         [iport (open-input-file "./corpus/spambase.data")]
         [data (map (map$ string->number) (port->list reader iport))]
         [data (shuffle data)]) ; shuffle to get the randomized corpus
    (map (^[lst] (cons (without-last lst) (number->boolean (last lst)))) data)))



;;; usage
;;;   (define s (make-svm (without-last corpus)))
;;;   (s (car (last corpus)))
