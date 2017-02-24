;;;
;;; A buggy implementation of deforestation
;;; (Implemented by Gauche)
;;;
;;; References:
;;;  [1] M.H. Sørensen, R. Gluck, and N.D. Jones. 1993. A Positive Supercompiler.
;;;

;;
;; q ::= d1 .. dM
;;
;; d ::= (def (f v1 .. vN) t)
;;
;; t ::= v                     [variable]
;;     | (c t1 .. tN)          [constructor]
;;     | (f t1 .. tN)          [function call]
;;     | (case t0 (p1 t1) ..)  [case-expression]
;;     | (if (= t1 t2) t3 t4)  [conditional]
;;
;; p ::= c v1 .. vN
;;

(use gauche.record)
(use util.match)

;; var contains one represented by a symbol, or an immediate value.
(define-record-type var #t #t
  value)

(define-record-type con #t #t
  type args)

(define-record-type fun #t #t
  name args body)

(define-record-type app #t #t
  fun-name args)

(define-record-type cas #t #t
  key clauses)

(define-record-type ifs #t #t
  testl testr th el)

(define (df-reader :optional (iport (current-input-port)))
  (rlet1 funs '()
    (until (read iport) eof-object? => s
      (push! funs (src->record s))
      )))

(define (src->record s)
  (define (var->record s)
    (make-var s))
  (define (con->record s)
    (make-con (car s) (map src->record (cdr s))))
  (define (app->record s)
    (make-app (car s) (map src->record (cdr s))))
  (define (pat->record s)
    (if (list? s)
        (make-con (car s) (map var->record (cdr s)))
        (var->record s)))

  (match s
   [('def (name . args) body)
    (make-fun name (map var->record args) (src->record body))]

   [('if ('= t1 t2) t3 t4)
    (make-ifs (src->record t1)
              (src->record t2)
              (src->record t3)
              (src->record t4))]

   [('case t0 . clauses)
    (make-cas (src->record t0)
              (map (^[c] (cons (pat->record (car c))
                               (src->record (cadr c))))
                   clauses))]

   ;; temporary constructors
   [('CONS _ _) (con->record s)]
   [('NIL) (con->record s)]

   [(fun . args)
    (app->record s)]

   [else
    (var->record s)]
   ))

(define (record->src p)
  (cond
   [(fun? p)
    `(def (,(fun-name p) . ,(map record->src (fun-args p)))
          ,(record->src (fun-body p)))]

   [(ifs? p)
    `(if (= ,(record->src (ifs-testl p)) ,(record->src (ifs-testr p)))
         ,(record->src (ifs-th p)) ,(record->src (ifs-el p)))]

   [(cas? p)
    `(case ,(record->src (cas-key p))
       .
       ,(map
         (^[c]
           (list (record->src (car c))
                 (record->src (cdr c))))
         (cas-clauses p)))]

   [(app? p)
    `(,(app-fun-name p) . ,(map record->src (app-args p)))]

   [(con? p)
    `(,(con-type p) . ,(map record->src (con-args p)))]

   [(var? p)
    (var-value p)]
   ))

(define ENV '())

(define (search-fun fname)
  (find
   (^[f]
     (and (fun? f) (eq? (fun-name f) fname)))
   ENV))

(define (var-type-equal? v1 v2)
  (let ([b1 (var-value v1)]
        [b2 (var-value v2)])
    (or (and (symbol? b1) (symbol? b2))
        (and (number? b1) (number? b2))
        (and (char? b1)   (char? b2))
        )))

(define (var-equal? v1 v2)
  (equal? (var-value v1) (var-value v2)))

(define (con-map f c)
  (make-con
   (con-type c)
   (map f (con-args c))
   ))

(define (pat-match? pat c)
  (and (equal? (con-type pat) (con-type c))
       (= (length (con-args pat)) (length (con-args c)))
       ))

;; args ::= '( (sym . var) .. )
(define (drive q :optional (args ()))

  (define (valueize q)
    (or (and-let* ([(var? q)]
                   [value (var-value q)]
                   [(symbol? value)]
                   [r (assoc-ref args value)]) r)
        q))

  ;; append undefined pat-variables
  (define (append-pats p lst)
    (if (con? p)
        (append (map (^[x] (cons (var-value x) x)) (con-args p)) lst)
        lst))

  (cond [(fun? q)
         (let ([fname (fun-name q)]
               [fargs (fun-args q)])
           (make-fun fname fargs (drive (make-app fname fargs))))]

        ;; (1)
        [(var? q)
         (let ([v (valueize q)])
           (cond [(and (var? v) (not (var-equal? q v)))
                  (drive v args)]

                 [(con? v)
                  ;; use 'valueize' instead of 'drive'
                  ;; in order to disable infinite recursions
                  (con-map (^[x] (if (and (var? x) (var-equal? x q)) x (valueize x))) v)]

                 [else v]
                 ))]

        ;; (2)
        [(con? q)
         (con-map (cut drive <> args) q)]

        ;; (3)
        [(app? q)
         (let* ([drived-args (map (cut drive <> args) (app-args q))]
                [fname (app-fun-name q)]
                ;; high-order test
                [fname (or (and-let1 v (assoc-ref args fname)
                            (and (var? v) (var-value v)))
                           fname)])
           (or (and-let1 fun (search-fun fname)
                (drive (fun-body fun)
                       (map cons (map var-value (fun-args fun)) drived-args)))

               (make-app fname drived-args)
               ))]

        ;; (4) (5)
        [(cas? q)
         (let ([key     (drive (cas-key q) args)]
               [clauses (cas-clauses q)])
           (if (con? key)

               ;; (4)
               (let ([c (find (^[c] (pat-match? (car c) key)) clauses)])
                 (let ([p (car c)]
                       [s (cdr c)])
                   (drive s
                     (append
                      (map (^[pi vi]
                             (cons (var-value pi) vi))
                           (con-args p) (con-args key))
                      args))
                   ))

               ;; (5)
               (make-cas key
                 (map
                  (^[c]
                    (let* ([p (car c)] [s (cdr c)])
                      (cons c (drive s args))))
                  clauses))
               ))]

        ;; (6) (7) (8)
        [(ifs? q)
         (let ([testl (drive (ifs-testl q) args)]
               [testr (drive (ifs-testr q) args)])
           (if (and (var? testl) (var? testr) (var-type-equal? testl testr))

               ;; (6) (7)
               (drive ((if (var-equal? testl testr) ifs-th ifs-el) q)
                      args)

               ;; (8)
               (make-ifs
                testl testr
                (drive (ifs-th q) args)
                (drive (ifs-el q) args))

               ))]
        ))

(define (df-test funs)
  (set! ENV funs)
  (write (record->src (drive (search-fun 'main))))
  (newline))

(define (main args)
  (when (= (length args) 2)
    (df-test (call-with-input-file (~ args 1) df-reader))
    ))
