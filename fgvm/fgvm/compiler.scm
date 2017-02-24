(define-module fgvm.compiler
  (use fgvm.data)
  (use fgvm.util)
  (use srfi-11)
  (use util.match)
  (use gauche.sequence)
  (export
   compile
   ))
(select-module fgvm.compiler)

(define (expand e)
  (match-let ([('def (name . args) . body) e])
    (cond

     ;; (def (name a1 a2) (f a1) (g a2))
     ;; => (def (name a1 a2) (do (name_0 a1 a2) (name_1 a1 a2)))
     ;;    (def (name_0 a1 a2) (f a1))
     ;;    (def (name_1 a1 a2) (g a2))
     [(> (length body) 1)
      (let ([n0 (symbol-append name '_0)]
            [n1 (symbol-append name '_1)])
        (list
         `(def (,name . ,args) (do (,n0 . ,args) (,n1 . ,args)))
         `(def (,n0 . ,args) ,(car body))
         `(def (,n1 . ,args) . ,(cdr body))
         ))]

     ;; (def (name a1) a1)
     ;; => (def (name a1) (values a1))
     [(= (plumb body) 1)
      (list
       `(def (,name . ,args) (values ,(car body))))]

     ;; (def (name a1) (if a1 (f1 (g1 a1)) (f2 (g2 a1))))
     ;; => (def (name a1) (if a1 (name_0 a1) (name_1 a1)))
     ;;    (def (name_0 a1) (f1 (g1 a1)))
     ;;    (def (name_1 a1) (f2 (g2 a1)))
     [(and (list? body) (eq? (caar body) 'if))
      (let* ([if-expr (car body)]
             [t-depth (plumb (~ if-expr 2))]
             [f-depth (plumb (~ if-expr 3))]
             [n0 (symbol-append name '_0)]
             [n1 (symbol-append name '_1)])
        (rlet1 ret '()
          (push! ret
           `(def (,name . ,args)
                 (if ,(~ if-expr 1)
                     ,(cond [(= t-depth 0) (list 'values (~ if-expr 2))]
                            [(= t-depth 1) (~ if-expr 2)]
                            [else (cons n0 args)])
                     ,(cond [(= f-depth 0) (list 'values (~ if-expr 3))]
                            [(= f-depth 1) (~ if-expr 3)]
                            [else (cons n1 args)]))))
          (when (> t-depth 1)
            (push! ret `(def (,n0 . ,args) ,(~ if-expr 2))))
          (when (> f-depth 1)
            (push! ret `(def (,n1 . ,args) ,(~ if-expr 3))))
          ))]

     ;; replace nesting if
     [else
      (rlet1 ret '()
       (letrec* ([replaced #f]
                 [replace
                  (match-lambda
                   [('if test t f)
                    (let ([n0 (symbol-append name '_0)])
                      (push! ret `(def (,n0 . ,args) (if ,test ,t ,f)))
                      `(,n0 . ,args))]
                   [(? list? lst) (map replace lst)]
                   [obj obj])])
         (push! ret `(def (,name . ,args) . ,(map replace body)))
         ))]

     )))

(define (extract body args)
  (let ([templates-with-index '()] [settings (make-list (length args) '())] [sum 0])
    (let rec ([body body] [waiting #f] [arg-index #f])

      (define (new!)
        (begin0 sum (inc! sum)))

      (define (filt obj waiting in)
        (cond [(and (pair? obj) (not (eq? (car obj) 'quote)))
               (begin (rec obj waiting in) #f)]
              [(index obj args eq?)
               => (^[j] (push! (~ settings j) (cons waiting in)) #t)]
              [else obj]))

      (define (add-template! self waiting arg-index nwaitings op args)
        (push! templates-with-index
          (cons self (make-packet waiting arg-index nwaitings op args #f))))

      (let ([self (new!)])
        (match body

          [('if test (t-op . t-args) (f-op . f-args))
           (let* ([if-index self]
                  [nwaitings (if (filt test if-index 0) 0 1)]
                  [t-index (new!)]
                  [f-index (new!)])
             (add-template! if-index
               #f #f nwaitings 'if (list test t-index f-index))
             (add-template! t-index
               #f #f 1 t-op (map-with-index (^[i obj] (filt obj t-index i)) t-args))
             (add-template! f-index
               #f #f 1 f-op (map-with-index (^[i obj] (filt obj f-index i)) f-args))
             )]

          [('do (s0-op . s0-args) (s1-op . s1-args))
           (let* ([s1-index self]
                  [s0-index (new!)])
             (add-template! s1-index
               #f #f 1 s1-op (map-with-index (^[i obj] (filt obj s1-index i)) s1-args))
             (add-template! s0-index
               s1-index #f 0 s0-op (map-with-index (^[i obj] (filt obj s0-index i)) s0-args))
             )]

          [(op . args)
           (let* ([new-args  (map-with-index (^[i obj] (filt obj self i)) args)]
                  [nwaitings (- (length new-args) (count values new-args))])
             (add-template! self
               waiting arg-index nwaitings op new-args))]

          )))

    (values (map cdr (sort-by templates-with-index car)) settings)
    ))

(define (translate definition)
  (match-let ([('def (name . args) body) definition])
    (let-values ([(templates settings) (extract body args)])
      (make-ir name templates settings)
      )))

(define (generate exprs)
  (map translate exprs))

(define (compile exprs)
  (let1 x (append-map expand exprs)
    (if (equal? x exprs) (generate exprs)
        (compile x)
        )))
