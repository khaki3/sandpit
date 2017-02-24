(define-module fgvm.util
  (use gauche.process)
  (use gauche.sequence)
  (export
   symbol-append
   plumb
   index
   fetch-cpunum
   ))
(select-module fgvm.util)

(define (symbol-append s1 s2)
  (string->symbol
   (string-append
    (symbol->string s1)
    (symbol->string s2)
    )))

(define (plumb obj)
  (let loop ([obj obj] [n 0])
    (cond [(not (list? obj)) n]
          [(null? obj) (+ n 1)]
          [else (apply max (map (cut loop <> (+ n 1)) obj))]
          )))

(define (index obj lst :optional obj=)
  (find-index (^[x] (obj= x obj)) lst))

(define (fetch-cpunum)
  (string->number (process-output->string '(nproc))))
