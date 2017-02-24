(add-load-path "." :relative)
(use fgvm.core)
(use file.util)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args) ([file "i=s"] . restargs)
    (let ([IRs (compile (if file
                            (file->sexp-list file)
                            (port->sexp-list (current-input-port))))]
          [VM  (make-fgvm)])
      (fgvm-set-irs VM IRs)
      (fgvm-execute VM restargs)
      (fgvm-wait    VM)
      )))
