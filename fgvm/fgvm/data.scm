(define-module fgvm.data
  (use gauche.record)
  (export-all))
(select-module fgvm.data)

(define-record-type ir #t #t
  name packet-templates arg-settings)

(define-record-type packet #t #t
  (waiting) (arg-index) (nwaitings) op args (mutex-write))
