;;;
;;; A simple QR-code detector (OLD FILE)
;;;

;;;
;;; *** NO RECURSION & EASY IMPLEMENTATION FOR PORTING TO Verilog ***
;;;

;;;
;;; References
;;;
;;; * http://www.adobe.com/jp/devnet/flash/articles/qr_code_reader.html
;;;
(use gauche.record)
(use gauche.uvector)
(use gauche.collection)
(use srfi-11)
(use srfi-27)
(random-source-randomize! default-random-source)

(define HEADER #f)
(define RAW #f)
(define HEIGHT #f)
(define WIDTH #f)
(define PIXELS #f)


;;;
;;; [STEP1] IO
;;;   PPM P6(single-byte) only
;;;

(define (read-img)
  (set! HEADER (map (^_ (read-line)) (iota 3)))
  (let ([wh (map string->number (string-split (~ HEADER 1) #\space))])
    (set! WIDTH (~ wh 0))
    (set! HEIGHT (~ wh 1))
    (set! PIXELS (* HEIGHT WIDTH))
    (set! RAW (read-uvector <u8vector> (* PIXELS 3)))))

(define (output-img)
  (for-each print HEADER)
  (write-uvector RAW))


;;;
;;; [STEP2] binarize
;;;

(define RAW_BIN #f)

(define (binarize)
  ;; http://homepage2.nifty.com/tsugu/sotuken/ronbun/sec3-2.html
  (set! RAW_BIN (make-vector PIXELS))
  (dotimes (i PIXELS)
    (let* ([j (* i 3)]
           [r (~ RAW j)]
           [g (~ RAW (+ j 1))]
           [b (~ RAW (+ j 2))]
           [tmp 0]
           [tmp (+ tmp (* r 0.298912))]
           [tmp (+ tmp (* g 0.586611))]
           [tmp (+ tmp (* b 0.114478))])
      (vector-set! RAW_BIN i (< tmp 100))
      )))

(define (color! i col)
  (let ([j (* i 3)])
    (u8vector-set! RAW (+ j 0) (logand (ash col 0)   #xff))
    (u8vector-set! RAW (+ j 1) (logand (ash col -8)  #xff))
    (u8vector-set! RAW (+ j 2) (logand (ash col -16) #xff))
    ))

(define (bin-to-raw)
  (dotimes (i PIXELS)
    (let ([col (if (~ RAW_BIN i) #x000000 #xffffff)])
      (color! i col)
      )))


;;;
;;; [STEP3] labeling, symbol detection
;;;

(define LABEL #f)
(define LABEL_NUM #f)
(define LABEL_INFO #f)
(define STACK '())

(define-record-type linfo #t #t
  num
  (min-x) (max-x)
  (min-y) (max-y)
  (center-x) (center-y)
  (is-rect?) (is-symbol?)
  (region)
  )

(define (new-linfo)
  (make-linfo
   LABEL_NUM
   WIDTH -1
   HEIGHT -1
   #f #f
   #f #f
   #f
   ))

(define (labelize)
  (set! LABEL (make-u32vector PIXELS))
  (set! LABEL_NUM 0)
  (set! LABEL_INFO (list (new-linfo)))

  (dotimes (i PIXELS)
    (when (and (~ RAW_BIN i) (= (~ LABEL i) 0))
      (inc! LABEL_NUM)
      (push! STACK i)
      (new-label)
      ))

  (inc! LABEL_NUM)
  (set! LABEL_INFO (list->vector (reverse! LABEL_INFO))))

(define (refresh-linfo! linfo x y)
  (let ([min-x (linfo-min-x linfo)]
        [max-x (linfo-max-x linfo)]
        [min-y (linfo-min-y linfo)]
        [max-y (linfo-max-y linfo)])
    (push! (linfo-region linfo) (cons x y))
    (linfo-min-x-set! linfo (min min-x x))
    (linfo-max-x-set! linfo (max max-x x))
    (linfo-min-y-set! linfo (min min-y y))
    (linfo-max-y-set! linfo (max max-y y))
    ))

(define-constant MAX_BLOB_AREA 50000)
(define-constant MIN_BLOB_AREA 500)
(define-constant MAX_BLOB_RATIO 1.2)
(define-constant MIN_BLOB_RATIO (/ MAX_BLOB_RATIO))
(define-constant MAX_SYMBOL_RATIO (* MAX_BLOB_RATIO 1.41)) ; root of 2
(define-constant MIN_SYMBOL_RATIO (/ MAX_SYMBOL_RATIO))

(define (complement-linfo! linfo)
  (let* ([min-x (linfo-min-x linfo)]
         [max-x (linfo-max-x linfo)]
         [min-y (linfo-min-y linfo)]
         [max-y (linfo-max-y linfo)]
         [center-x (div (+ min-x max-x) 2)]
         [center-y (div (+ min-y max-y) 2)]
         [dx (- max-x min-x)]
         [dy (- max-y min-y)]
         [area (* dx dy)]
         [ratio (/. dx dy)])

    (define (is-rect?)
      (and (< area MAX_BLOB_AREA)   (> area MIN_BLOB_AREA)
           (< ratio MAX_BLOB_RATIO) (> ratio MIN_BLOB_RATIO)
           ))

    (define (is-symbol?)
      (define (judge count)
        (let* ([sum (fold + 0 count)]
               [ave (/ sum 5)]
               [count-list (u32vector->list count)]
               [ratio (/ sum (/ (+ dx dy) 2))])

          (and (< ratio MAX_SYMBOL_RATIO) (> ratio MIN_SYMBOL_RATIO)
               (> (~ count 2) ave)
               (every (cut > ave <>) (cdr (sort count-list >))))

          ;; (and (> (~ count 2) (* ave 2.5))
          ;;      (< (~ count 2) (* ave 3.5)))
          ))

      (and
       ;; horizontal
       (let* ([y center-y]
              [count (make-u32vector 5)]
              ; skip the first of white
              [x (let loop ([x min-x])
                   (if (~ RAW_BIN (xy->i x y)) x
                       (loop (+ x 1))))])

         (let loop ([x x] [index 0] [last #t])
           (let* ([current (~ RAW_BIN (xy->i x y))]
                  [index (if (eq? last current) index (+ index 1))])
             (when (and (<= x max-x) (< index 5))
               (inc! (~ count index))
               (loop (+ x 1) index current)
               )))

         (judge count))

       ;; vertical
       (let* ([x center-x]
              [count (make-u32vector 5)]
              ; skip the first of white
              [y (let loop ([y min-y])
                   (if (~ RAW_BIN (xy->i x y)) y
                       (loop (+ y 1))))])

         (let loop ([y y] [index 0] [last #t])
           (let* ([current (~ RAW_BIN (xy->i x y))]
                  [index (if (eq? last current) index (+ index 1))])
             (when (and (<= y max-y) (< index 5))
               (inc! (~ count index))
               (loop (+ y 1) index current)
               )))

         (judge count))
       ))

    (let ([is-rect? (is-rect?)])
      (linfo-center-x-set!   linfo center-x)
      (linfo-center-y-set!   linfo center-y)
      (linfo-is-rect?-set!   linfo is-rect?)
      (linfo-is-symbol?-set! linfo (and is-rect? (is-symbol?)))
      )))

(define (i->xy i)
  (values (mod i WIDTH) (div i WIDTH)))

(define (xy->i x y)
  (let* ([x (cond [(< x 0) 0]
                  [(>= x WIDTH) (- WIDTH 1)]
                  [else x])]
         [y (cond [(< y 0) 0]
                  [(>= y HEIGHT) (- HEIGHT 1)]
                  [else y])])
    (+ (* y WIDTH) x)))

(define (new-label)
  (define linfo (new-linfo))
  (linfo-region-set! linfo '())

  (until (null? STACK)
    (let*-values ([(i) (pop! STACK)]
                  [(x y) (i->xy i)])
      (when (and (~ RAW_BIN i) (= (~ LABEL i) 0))
        (u32vector-set! LABEL i LABEL_NUM)
        (refresh-linfo! linfo x y)

        (map
         (^[dx]
           (map
            (^[dy]
              (push! STACK (xy->i (+ x dx) (+ y dy))))
            '(-1 0 1)
            ))
         '(-1 0 1))

        )))

  (complement-linfo! linfo)
  (push! LABEL_INFO linfo))

(define (label-to-raw)
  ;; color labels of symbols
  (dotimes (i PIXELS)
    (when (linfo-is-symbol? (~ LABEL_INFO (~ LABEL i)))
      (color! i #xff0000)))

  ;; color labels of rectangles
  ;; (let ([COLOR (make-u8vector (* LABEL_NUM 3))])
  ;;   (dotimes (i LABEL_NUM)
  ;;     (when (linfo-is-rect? (~ LABEL_INFO i))
  ;;       (let ([j (* i 3)])
  ;;         (dotimes (k 3)
  ;;           (u8vector-set! COLOR (+ j k) (random-integer #x100))))))
  ;;   (dotimes (i PIXELS)
  ;;     (let ([j (* i 3)]
  ;;           [c (* (~ LABEL i) 3)])
  ;;       (dotimes (k 3)
  ;;         (u8vector-set! RAW (+ j k) (~ COLOR (+ c k)))
  ;;         ))))
  )


;;;
;;; [STEP4] extract QR
;;;

(define QR #f)
(define-record-type qr #t #t
  size body)

(define-syntax define-xy
  (syntax-rules ()
    [(_ p x y)
     (begin
       (define x (car p))
       (define y (cdr p)))]))

(define (extract-qr)
  (define (fave proc . args)
    (div (apply + (map proc args)) (length args)))
  (define (select-method linfo proc init o-x o-y) ; from point o
    (let loop ([region (linfo-region linfo)] [far #f] [dist init])
      (if (null? region) far
          (let* ([p (car region)]
                 [x (car p)]
                 [y (cdr p)]
                 [d (measure-distance x y o-x o-y)])
            (if (proc d dist)
                (loop (cdr region) p d)
                (loop (cdr region) far dist)
                )))))
  (define (select-farthest linfo o) (select-method linfo > -1 (car o) (cdr o)))
  (define (select-method2 linfo proc init a b c) ; from line ab and point c
    (define (sub p1 p2)
      (cons (- (car p1) (car p2)) (- (cdr p1) (cdr p2))))
    (define (dot p1 p2)
      (+ (* (car p1) (car p2)) (* (cdr p1) (cdr p2))))
    (define (cross-product p1 p2)
      (- (* (car p1) (cdr p2)) (* (cdr p1) (car p2))))
    (define ab (sub b a))
    (define-xy c c-x c-y)

    (let loop ([region (linfo-region linfo)] [far #f] [dist init])
      (if (null? region) far
          (let* ([p (car region)]
                 [d (+ (abs (cross-product (sub p a) ab))
                       (measure-distance c-x c-y (car p) (cdr p)))])
            (if (proc d dist)
                (loop (cdr region) p d)
                (loop (cdr region) far dist)
                )))))
  (define (select-nearest2 linfo a b c) (select-method2 linfo < PIXELS a b c))

  (let* ([symbols (find-symbols)]
         [L (~ symbols 0)]
         [M (~ symbols 1)]
         [O (~ symbols 2)]
         [center (cons (fave linfo-center-x M O) (fave linfo-center-y M O))]
         [top-left    (select-farthest L center)]
         [top-right   (select-farthest M center)]
         [bottom-left (select-farthest O center)]

         [L-top-right   (select-nearest2 L top-left top-right top-right)]
         [L-bottom-left (select-nearest2 L top-left bottom-left bottom-left)])
    (clip-qr top-left top-right bottom-left L-top-right L-bottom-left)

    ;; debug
    (line top-left L-top-right)
    (line top-left L-bottom-left)

    (rect center)
    (rect top-left)
    (rect top-right)
    (rect bottom-left)

    (rect L-bottom-left)
    (rect L-top-right)
    ))

(define (line a b)
  (define-xy a x1 y1)
  (define-xy b x2 y2)

  (let-values ([(x1 y1 x2 y2)
                (if (<= x1 x2)
                    (values x1 y1 x2 y2)
                    (values x2 y2 x1 y1))])
    (let* ([dx  (- x2 x1)]
           [dy  (- y2 y1)]
           [par (/. dy dx)])
      (unless (= dx 0)
        (let loop ([step 0])
          (when (<= step dx)
            (dotimes (h 2)
              (let ([i (xy->i (+ x1 step) (round->exact (+ y1 h (* par step))))])
                (color! i #x0000ff)))
            (loop (+ step 1))
            ))))))

(define (rect o)
  (define-xy o x y)

  (dotimes (i 3)
    (dotimes (j 3)
      (let ([k (xy->i (+ x i) (+ y j))])
        (color! k #x00ff00)
        ))))

(define (detect-qr-size x)
  ; todo
  53)

;;
;; L M
;; O
;;
;; L => |L R|
;;      |B  |
;;
(define (clip-qr top-left top-right bottom-left r b)
  (define (ave . args)
    (/ (apply + args) (length args)))
  (define-xy top-left    top-left-x    top-left-y)
  (define-xy top-right   top-right-x   top-right-y)
  (define-xy bottom-left bottom-left-x bottom-left-y)
  (define-xy r r-x r-y)
  (define-xy b b-x b-y)

  (let* ([LR-dx (- r-x top-left-x)]
         [LR-dy (- r-y top-left-y)]
         [LB-dx (- b-x top-left-x)]
         [LB-dy (- b-y top-left-y)]

         [LM-dx (- top-right-x   top-left-x)]
         [LM-dy (- top-right-y   top-left-y)]
         [LO-dx (- bottom-left-x top-left-x)]
         [LO-dy (- bottom-left-y top-left-y)]

         [LM-num-by-x (/. LM-dx (/ LR-dx 7))]
         [LM-num-by-y (/. LM-dy (/ LR-dy 7))]
         [LO-num-by-x (/. LO-dx (/ LB-dx 7))]
         [LO-num-by-y (/. LO-dy (/ LB-dy 7))]

         [size (detect-qr-size (ave LM-num-by-x LM-num-by-y LO-num-by-x LO-num-by-y))]
         [horizontal-dx (/ LM-dx size)]
         [horizontal-dy (/ LM-dy size)]
         [vertical-dx   (/ LO-dx size)]
         [vertical-dy   (/ LO-dy size)]

         [body (make-vector (* size size))]
         [start-x (+ top-left-x (/ horizontal-dx 2) (/ vertical-dx 2))]
         [start-y (+ top-left-y (/ horizontal-dy 2) (/ vertical-dy 2))])

    (dotimes (qr-x size)
      (dotimes (qr-y size)
        (let* ([x (round->exact (+ start-x (* qr-x horizontal-dx) (* qr-y vertical-dx)))]
               [y (round->exact (+ start-y (* qr-x horizontal-dy) (* qr-y vertical-dy)))]
               [i (xy->i x y)])
          ;; debug
          (color! i #xff00ff)
          (vector-set! body (+ qr-x (* qr-y size)) (~ RAW_BIN i))
          )))

    (set! QR (make-qr size body))
    ))

(define (qr-to-raw)
  (let ([size (qr-size QR)])
    (set! (~ HEADER 1) #"~size ~size")

    (fluid-let ([PIXELS  (* size size)]
                [RAW_BIN (qr-body QR)])
      (bin-to-raw)
      )))

(define (measure-distance x1 y1 x2 y2)
  (let ([dx (- x1 x2)]
        [dy (- y1 y2)])
    (+ (* dx dx) (* dy dy))))

(define (measure-linfo-distance linfo1 linfo2)
  (measure-distance
   (linfo-center-x linfo1) (linfo-center-y linfo1)
   (linfo-center-x linfo2) (linfo-center-y linfo2)))

;;
;; ________
;; |L    M |
;; |       |
;; |O   (N)|
;; ~~~~~~~~~
;;
;; (sort-symbols symbols) => '(L M O)
;;
(define (sort-symbols symbols)
  (let* ([sym0-sym1 (measure-linfo-distance (~ symbols 0) (~ symbols 1))]
         [sym0-sym2 (measure-linfo-distance (~ symbols 0) (~ symbols 2))]
         [sym1-sym2 (measure-linfo-distance (~ symbols 1) (~ symbols 2))]
         [max-dist  (max sym0-sym1 sym0-sym2 sym1-sym2)]

         [order (cond [(= max-dist sym0-sym1) '(2 0 1)]
                      [(= max-dist sym0-sym2) '(1 0 2)]
                      [(= max-dist sym1-sym2) '(0 1 2)])]
         [top-left (~ symbols (~ order 0))]
         [sym1     (~ symbols (~ order 1))]
         [sym2     (~ symbols (~ order 2))])

    ;;
    ;; http://www5d.biglobe.ne.jp/~noocyte/Programming/Geometry/RotationDirection.html
    ;;
    ;;  y
    ;;  /\
    ;;  | L M
    ;;  | O
    ;;  -----> x
    ;;
    ;;  ||
    ;;  ||  As image data
    ;;  \/
    ;;
    ;;  y
    ;;  /\
    ;;  | O
    ;;  | L M
    ;;  -----> x
    ;;
    ;; LM x LO = (M-x - L-x) * (O-y - L-y) - (M-y - L-y) * (O-x - L-x) > 0
    ;;

    (let* ([top-left-x (linfo-center-x top-left)]
           [top-left-y (linfo-center-y top-left)]
           [sym1-x     (linfo-center-x sym1)]
           [sym1-y     (linfo-center-y sym1)]
           [sym2-x     (linfo-center-x sym2)]
           [sym2-y     (linfo-center-y sym2)]
           [cross-product (- (* (- sym1-x top-left-x) (- sym2-y top-left-y))
                             (* (- sym1-y top-left-y) (- sym2-x top-left-x)))])
      (if (>= cross-product 0)
          (list top-left sym1 sym2)
          (list top-left sym2 sym1)
          ))))

(define (find-symbols)
  (let ([symbols
         (filter-map
          (^[i] (let1 linfo (~ LABEL_INFO i)
                  (and (linfo-is-symbol? linfo) linfo)))
          (iota LABEL_NUM))])

    (unless (= (length symbols) 3)
      (error "Invalid image"))
    (sort-symbols symbols)
    ))


;;;
;;; main
;;;

(define (main args)
  (read-img)
  (binarize)
  ;(bin-to-raw)
  (labelize)
  (label-to-raw)
  (extract-qr)
  (qr-to-raw)
  (output-img)
  )
