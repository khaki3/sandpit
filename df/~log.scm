% cat test1.df
(def (h k) k)
(def (main) (h 1))

% gosh df.scm test1.df
(def (main) 1)

---

% cat test2.df 
(def (k n)
  (m (m n)))

(def (m n)
  (case n
    ((NIL) 0)
    ((CONS x y) y)
    ))

(def (main)
  (k (CONS 1 (NIL))))

% gosh df.scm test3.df
(def (main) 0)

--

% cat map.df 
(def (map f x)
  (case x
    [(NIL) (NIL)]
    [(CONS x xs) (CONS (f x) (map f xs))]
    ))

(def (t x)
  (F1 (F2 x)))

(def (main)
  (map t (map A (map B (map C (CONS 1 (CONS 2 (CONS 3 (NIL)))))))))

% gosh df.scm map.df 
(def (main) (CONS (F1 (F2 (A (B (C 1))))) (CONS (t (A (B (C 2)))) (CONS (t (A (B (C 3)))) (NIL)))))

--

% cat sum.df
(def (sum x)
  (case x
    [(NIL) 0]
    [(CONS x xs) (+ x (sum xs))]
    ))

(def (map f x)
  (case x
    [(NIL) (NIL)]
    [(CONS x xs) (CONS (f x) (map f xs))]
    ))

(def (main)
  (sum (map A (map B (map C (CONS 1 (CONS 2 (CONS 3 (NIL)))))))))

% gosh df.scm sum.df 
(def (main) (+ (A (B (C 1))) (+ (A (B (C 2))) (+ (A (B (C 3))) 0))))
