(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
(define numer car)
(define denom cdr)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (+ (* (denom x) (denom y)))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (+ (* (denom x) (denom y)))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) ( denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; Ex. 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (if (< d 0)
          (cons (- n) (- d))
          (cons n d)))))

(make-rat 1 2)
(make-rat -1 2)
(make-rat 1 -2)
(make-rat -1 -2)

; Ex. 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (mid-segment s)
  (define (average x y) (/ (+ x y) 2))
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Ex. 2.3
(define (make-rectangle p1 p2)
  (cons p1 p2))
(define upper-left car)
(define lower-right cdr)
(define (perimeter rec)
  (let ((w (abs (- (x-point (upper-left rec)) (x-point (lower-right rec)))))
        (h (abs (- (y-point (upper-left rec)) (y-point (lower-right rec))))))
    (* (+ w h) 2)))
(define (area rec)
  (let ((w (abs (- (x-point (upper-left rec)) (x-point (lower-right rec)))))
        (h (abs (- (y-point (upper-left rec)) (y-point (lower-right rec))))))
    (* w h)))

(define (make-rectangle p w h)
  (cons p (cons w h)))
(define (width rec)
  (car (cdr rec)))
(define (height rec)
  (cdr (cdr rec)))
(define (point rec)
  (car rec))
(define (perimeter rec)
  (* (+ (width rec) (height rec)) 2))
(define (area rec)
  (* (width rec) (height rec)))

; Ex. 2.4
(define (excons x y)
    (lambda (m) (m x y)))
(define (excar z)
    (z (lambda (p q) p)))
(define (excdr z)
  (z (lambda (p q) q)))

; excons returns a lambda that receives a lambda m
; and returns the application of excons's arguments to m.
; excar receives the lambda the excons returns
; and passes it a lambda that takes two arguemtns and evaluates to the first one,
; which is the first argument given to the excons.
; If we instead pass a lambda that takes two arguments and evaluates to the second one,
; that's our cdr!

; Ex. 2.5
(define (ncons x y)
  (define (pow b p)
    (if (= p 0)
        1
        (* b (pow b (- p 1)))))
  (* (pow 2 x) (pow 3 y)))
(define (div x y)
  (if (= (remainder x y) 0)
      (+ 1 (div (/ x y) y))
      0))
(define (ncar z)
  (div z 2))
(define (ncdr z)
  (div z 3))

; Ex. 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))

; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; Ex. 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; a is lower-bound and b is upper-bound
(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

; Ex. 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

; Ex. 2.9
; Let x be an interval, xl be x's lower bound, xu be x's upper bound and w be x's width.
; We have xu = xl + 2w.
; Similarly, let y be an interval, yl be y's lower bound, yu be y's upper bound and w' be y's width.
; We have yu = yl + 2w'.
;
; By adding x and y, we have an interval with lower bound
;   xl + yl
; and higher bound
;   xl + yl + 2 * (w + w')
; Thus, the resulting interval's width is  w + w'.
;
; Since interval subtraction is defined in terms of interval addition,
; The above reasoning works for interval subtraction as well.
;
; Now let's suppose that the lower bounds of both x and y are positive.
; If we multiply x and y, the resulting interval has lower bound
;   xlyl
; and upper bound
;   (xl + 2w)(yl + 2w')
;   = xlyl + 2w'xl + 2wyl + 4ww'
; Thus its width cannot be represented as a function of only w and w',
; but we also need to take xl and yl into account.
;
; (mul-interval (make-interval 1 5) (make-interval 1 5))
; (mul-interval (make-interval 1 5) (make-interval -1 3))

; Ex. 2.10
(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "Divisor must not span on 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Ex. 2.11
(define (mul-interval x y)
  (let ((l1 (lower-bound x))
        (u1 (upper-bound x))
        (l2 (lower-bound y))
        (u2 (upper-bound y)))
    (cond ((<= u1 0)
           (cond ((<= u2 0)
                  (make-interval (* u1 u2) (* l1 l2)))
                 ((>= l2 0)
                  (make-interval (* l1 u2) (* u1 l2)))
                 (else
                  (make-interval (* l1 u2) (* l1 l2)))))
          ((>= l1 0)
           (cond ((<= u2 0)
                  (make-interval (* u1 l2) (* l1 u2)))
                 ((>= l2 0)
                  (make-interval (* l1 l2) (* u1 u2)))
                 (else
                  (make-interval (* u1 l2) (* u1 u2)))))
          (else
           (cond ((<= u2 0)
                  (make-interval (* u1 l2) (* l1 l2)))
                 ((>= l2 0)
                  (make-interval (* l1 u2) (* u1 u2)))
                 (else
                  (make-interval
                   (min (* l1 u2) (* u1 l2))
                   (* u1 u2))))))))

; Ex. 2.12
(define (make-center-percent c pct)
  (let ((w (/ (* c pct) 100)))
    (make-interval (- c w) (+ c w))))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (* (width i) 100) (center i)))

(percent (make-center-percent 100 10))

; Ex. 2.13
; Let c1 and c2 be the centers of two intervals,
; and T1 and T2 be the percentage tolerance of each intervals, respectively.
; Let t1 and t2 be the ratio of tolerance, i.e. t1 = T1 / 100 and t2 = T2 / 100.
; Specifically, 0 < t1, t2 < 1.
; Two intervals can be written as
;   (c1 - t1c1, c1 + t1c1)
;   (c2 - t2c2, c2 + t1c2)
; and the product of them be
;   ((1 - t1 - t2 + t1t2) * c1c2, (1 + t1 + t2 + t1t2) * c1c2)
; The center of the product is
;   (1 - t1 - t2 + t1t2 + 1 + t1 + t2 + t1t2) * c1c2 / 2
;   = (1 + t1t2) * c1c2
; and the width of the product is
;   (1 + t1 + t2 + t1t2 - 1 + t1 + t2 + t1t2) * c1c2 / 2
;   = (t1 + t2) * c1c2
; Thus, the ratio of tolerance of the product is
;  (t1 + t2) / (1 + t1t2)
; Now, if t1 and t2 are small enough, we can ignore the term t1t2,
; and can approximate the resulting percentage tolerance as
;   100 * (t1 + t2)
;   = T1 + T2

; Ex. 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(center (par1 (make-center-percent 10000 1) (make-center-percent 999999 5)))
; 9959.915092147685
(percent (par1 (make-center-percent 10000 1) (make-center-percent 999999 5)))
; 10.92489865490932
(center (par2 (make-center-percent 10000 1) (make-center-percent 999999 5)))
; 9900.834323352818
(percent (par2 (make-center-percent 10000 1) (make-center-percent 999999 5)))
; 1.0396983417344345

; Ex. 2.15
; Eva is right. An interval represents the uncertainty of the actual value,
; and any arithmetic operations only increase the uncertainty of the resulting value.
; Since par2 has two intervals while par1 has four, par2 has less uncertainty.

; Ex. 2.16
; In our interval system, some properties of mathematic operations don't hold,
; and therefore some algebraric expressions that we think equivalent
; aren't necessarily equivalent.
;
; For example, the distributed property does not hold in our interval system.
(mul-interval (add-interval (make-interval -5 -1) (make-interval 8 12))
              (make-interval 5 25))
; (15 . 275)
(add-interval (mul-interval (make-interval -5 -1) (make-interval 15 25))
              (mul-interval (make-interval 8 12) (make-interval 15 25)))
; (-5 . 285)


; 2.2  Hierarchical Data and the Closure Property

; Ex. 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; Ex. 2.18
(define (reverse l)
  (define (rev l r)
    (if (null? l)
        r
        (rev (cdr l) (cons (car l) r))))
  (rev l ()))

; Ex. 2.19
(define (cc amount coin-values)
  (define first-denomination car)
  (define except-first-denomination cdr)
  (define no-more? null?)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)

; The order of the list does not matter since the function tests
; all possible combinations anyway.

; Ex. 2.20
(define (filter f l)
  (cond ((null? l)
         ())
        ((f (car l))
         (cons (car l) (filter f (cdr l) )))
        (else
         (filter f (cdr l) ))))
(define (same-parity x . l)
  (cons x
        (filter (lambda (e)
                  (= (remainder e 2)
                     (remainder x 2)))
                l)))

; Ex. 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (square-list items)
  (map square items))

; Ex. 2.22
; The first procedure the answer list in the reverse order
; because in each iteration it makes a pair whose first element is
; the intermediate answer list and the second element is
; the car of the list squared.
;
; The second procedure does not work either
; since in each iteration it constructs a pair by prepending
; the intermediate answer list to the car of the list squared.
; The resulting pair's first element is a list, not an integer.

; Ex. 2.23
(define (for-each f l)
  (if (not (null? l))
      ((lambda()
        (f (car l))
        (for-each f (cdr l))))))

; Ex. 2.24
 (list 1 (list 2 (list 3 4)))
; (1 (2 (3 4)))

; Ex. 2.25
(let ((l
       (list 1 3 (list 5 7) 9)))
  (car (cdr (car (cdr (cdr l))))))
(let ((l
       (list (list 7))))
  (car (car l)))
(let ((l
       (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l)))))))))))))

; Ex. 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; (1 2 3 4 5 6)
(cons x y)   ; ((1 2 3) 4 5 6)
(list x y)   ; ((1 2 3) (4 5 6))

; Ex. 2.27
(define (deep-reverse x)
  (if (pair? x)
      (reverse (map deep-reverse x))
      x))
(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)

; Ex. 2.28
(define (fringe x)
  (cond ((null? x)
         ())
        ((pair? x)
         (append (fringe (car x)) (fringe (cdr x))))
        (else
         (list x))))
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

; Ex. 2.29
; a.
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define left-branch car)
(define (right-branch mobile)
  (car (cdr mobile)))
(define branch-length car)
(define (branch-structure branch)
  (car (cdr branch)))
; b.
(define (mobile? structure)
  (pair? structure))
(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (mobile? s)
        (total-weight s)
        s)))
(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))
; c.
(define (balanced? m)
  (let ((left (left-branch m))
        (right (right-branch m)))
    (and (= (branch-torque left)
            (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))
(define (branch-torque b)
  (* (branch-length b)
     (branch-weight b)))
(define (branch-wieght b)
  (let ((s (branch-structure b)))
    (if (mobile? s)
        (total-weight s)
        s)))
(define (branch-balanced? b)
  (let ((s (branch-structure b)))
    (if (mobile? s)
        (balanced? s)
        #t)))
; d
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define right-branch cdr)
(define branch-structure cdr)

;;;

(define m (make-mobile
 (make-branch 5 10)
 (make-branch 3 (make-mobile
                 (make-branch 7 14)
                 (make-branch 8 88)))))
(branch-structure (left-branch (branch-structure (right-branch m))))
(total-weight m)

(define m (make-mobile
 (make-branch 3 (make-mobile
                 (make-branch 6 6)
                 (make-branch 4 9)))
 (make-branch 5 (make-mobile
                 (make-branch 1 8)
                 (make-branch 8 1)))))
(balanced? m)

; Ex. 2.30
(define (square-tree tree)
  (cond ((null? tree)
         ())
        ((pair? tree)
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))
        (else
         (square tree))))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; Ex. 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (square-tree tree) (tree-map square tree))

; Ex. 2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset))
                          rest)))))
; Let S(0) be an empty set and S(n) be the set of all subsets of the set with
; first to n-th elements, E(1) to E(n). S(n) is the union of S(n-1)
; and a set of all subsets in S(n-1) with E(n) added to each subset.

; Ex. 2.33
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; Ex. 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* higher-terms x)
                   ))
              0
              coefficient-sequence))

; Ex. 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (st)
                     (cond ((null? st) 0)
                           ((pair? st) (count-leaves st))
                           (else 1)))
                   t)))

; Ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex. 2.37
; This uses built-in map procedure.
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v))  m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
           m)))

(matrix-*-matrix (list (list 2 3) (list 1 4) (list 2 1)) (list (list 3 1 2) (list 2 4 2)))

; Ex. 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; The operation must be both associative and commutative to guarantee that
; fold-right and fold-left produce the same values for any sequence.
;
; (fold-right op initial (list e1 e2 e3))
; = (op e1 (op e2 (op e3 initial)))
; = (op e1 (op (op e2 e3) initial)) ; by associative property
; = (op e1 (op initial (op e2 e3))) ; by commutative property
; = (op (op e1 initial) (op e2 e3)) ; by associative property
; = (op (op initial e1) (op e2 e3)) ; by commutative property
; = (op (op (op initial e1) e2) e3) ; by associative property
; (fold-left op initial (list e1 e2 e3))

; commutative but not associative operator
(define (commutative-but-not-associative a b)
  (- (* a b) (+ a b)))
(commutative-but-not-associative 1 2)
(commutative-but-not-associative 2 1)
(commutative-but-not-associative (commutative-but-not-associative 1 2) 5)
(commutative-but-not-associative 1 (commutative-but-not-associative 2 5)
(fold-right commutative-but-not-associative 0 (list 1 2 3))
(fold-left commutative-but-not-associative 0 (list 1 2 3))

; associative but not commutative operator
(define m (list (list 1 2) (list 3 4)))
(define n (list (list 5 6) (list 7 8)))
(matrix-*-matrix m n)
(matrix-*-matrix n m)
(matrix-*-matrix (matrix-*-matrix m n) m)
(matrix-*-matrix m (matrix-*-matrix n m))
(fold-right matrix-*-matrix (list (list 10 9) (list 8 7)) (list m n m))
(fold-left matrix-*-matrix (list (list 10 9) (list 8 7)) (list m n m))

; Ex. 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

; Ex. 2.40
(define (enumerate-interval l h)
  (if (> l h)
      ()
      (cons l (enumerate-interval (+ l 1) h))))

(define (flatmap proc seq)
  (accumulate append
              ()
              (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (smallest-divisor n)
  (define (divides? a b) (= (remainder a b) 0))
  (define (square n) (* n n))
  (define (find-divisor n t)
    (cond ((> (square t) n) n)
          ((divides? n t) t)
          (else (find-divisor n (+ t 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))

; Ex. 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
             (flat-map (lambda (j)
                         (map (lambda (k)
                                (list i j k))
                              (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum l)
  (accumulate + 0 l))

(define (triples-sum-to s n)
  (filter (lambda (t)
            (= (sum t) s))
          (unique-triples n)))

; Ex. 2.42
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval l h)
  (if (> l h)
      ()
      (cons l (enumerate-interval (+ l 1) h))))
(define (flatmap proc seq)
  (accumulate append
              ()
              (map proc seq)))
(define (any? proc items)
  (cond ((null? items) #f)
        ((proc (car items)) #t)
        (else (any? proc (cdr items)))))
(define empty-board ())
(define (make-position row col)
  (cons row col))
(define row car)
(define col cdr)
(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))
; NOTE: safe? does not use k since the current implementation of queens
; prepends the current (k-th) queen's position to the list of positions of other queens.
(define (safe? k positions)
  (let ((r (row (car positions)))
        (c (col (car positions))))
    (not (any? (lambda (position)
                 (let ((r2 (row position))
                       (c2 (col position)))
                   (or (= r r2)
                       (= c c2)
                       (= (abs (- r r2))
                          (abs (- c c2))))))
               (cdr positions)))))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (length (queens 8)))
(display (car (queens 8)))
