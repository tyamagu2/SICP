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
