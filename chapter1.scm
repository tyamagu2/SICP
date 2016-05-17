; Ex. 1.3
(define (sum-square x y z)
  (+ (* x x) (* y y) (* z z)))

; Ex. 1.5
; If the interpreter uses appicative-order evaluation,
; (test 0 (p)) falls into an infinite loop.
; If, on the other hand, the interpreter uses normal order evaluation,
; the given expression evaluates to 0.

; Ex. 1.6
; It falls into an infiite loop.
; The interpreter must evaluate the three arguments
; before it can evaluate new-if's body.
; To evaluate the last argument of the new-if, however,
; it needs to evaluate another sqrt-iter,
; leading to the recursive evaluation of new-if and sqrt-iter that never ends..

; Ex. 1.7
; When x given to sqrt is too small compared to the threshold value used in good-enough?,
; the effect x has on the evaluated value of (abs (- (square guess) x)) is so small
; that you can ignore x and (square guess) becomes the sole factor to determine
; either to continue or to stop the procedure.
; When x is very small and the threshold value in good-enough? is 0.001,
; sqrt x converges to around 0.031, which is a square root of the threhsold.
;
; When x is too large, calculating the guess squared causes buffer overflow.

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? new-guess old-guess)
  (< (abs (- 1.0 (/ old-guess new-guess))) 0.001))

; Ex. 1.8
(define (cbrt x)
  (define (good-enough? guess old-guess)
    (< (abs (- 1.0 (/ old-guess guess))) 0.001))
  (define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess old-guess x)
    (if (good-enough? guess old-guess)
        guess
        (cbrt-iter (improve guess x) guess x)))
  (cbrt-iter 1.0 x x))


;; 1.2 Procedures and the Processes They Generate

(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (factorial-iter v c n)
    (if (> c n)
        v
        (factorial-iter (* v c) (+ c 1) n)))
  (factorial-iter 1 1 n))

; Ex. 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
; (+ 4 5)
; (inc (+ 3 5)
; (inc (inc (+ 2 5)))
; (inc (inc (inc + 1 5)))
; (inc (inc (inc (inc + 0 5))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9

; Ex. 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
; f(n) = 2n

(define (g n) (A 1 n))
; g(n) = A(1, n)
;      = A(0, A(1, n-1))
;      = A(0, A(0, A(1, n-2))
;      = ...
;      = A(0, A(0, A(0, ... A(0, A(0, 1)))))
;      = A(0, A(0, A(0, ... A(0, 2))))
;      = 2 ^ n
; g(0) = 0

(define (h n) (A 2 n))
; h(n) = A(2, n)
;      = A(1, A(2, n-1))
;      = A(1, A(1, A(2, n-2)))
;      = A(1, A(1, ... A(1, A(1, 1))))
;      = g(g(...g(g(1))))
;      = 2 ^ 2 ^ 2 ^ ,,,
;      = 2 ^ h(n - 1)
; h(0) = 0

; Counting change
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (cc amount 5))

; Ex. 1.11
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (f-recursive (- n 2))
         (f-recursive (- n 3)))))

(define (f-iterative n)
  (define (f-iter e1 e2 e3 c)
    (if (= c 0)
        e3
        (f-iter (+ e1 e2 e3) e1 e2 (- c 1))))
  (f-iter 2 1 0 n))

; Ex. 1.12
(define (pascal-elem d i)
  (if (or (= i 0) (= i (- d 1)))
      1
      (+ (pascal-elem (- d 1) (- i 1))
         (pascal-elem (- d 1) i))))

; Ex. 1.13
; It's trivial to prove that
;
;     Fib(n) = (PHI ^ n - PSI ^ n) / sqrt(5)
;
; by using mathematical induciton.
;
; To prove that Fib(n) is the closest integer to PHI ^ n / sqrt(5),
; We prove that
;
;     |PHI ^ n / sqrt(5) - Fib(n)| = |PSI ^ n / sqrt(5)| <= 1/2
;     => |PSI ^ n| <= sqrt(5) / 2
;
; Since 2 < sqrt(5) < 3, -1 < PSI < -1/2.
; Thus, |PSI ^ n| < 1, and therefore
; |PSI ^ n| < 1 < sqrt(5) / 2
;
; We have proved that |PHI ^ n / sqrt(5) - Fib(n)| < 1/2,
; and thus that F(n) is the closest integer to PHI ^ n / sqrt(5).
; Q.E.D.

; Ex. 1.14
; (11, 5)
;   (11, 4)
;     (11, 3)
;       (11, 2)
;         (11, 1)
;           (11, 0)
;           (10, 1)
;             (10, 0)
;             (9, 1)
;               (9, 0)
;               (8, 1)
;                 ...
;                 (1, 0)
;                 (0, 1)
;         (6, 2)
;       (1, 3)
;         (1, 2)
;           (1, 1)
;             (1, 0)
;             (0, 1)
;           (-4, 2)
;         (-9, 3)
;     (-14, 4)
;   (-39, 5)
;
; Space: n
; Steps: 2 ^ n

; Ex. 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) ( * 4 (cube x))))
(define (sine angle)
  (if (> (abs angle) 0.1)
      (p (sine (/ angle 3.0)))
      angle))
; a. 5
; b. space: log(a)
;    steps: log(a)

; Ex. 1.16
(define (fast-expt b n)
  (define (square n) (* n n))
  (define (even? n) (= (remainder n 2) 0))
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

; Ex. 1.17
(define (mul-rec a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (mul-rec (double a) (halve b)))
        (else (+ a (mul-rec a (- b 1))))))

; Ex. 1.18
(define (mul a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (iter a b r)
    (cond ((= b 0) r)
          ((even? b) (iter (double a) (halve b) r))
          (else (iter a (- b 1) (+ r a)))))
  (iter a b 0))

; Ex. 1.19
; p' = p ^ 2 + q ^ 2
; q' = 2pq + q ^ 2
(define (fast-fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

; Ex. 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal order
; (gcd 206 40)
;   (gcd 40 (remainder 206 40))
;     (if (= (remainder 206 40) 0)) ;; 1
;     (gcd (remainder 206 40) (remainder 40 (remainder (206 40))))
;       (if (= (remainder 40 (remainder 206 40)))) ;; 3
;       (gcd (remainder 40 (remainder 206 40)) (remainder (remainder...
; ...
; SUM(i from 1 to n, fib(i) + fib(i - 1)) - 1
;
; Applicative order
; (gcd 206 40)
;   (gcd 40 (remainder 206 40))
;   (gcd 40 6)
;     (gcd 6 (remainder 40 6))
;     (gcd 6 4)
;       (gcd 4 (remainder 6 4))
;       (gcd 4 2)
;         (gcd 2 (remainder 4 2))
;         (gcd 2 0)
; 4

; Ex. 1.21
(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

; Ex. 1.22
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

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (define (iter cur to)
    (if (<= cur to)
        (timed-prime-test cur))
    (if (<= cur to)
        (iter (+ cur 2) to)))
  (iter (if (even? from)
            (+ from 1)
            from)
        to))

; 1009 *** 4
; 1013 *** 5
; 1019 *** 5
;
; 10007 *** 14
; 10009 *** 14
; 10037 *** 14
;
; 100003 *** 43
; 100019 *** 43
; 100043 *** 42
;
; 1000003 *** 134
; 1000033 *** 133
; 1000037 *** 133

; 1000000000039 *** 81853
; 1000000000061 *** 78162
; 1000000000063 *** 78002

; Ex. 1.23
(define (smallest-divisor n)
  (define (divides? a b) (= (remainder a b) 0))
  (define (square n) (* n n))
  (define (next t)
    (if (= t 2)
        3
        (+ t 2)))
  (define (find-divisor n t)
    (cond ((> (square t) n) n)
          ((divides? n t) t)
          (else (find-divisor n (next t)))))
  (find-divisor n 2))

; 1000003 *** 73
; 1000033 *** 72
; 1000037 *** 73

; 1000000000039 *** 41578
; 1000000000061 *** 52821
; 1000000000063 *** 46891

; Ex. 1.24
(define (expmod base exp m)
  (define (square n) (* n n))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(use srfi-27)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 100))

; 1000003 *** 962
; 1000033 *** 626
; 1000037 *** 642

; 1000000000039 *** 1992
; 1000000000061 *** 1957
; 1000000000063 *** 1983

; Ex. 1.25
(define (poor-expmod base exp m)
  (remainder (fast-expt base exp) m))

; Modified version of expmod produces gigantic intermediate numbers,
; causing the calculation to be significantly slower.
; The modified version could also cause buffer overflow in other languages.

; Ex. 1.26
; The modified version of expmod calls expmod twice when exp is even.
; In other words, the modified version generates tree-recursive process,
; and its execution time grows exponentially.
; Since the depth of the process tree grows logarithmically,
; the execution time grows linearly;
; i.e. 2 ^ log(n) = n

; Ex. 1.27
(define (full-fermat-test n)
  (define (fermat-test a)
    (= (expmod a n n) a))
  (define (iter a)
    (cond ((= a n) #t)
          ((fermat-test a)
           (iter (+ a 1)))
          (else #f)))
  (iter 1))

; Carmichael numbers
(full-fermat-test 561)  ; #t
(full-fermat-test 1105) ; #t
(full-fermat-test 1729) ; #t
(full-fermat-test 2465) ; #t
(full-fermat-test 2821) ; #t
(full-fermat-test 6601) ; #t

; Non-Carmichael numbers
(full-fermat-test (*561 561)) ; #f

; Ex. 1.28

(define (expmod base exp m)
  (define (square n) (* n n))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(use srfi-27)

(define (miller-rabin-test n)
  (define (expmod base exp m)
    (define (square n) (* n n))
    (define (even? n) (= (remainder n 2) 0))
    (define (square-mod-check x m)
      (define (non-trivial-check x xx)
        (if (and (= xx 1)
                 (not (= x 1))
                 (not (= x (- m 1))))
            0
            xx))
      (non-trivial-check x (remainder (square x) m)))
    (cond ((= exp 0) 1)
          ((even? exp)
           (square-mod-check (expmod base (/ exp 2) m) m))
          (else
           (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (congruent? x)
    (or (= x 0) (= x 1)))
  (not (congruent? (expmod (+ 1 (random-integer (- n 1))) (- n 1) n))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
         ((miller-rabin-test n) (miller-rabin-fast-prime? n (- times 1)))
         (else #f)))

; Carmichael numbers
(miller-rabin-prime? 561  100) ; #f
(miller-rabin-prime? 1105 100) ; #f
(miller-rabin-prime? 1729 100) ; #f
(miller-rabin-prime? 2465 100) ; #f
(miller-rabin-prime? 2821 100) ; #f
(miller-rabin-prime? 6601 100) ; #f

; Non-Carmichael numbers
(miller-rabin-prime? (* 561 561) 100) ; #t
