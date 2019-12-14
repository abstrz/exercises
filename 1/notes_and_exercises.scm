;=============Common and simple procedures====================
(define (inc x)
    (+ x 1))
(define (dec x)
    (- x 1))
(define (identity x)
  x)
(define (cube x) (* x x x))
(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))
(define (logB B x)
  (/ (log x) (log B)))
(define (gen-gcd . args)
  (define (rec l)
    (cond ((= (length l) 1) (car l)) 
          ((= (length l) 2) (gcd (car l) (cadr l)))
          ((> (length l) 2) (gcd (gcd (car l) (cadr l)) (rec (cddr l))))))
  (if (< (length args) 2) 
      (error "You need to provide at least two arguments. You gave:" (length args))
      (rec args)))
(define (copies-list element size)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (cons element result))))
  (iter size '()))
;=============1.2.4 Exponentiation=============================
;;recursive definition
;(define (expt_rec b n)
;  (if (= n 0)
;      1
;      (*  b (expt_rec b (- n 1)))))
;;iterative definition
;(define (expt_it b n)
;  (define (expt-iter b counter product)
;    (if (= counter 0)
;product
;(expt-iter b
;   (- counter 1)
;   (* b product))))
;  (expt-iter b n 1))
;;faster recursive definition
(define (fast-exp b n)
  (define (even? n)
    (= (remainder n 2) 0 ))
  (cond ((= n 0) 1)
((even? n) (square (fast-exp b (/ n 2))))
(else (* b (fast-exp b (- n 1))))))
;(define (e b n)
;(define (e-iter b n a)
;(cond ((= n 0) a)
;      ((even? n) (e-iter (square b) (/ n 2) a))
;      (else (e-iter b (- n 1) (* b a)))))
;
;(e-iter b n 1))
;(define (fast-mult a b)
;(cond((= b 0) 0)
;((even? b) (* 2 (fast-mult a (/ b 2))))
;(else (+ a (fast-mult a (- b 1))))))
;;Want a*(b*n) to stay constant in each iteration.  
;(define (*-it b n)
;  (define (fmi a b n)  
;  (cond ((= n 0) a)
;((even? n) (fmi a (* 2 b) (/ n 2)))
;(else (fmi (+ a b) b (- n 1)))))
;  (fmi 0 b n))
;
;;(define (fib n)
;;  (fib-iter 1 0 0 1 n))
;;(define (fib-iter a b p q count)
;;  (cont ((= count 0) b)
;;((even? count) 
;; (fib-iter a 
;;   b 
;;   (+ (square p) (square q)) 
;;   (* q (+ (* 2 p) q)) 
;;   (/ count 2)))
;;(else (fib-iter (+ (* b q) (* a q) (* a p)) 
;;(+ (* b p) (* a q)) 
;;p 
;;q 
;;(- count 1)))))
;
;1.2.6===================Example: Testing for Primality ==============================
(define (smallest-divisor n) 
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor)
(else (find-divisor n (inc test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
;
;; \Theta(\sqrt{n}) order of growth.
(define (prime? n)
  (= n (smallest-divisor n)))
;
;; \Theta(log n)
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;((even? exp)
; (remainder
;   (square (expmod base (/ exp 2) m))
;   m))
;(else
;  (remainder
;    (* base (expmod base (- exp 1) m))
;    m))))
;(define (fermat-test n)
;  (define (try-it a)
;    (= (expmod a n n) a))
;  (try-it (+ 1 (random (- n 1)))))
;(define (fast-prime? n times)
;  (cond ((= times 0) true)
;((fermat-test n) (fast-prime? n (- times 1)))
;(else false)))
;
;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (if (fast-prime? n)
;      (report-prime (- (runtime) start-time))))
;;prints elapsed-time
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))
;
;;check for first three odd primes in [a,b]
;(define (iterator a b i)
;  (cond ((<= i b)
;  (if (and (= (remainder i 2) 1) (prime? i))
;    (timed-prime-test i))
;  (iterator a b (+ i 1)))
;(else 
;  (newline)
;  (display "DONE!"))))
;(define (search-for-primes a b)
;  (iterator a b (ceiling a)))
;
;(define (next n)
;  (if (= n 2)
;      3
;1.3===================Formuluating Abstractions with Higher-Order Procedures========================
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a) (sum term (next a) next b))))
;Not sure of the name of the rule used here, but a crude approximation none the less. 
(define (integral_1 f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;Exercise 1.29
(define (integral_2 f a b n)
  (define (add-h x) 
    (+ x (/ (- b a ) n)))
  (define (f-prod x)
    ;if the x-a/h is 0 or n, then we have the extremal indices, and we pass back the function.
    (cond ((or (= (/ (- x a) (/ (- b a ) n)) 0) (= (/ (- x a) (/ (- b a ) n)) n)) 
   (f x))
    ;if x-a/h is even, then we return the function times 2. If we are at the last endpoint,
    ;then we would have already hit the first condition, so that this second condition would
    ;never be hit, so we get the behavior we want.
  ((= (remainder (/ (- x a) (/ (- b a ) n)) 2) 0)
   (* 2 (f x)))
    ;if x-a/h is odd, then we return the function times 4...
          ((= (remainder (/ (- x a) (/ (- b a ) n)) 2) 1)
   (* 4 (f x)))))
  (* (/ (/ (- b a ) n) 3.0) (sum f-prod a add-h b)))
;exercise 1.30
;define (sum-it term a next b)
;  (define (iter a result)
;    (if (> a b)
;result
;(iter (next a) (+ result (term a)))))
;  (iter a 0))
;Exercise 1.31.a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (factorial n)
  (define (inc x)
    (+ x 1))
  (define (identity x)
    x)
  (product identity 1 inc n))
;this is an infinite product that sums to pi/4
(define (pi/4-approx n)
  (define (inc x)
    (+ x 1))
  (define (f x)
    (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))
  (/ (product-it f 1 inc n) 2.0))
(define (pi-approx n)
  (* 4 (pi/4-approx n)))
;Exercise 1.31.b:
(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
result
(iter (next a) (* result (term a)))))
  (iter a 1))
;Exercise 1.32.a:
(define (accumulate combiner null-value term a next b )
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))
;Exercise 1.32.b:
(define (accumulate_it combiner null-value term a next b)
  (define (iter a result)
    (if (a>b)
result
(iter (next a) (combiner result (term a))))
  (iter a null-value)))
;Exercise 1.33.a
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) 
 null-value)
        ((filter a) 
         (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
(else 
  (filtered-accumulate filter combiner null-value term (next a) next b))))
(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))
;Euclid's Algorithm for GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;Returns a procedure which mathematically is just f(x)=gcd(x,b).
(define (relatively-prime? b)
  (define (gcd_1_arg x)
    (= (gcd x b) 1))
  gcd_1_arg)
;Exercise 1.33.b
(define (product-relatively-prime a b)
  (filtered-accumulate (relatively-prime? b) * 1 identity a inc b))
;1.3.2============Constructing Procedures Using lambda=====================
;===============lambda syntax================ 
;((lambda (<var_1> \dots <var_n>)
;    <body>)
;  <exp_1>
;  /dots
;  <exp_n>)
;============================================
;===============let syntax=================== 
;(let ((<var_1> <exp_1>)
;      (<var_2> <exp-2>)
;      \dots
;      (<var_n> <exp_n>))
;    <body>)
;============================================
(define (f x y) 
  ((lambda (a b)
     (+ (* x (square a))
(* y b)
(* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(define (f x y)
  (let ((a (+ 1 (* x y)))
(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;Procedure that finds the root of a function
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
midpoint
(let ((test-value (f midpoint)))
  (cond ((positive? test-value) 
(search f neg-point midpoint))
((negative? test-value)
 (search f midpoint pos-point))
(else midpoint))))))
;Extend functionality of search to check the signs of the function at the endpoints, and proceed accordingly. 
(define (half-interval-method f a b)
  (let ((a-value (f a))
(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
   (search f a b))
  ((and (negative? b-value) (positive? a-value))
   (search f b a))
   (else 
     (error "Values are not of the opposite sign" a b)))))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    ;modifier so that the next guess is the average of the previous one and (f guess).
    (let ((next (f guess)))
      (if (close-enough? guess next)
  next
  (try next))))
  (try first-guess))
;Exercise 1.35:
(define (golden_ratio n)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) n))
;Exercise 1.36;
(define (approx_x^x n)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) n))
;As per the requirements of this exercise, I modified
;the fixed-point approximation to use average damping,
;i.e., to take as the next guess an average of the function
;value at the guess, and the guess;
;, and the performance was much improved.
;Exercise 1.37.a
(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n 1) (+ (d 1) (cont-frac (lambda (i) (n (inc i))) (lambda (i) (d (inc i))) (dec k))))))
;Exercise 1.37.b
(define (cont-frac_it n d k)
  (define (iter i result)
    (if (= i 1)
result
(iter (dec i) (/ (n (dec i)) (+ (d (dec i)) result)))))
  (iter k (/ (n k) (d k))))
;Exercise 1.38
(define (e-cf k)
  (define (d i)
    (cond ((= i 2) 2.0)
  ((= (remainder i 3) 2) (* (/ 2 3) (+ i 1.0)))
  (else 1)))
  (cont-frac (lambda (i) 1.0) d k))
;Exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1) 
x
(- (square x))))
  (define (d i)
    (- (* 2.0 i) 1.0))
  (cont-frac n d k))
;=============1.3.4 Procedures as Returned Values ==============
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
;Exercise 1.40:
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
;Exercise 1.41:
(define (double f) 
  (lambda (x) (f (f x))))
;Exercise 1.42:
(define (compose f g)
  (lambda (x) (f (g x))))
;Exercise 1.43:
(define (repeated f n)
  (if (= n 0)
      identity 
      (compose f (repeated f (- n 1)))))
;Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (repeated-smooth f n)
  (repeated (smooth f) n))
;Exercise 1.45
(define (repeated-average-damp f n)
  (repeated (average-damp f) n))
(define (average-damped-root n y)
  (fixed-point 
    (repeated-average-damp 
      (lambda (x) (/ y (expt x (- n 1)))) 
      (ceiling (/ (log n) (log 2)))) 
    1.0))
;Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (f guess) 
    (if (good-enough? guess)
guess
(f (improve guess))))
  f)
(define (sqrt-custom n) 
  ((iterative-improve 
     (lambda (guess) (< (abs (- (square guess) n)) 0.000001)) 
     (lambda (guess) (average guess (/ n guess))))
   1.0))
(define (fixed-point-2 f) 
  ((iterative-improve
     (lambda (guess) (< (abs (- guess (f guess))) 0.000001))
     (lambda (guess) (f guess))
     )
   2.0))
;=============2. Building Abstractions with Data ==============
;==2.1.1 Example: Arithmetic Operations for Rational Numbers==
;(define (add-rat x y)
;  (make-rat (+ (* (numer x) (denom y))
;               (* (numer y) (denom x)))
;            (* (denom x) (denom y))))
;(define (sub-rat x y)
;  (make-rat (- (* (numer x) (denom y))
;               (* (numer y) (denom x)))
;            (* (denom x) (denom y))))
;(define (mult-rat x y)
;  (make-rat (* (numer x) (numer y))
;            (* (denom x) (denom y))))
; (define (div-rat x y)
;  (make-rat (* (numer x) (denom y))
;            (* (denom x) (numer y))))              
;(define (numer x) (car x))
;(define (denom x) (cdr x))
;(define (print-rat x)
;  (newline)
;  (display (numer x))
;  (display "/")
;  (display (denom x)))
;;Exercise 2.1
;(define (make-rat n d) 
;  (let ((g (gcd n d)))
;    (if (not (= d 0))
;        (if (>= (* n d) 0)
;            (cons (abs (/ n g)) (abs (/ d g)))
;            (cons (- (abs (/ n g))) (abs (/ d g))))
;        (error "Denominator cannot be zero!"))))
;======= 2.1.2 Abstraction Barriers =======
;Exercise 2.2
(define (make-segment p q)
  (cons p q))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (make-point x y)
  (cons x y))
(define (x-point z)
  (car z))
(define (y-point z)
  (cdr z))
(define (midpoint-segment l)
  (make-point
   (average (x-point (start-segment l)) (x-point (end-segment l)))
   (average (y-point (start-segment l)) (y-point (end-segment l)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
;Exercise 2.3
;pass in two numbers that represent scalars in standard basis directions
(define (rec1 a b)
  (cons (make-segment (make-point (- a) 0) (make-point a 0)) (make-segment (make-point 0 (- b)) (make-point 0 b))))
;p is a point
(define (rec2 p)
  (cons (make-segment (make-point 0 0) (make-point (x-point p) 0)) 
        (make-segment (make-point 0 0) (make-point 0 (y-point p)))))
(define (base rectangle)
  (car rectangle))
(define (height rectangle)
  (cdr rectangle))
(define (magnitude-segment l)
  (sqrt (+ (square (- (y-point (end-segment l)) (y-point (start-segment l)))) 
           (square (- (x-point (end-segment l)) (x-point (start-segment l)))))))
;rec-area and rec-per take as arguments b h, which are the lengths of the base and height, respectively. 
(define (rec-area rectangle)
  (* (magnitude-segment (base rectangle)) (magnitude-segment (height rectangle))))
(define (rec-per rectangle)
  (* 2 (+ (magnitude-segment (base rectangle)) (magnitude-segment (height rectangle)))))
;Exercise 2.4:
(define (cons_alt x y)
  (lambda (m) (m x y)))
(define (car_alt z)
  (z (lambda (p q) p)))
(define (cdr_alt z)
  (z (lambda (p q) q)))
;Exercise 2.5:
(define (cons_nonneg a b)
  (* (expt 2 a) (expt 3 b)))
(define (car_nonneg n)
  (define (iter k n)
    (if (not (= (remainder n 2) 0)) k
        (iter (+ k 1) (/ n 2))))
  (iter 0 n))
(define (cdr_nonneg n)
  (define (iter k n)
    (if (not (= (remainder n 3) 0)) k
        (iter (+ k 1) (/ n 3))))
  (iter 0 n))
;Exercise 2.6:
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-church n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
;2.1.4. Extended Exercise: Interval Arithmetic 
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y) 
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0)) 
           (make-interval (* a c) (* b d)))
          ((and (>= a 0) (>= b 0) (< c 0) (>= d 0)) 
           (make-interval (* b c) (* c d)))
          ((and (< a 0) (>= b 0) (>= c 0) (>= d 0)) 
           (make-interval (* a d) (* b d)))
          ((and (>= a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a d)))
          ((and (< a 0) (< b 0) (>= c 0) (>= d 0))
           (make-interval (* a d) (* b c)))
          ((and (< a 0) (>= b 0) (< c 0) (>= d 0))
           (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))
          ((and (< a 0) (< b 0) (< c 0) (>= d 0))
           (make-interval (* a d) (* a c)))
          ((and (< a 0) (< b 0) (< c 0) (>= d 0))
           (make-interval (* a d) (* a c)))
          ((and (< a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a c)))
          ((and (< a 0) (< b 0) (< c 0) (< d 0))
           (make-interval (* b d) (* a c))))))
(define (div-interval x y)
  (if (and (not (= (upper-bound y) 0)) (not (= (lower-bound y) 0))) 
      (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))
      (error "Second parameter to div-interval  must be an interval with non zero end points!")))
;Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound I) (cdr I))
(define (lower-bound I) (car I))
;Exercise 2.8
(define (subtract-intervals x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
(define (width I)
  (/ (- (upper-bound I) (lower-bound I)) 2))
;Exercise 2.9
;Done on paper. You get that width(I +/- J) = width(I) +/- width(J)
;Exercise 2.10
;to modify div-interval to check if dividing by zero, which I've done.
;Exercise 2.11
;edits done made to mul-interval to check signs of endpoints.
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center I)
  (/ (+ (lower-bound I) (upper-bound I)) 2))
;Exercise 2.12
(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))
(define (percent I)
  (if (not (= (center I) 0))
      (/ (width I) (center I))
      (error "Interval must have non-zero center!")))
;Exercise 2.13
;One can show that p(IJ) \approx p(I) + p(J) 
;this insight comes from writing I= (c(I)-w(I), c(I)+w(I)), J=(c(J)-w(J), c(J)+w(J)), and noting that if
;p(I),p(J) are small, then necessarily w(I),w(J) are small. The result follows immediately. 
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))
;Exercise 2.14
;It turns out that if we have I,J with p(I), p(J) \approx 0, p(I/J) \approx p(IJ) \approx p(I) + p(J)
;Exercise 2.15
;I think yes, because variables that represent an uncertain number each add together to increase the uncertainty
;of the parallel resistance. This is why r1r2/r1+r2 is worse. We get p(r1r2/r1+r2)= p(r1)+p(r2)+p(r1+r2),
;under the assumption that p(r1),p(r2),p(r1+r2) are small. On the other hand, if we assume p(1/r1+1/r2) is small,
;we get that p(1/(1/r1 + 1/r2)) = p(1/r1+1/r2). 
;========2.2 Hierarchical Data and the Closure Property==========
;Syntax:
;(list <a_1> <a_2> /dots <a_n>) is equivalent to 
;(cons <a_1>  (cons <a_2> (cons \dots (cons <a_n> ()) \dots ))),
;where () is understood to be nil, null, empty.
;(cadr <arg>) = (car (cdr <arg>))
;Let l be (list <a_1> <a_2> \dots <a_n>), then
;(cons <a_0> l) is (list <a_0> <a_1> <a_2> \dots <a_n>)
;The following procedures are already implemented in Scheme, and are
;only being written down for pedagogical reasons:
;(define (list-ref items n)
;   (if (= n 0)
;       (car items)
;       (list-ref (cdr items) (- n 1))))
;(define (length items)
;   (if (null? items)
;   0
;   (+ 1 (length (cdr items)))))
;where null? just checks if its argument is the empty list ().
;We could also compute length using iteration:
;(define (length items)
;   (define (length-iter a count)
;       (if (null? a)
;           count
;           (length-iter (cdr a) (+ 1 count)))))
;(define (append list1 list2)
;   (if (null? list1)
;       list2
;       (cons (car list1) (append (cdr list1) list2))))
;Exercise 2.17:
(define (last-pair l)
  (list (list-ref l (- (length l) 1))))
;Exercise 2.18:
;(define (reverse l)
;  (define (reverser l n)
;    (if (= n (- 1))
;        ()
;        (cons (list-ref l n) (reverser l (- n 1)))))
;  (reverser l (- (length l) 1)))
;Exercise 2.19:
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount (first-denomination
                             coin-values))
                 coin-values)))))
(define (first-denomination coin-values)
  (list-ref coin-values 0))
(define (except-first-denomination coin-values)
  (define (popper l n)
    (if (= n (length l)) 
        ()
        (cons (list-ref l n) (popper l (+ n 1)))))
  (popper coin-values 1))
;There has to be a prettier way to do this, but this works :| .
(define (no-more? l)
  (if (= (length l) 0)
      #t
      #f))
;Does the order of the list matter? No. The counting is the same.
;(define (f x y . z) <body>). This syntax says that f needs at least
;two arguments provided, but can have arbitrarily many arguments over
;two. For example if we call (f 1 2 3 4 5), then we get
;that x=1, y=2 and z=(3 4 5).
;(define (g . w) <body>). Now we can either call g with no arguments,
;or arbitrarily many.
;Exercise 2.20
(define (same-parity x . y)
  (define (parity-checker m n)
    (if (= (remainder m 2) (remainder n 2)) 
        #t
        #f))
  (define (parity-builder l n)
    (cond ((= n (length l)) ())
          ((parity-checker x (list-ref l n)) (cons (list-ref l n) (parity-builder l (+ n 1))))
          (else  (parity-builder l (+ n 1)))))
  (cons x (parity-builder y 0)))
;map syntax
;(define (map proc items)
;   (if (null? items)
;       ()
;       (cons (proc (car items))
;             (map proc (cdr items)))))
;Exercise 2.21:
;(define (square-list items)
;  (if (null? items)
;      ()
;      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))
;Exercise 2.22
;The first method does return the list in reverse order, which one can verify by
;evaluating it on an argument by substitution.
;The reason seems to be (cons (square (car things)) answer).
;You are building it right to left, iteratively. 
;To fix it maybe write (cons answer (square (car things)))
;Apparently this doesnt work either, which is the next
;part of the question. I'm going to evaluate this to see why... 
;In the second case, if you pass to iter (list 1 2 3 4) () you get
;(cons (cons (cons (cons () 1) 4) 9) 16), so you build something that
;is not even a list, anymore. The car of each pair points to the next pair,
;and the cdr of each pair contains the value at that pair, but in reverse order.
;You build all together the wrong thing! 
;You could fix the second method by using append:
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items ()))
;Exercise 2.23:
(define (for-each func items)
  (func (list-ref items 0))
  (newline)
  (if (not (null? (cdr items))) 
      (for-each func (cdr items)) 
      ))
;=============2.2.2 Hierarchical Structures ==============
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;Exercise 2.24:
;done sur un papier, mon ami. 
;Exercise 2.25:
;1.cadaddr, or (car(cdr(car(cdr(cdr *)))))
;2.caar , or (car(car *)))
;3.cadadadadadadr 
;Exercise 2.26:
;(1 2 3 4 5 6)
;( (1 2 3) 4 5 6)
;( (1 2 3) (4 5 6))
;Exercise 2.27:
;I am going to redefine reverse here. I don't like how I wrote it above.
(define (reverse items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (car things) answer))))
  (iter items ()))
(define (deep-reverse items)
  (define (iter things answer)
    (cond ((null? things) answer)
          ((pair? (car things))
           (iter (cdr things) (cons (deep-reverse (car things)) answer)))
          (else (iter (cdr things) (cons (car things) answer)))))
  (iter items ()))
;(define (deep-reverse items)
;  (define (reverser  things index)
;    (cond ((< index 0) ())
;          ((pair? (list-ref things index)) (cons (deep-reverse (list-ref things index)) (reverser things (- index 1))))
;          (else (cons (list-ref things index) (reverser things (- index 1))))))
;  (display (car items))
;  (reverser items (- (length items) 1)))
;Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) ())
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))
;Exercise 2.29
;left and right are branches. 
(define (make-mobile left right)
  (list left right))
;structure can either be a number, which represents a weight, or a mobile.
(define (make-branch len structure)
  (list len structure))
;a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
;b
(define (total-weight mobile)
  (cond ((and (not (pair? (branch-structure (left-branch mobile)))) (not (pair? (branch-structure (right-branch mobile)))))
         (+ (branch-structure (left-branch mobile)) (branch-structure (right-branch mobile))))
        ((and (pair? (branch-structure (left-branch mobile))) (not (pair? (branch-structure (right-branch mobile)))))
         (+ (total-weight (branch-structure (left-branch mobile))) (branch-structure (right-branch mobile))))
        ((and (not (pair? (branch-structure (left-branch mobile)))) (pair? (branch-structure (right-branch mobile))))
         (+ (branch-structure (left-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))
        ((and (pair? (branch-structure (left-branch mobile))) (pair? (branch-structure (left-branch mobile))))
         (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile)))))))
;c
(define (torque branch)
  (if (pair? (branch-structure branch))
      (* (branch-length branch) (total-weight (branch-structure branch)))
      (* (branch-length branch) (branch-structure branch))))
(define (balanced mobile)
  (cond ((and (not (pair? (branch-structure (left-branch mobile)))) (not (pair? (branch-structure (right-branch mobile)))))
         (= (torque (left-branch mobile)) (torque (right-branch mobile))))
        ((and (pair? (branch-structure (left-branch mobile))) (not (pair? (branch-structure (right-branch mobile)))))
         (and (balanced (branch-structure (left-branch mobile))) 
              (= (torque (left-branch mobile)) (torque (right-branch mobile)))))
        ((and (not (pair? (branch-structure (left-branch mobile)))) (pair? (branch-structure (right-branch mobile))))
         (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) 
              (balanced (branch-structure (right-branch mobile)))))
        ((and (pair? (branch-structure (left-branch mobile))) (pair? (branch-structure (left-branch mobile))))
         (and (balanced (branch-structure (left-branch mobile))) (balanced (branch-structure (right-branch mobile)))))))
;d
;Just change the cadr in the right-branch selector and branch-structure selector to cdr, and we are done.
;(define (scale-tree tree factor)
;  (cond ((null? tree) ())
;        ((not (pair? tree)) (* tree factor))
;        (else (cons (scale-tree (car tree) factor)
;                    (scale-tree (cdr tree) factor)))))
;(define (scale-tree tree factor)
;   (map (lambda (sub-tree)
;   (if (pair? sub-tree)
;       (scale-tree sub-tree factor)
;       (* sub-tree factor)))
;       tree))
;Exercise 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
;Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
;Exercise 2.32:
(define (powerset s)
  (if (null? s)
      (list ())
      (let ((rest (powerset (cdr s))))
        (append rest (map (lambda (subset) 
                            (cons (car s) subset)) 
                          rest)))))
;It works because the set of all subsets is simply the set of all subsets of the cdr of the set union the set of the one missing element union any those subsets... 

;=========2.2.3 Sequences as Conventional Interfaces ==========
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;Exercise 2.33:
(define (mapp p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
(define (appendd seq1 seq2)
  (accumulate cons seq2 seq1))
(define (lengthh sequence)
  (accumulate (lambda (x y)
                (+ y 1)) 
              0 sequence))
;Exercise 2.34:
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff  (* higher-terms x)))
              0
              coefficient-sequence))
;Exercise 2.35:
(define (leaves t)
  (cond ((null? t) ())
        ((pair? (car t)) (append (leaves (car t)) (leaves (cdr t))))
        (else (append (list (car t)) (leaves (cdr t))))))
(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ y 1))
              0 (map identity (leaves t))))
;Exercise 2.36
;makes a list containing the first element of each sequence.
(define (first-element seqs)
  (if (null? seqs)
      ()
      (cons (caar seqs) (first-element (cdr seqs)))))
;makes a list containing sequences that are all but the first element of the original sequences.
(define (next-seqs seqs)
  (if (null? seqs)
      ()
      (cons (cdar seqs) (next-seqs (cdr seqs)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (first-element seqs))
            (accumulate-n op init (next-seqs seqs)))))
;Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product w v))
       m))
(define (transpose m)
  (accumulate-n cons () m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;Exercise 2.38
;op should be commutative.
;Exercise 2.39
(define (reverse_1 sequence)
  (accumulate (lambda (x y) (append y (list x))) () sequence))
(define (reverse_2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))
;Nested Mappings
;The flatmap procedure first maps each element using proc, then flattens the result
;into a new list. 
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?  (flatmap 
                             (lambda (i)
                               (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                             (enumerate-interval 1 n)))))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
;Exercise 2.40:
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?  (unique-pairs n))))
;Exercise 2.41:
(define (equal-to-n? i j k n)
  (= (+ i j k) n))
(define (in-list? x l)
  (not (= (length (remove x l)) (length l))))
(define (crude-triple n)
  (flatmap (lambda (i)
             (map (lambda (l) 
                    (if (in-list? i l)
                        () 
                        (append (list i) l)))
                  (unique-pairs n)))
           (enumerate-interval 1 n)))
(define (refined-triple n)
  (filter (lambda (x) (not (= (length x) 0))) (crude-triple n)))
(define (triple-sum n s)
  (filter (lambda (l) (= (+ (car l) (cadr l) (caddr l)) s)) (refined-triple n)))
;Exercise 2.42:
;we are assuming that there is only one queen per column in writing these procedures.
(define (adjoin-position row column board)
  (append board (list (list row column))))
;selectors
(define (get-row column positions)
  (caar (filter (lambda (p) (= column (cadr p))) positions)))
(define (get-column row positions)
  (map (lambda (p) (cadr p)) (filter (lambda (p) (= row (car p))) positions)))
;safe-check procedures
(define (safe? k positions)
  (and (safe-row? k positions) (safe-diag? k positions)))
(define(safe-row? k positions)
  (= (length (get-column (get-row k positions) positions)) 1))
(define (same-diag? p v)
  (= (abs (- (car v) (car p))) (abs (- (cadr v) (cadr p)))))
(define (safe-diag? k positions)
  (= (length (filter (lambda (p) (same-diag? p (list (get-row k positions) k))) positions)) 1))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list ())
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))
;Exercise 2.43:
(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list ())
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
;Important point about flatmap:
;(flatmap f (a_1 a_2 \dots a_n)) can be rewritten in pseudo-LaTeX-math-code as \cup_{k=1}^n (f a_k)

(define (timed-queens n start-time)
  (queens n)
  (- (runtime) start-time))
(define (timed-queens-slow n start-time)
  (queens-slow n)
  (- (runtime) start-time))
(define (compute-queens-slow n)
  (/ (* (expt n ( / n 2 )) (timed-queens n (runtime))) 2))
;this approximation works suprisingly well for small values. 
;I don't know why it works... I got to it by just looking at
;how many times queen-colls is counted, and made some guesses
;and trial and errored it until it got to be a pretty decent
;approximation.
(define (check-approx n)
  (/ (compute-queens-slow n) (timed-queens-slow n (runtime))))
;================ 2.3 Symbolic Data ======================
;================ 2.3.1 Quotation ========================
;memq definition
;(define (memq item x)
;  (cond ((null? x) false)
;        ((eq? item (car x)) x)
;        (else (memq item (cdr x)))))
;Exercise 2.53:
;did it in the interpreter
;Exercise 2.54:
;first predicate is there to avoid type error arising from (car ())
(define (equal? a b)
  (cond ((and (eq? a ()) (eq? b ())) 
         #t)
        ((and (list? a) (list? b))
         (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((not (and (list? a) (list? b)))
         (eq? a b))))
;Exercise 2.55:
;(car ''abracadabra) returns quote because ''abracadabra returns (quote abracadabra), and the car of that list is quote.
;============== 2.3.2. Example: Symbolic Differentiation ================
;the differentiation program with abstract data
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((and (number? (exponent exp)) (exponentiation? exp))
         (make-product
           (exponent exp)
           (make-product 
             (make-exponentiation (base exp) (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else 
          (error "unknown expression type: DERIV" exp))))

;Representing algebraic expressions
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))
(define (make-sum p1 . p2)
  (cond ((=number? p1 0) p2)
        ((=number? p2 0) p1)
        ((and (number? p1) (number? p2))
         (+ p1 p2))
        (else (append (list '+) (append (list p1) p2)))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product p1 . p2) 
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((and (number? p1) (number? p2)) (* p1 p2))
        (else (append (list '*) (append (list p1) p2)))))
(define (sum? x) 
  (and (pair? x) (or (eq? (car x) '+) (eq? (cadr x) '+))))
(define (addend s) 
  (cond ((eq? (car s) '+)
         (cadr s))
        ((eq? (cadr s) '+)
         (car s))
        (else (error "Not a supported sum representation!:" s))))
(define (augend s) 
  (cond ((eq? (car s) '+)
         (cond ((> (length s) 3)
                (append (list '+) (cddr s)))
               ((= (length s) 3)
                (caddr s))
               (else 0)))
        ((eq? (cadr s) '+)
         (if (> (length s) 3)
             (cddr s)
             (caddr s)))
        (else (error "Not a sum!:" s))))
(define (product? x) 
  (and (pair? x) (or (eq? (car x) '*) (eq? (cadr x) '*))))
(define (multiplier p) 
  (cond ((eq? (car p) '*)
         (cadr p))
        ((eq? (cadr p) '*)
         (car p))
        (else (error "Not a supported sum representation!:" p))))
(define (multiplicand p) 
  (cond ((eq? (car p) '*)
         (cond ((> (length p) 3)
                (append (list '*) (cddr p)))
               ((= (length p) 3)
                (caddr p))
               (else 1)))
        ((eq? (cadr p) '*)
         (caddr p))
        (else (error "Not a product!:" p))))
;Exercise 2.56:
;added new clause to the deriv program
(define (exponentiation? e)
  (eq? (car e) '**))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))
;(list '** b n) represents b^n
(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        (else (list '** b n))))
;Exercise 2.57:
;done
;Exercise 2.58:
;done
;add conditions to make the deriv procedure work with infix notation.
;============== 2.3.3. Example:  Representing Sets ======================
;Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t) 
        (else (element-of-set? x (cdr set)))))
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))
;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1) (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))
;;Exercise 2.59:
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2)
;         (union-set (cdr set1) set2))
;        (else (cons (car set1) (union-set (cdr set1) set2)))))
;Exercise 2.60:
;(define (adjoin-set x set)
;  (cons x set))
;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1) (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))
;(define (union-set set1 set2)
;  (append set1 set2))
;sets as ordered lists
;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((= x (car set)) true)
;        ((< x (car set)) false)
;        (else (element-of-set? x (cdr set)))))
;(define (intersection-set set1 set2)
;  (if (or (null? set1) (null? set2))
;      '()
;      (let ((x1 (car set1)) (x2 (car set2)))
;        (cond ((= x1 x2)
;               (cons x1 (intersection-set (cdr set1) (cdr set2))))
;              ((< x1 x2)
;               (intersection-set (cdr set1) set2))
;              ((< x2 x1)
;               (intersection-set set1 (cdr set2)))))))
;(define (adjoin-set x set)
;  (cond ((null? set) (list x))
;        ((= x (car set)) set)
;        ((and (< x (car set)) (> x (cadr set))) (cons (car set) (cons x (cdr set)))) 
;        ((< x (car set)) (cons x set))
;        (else (cons (car set) (adjoin-set x (cdr set))))))
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
;        ((< (car set2) (car set1)) (cons (car set2) (union-set (cdr set2) set1)))))
;sets as trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
        (make-tree (entry set)
                   (adjoin-set x (left-branch set))
                   (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree) 
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))
(define (tree->list-2-timed tree starttime)
  (tree->list-2 tree)
  (- (runtime) starttime))
;trees used in exercise 2.63

;Exercise 2.64:
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
;partial-tree called with a list and its length builds a tree recursively
;by calling itself to build left and right subtrees each of roughly half 
;the elements in the list. The first of the non-left-elts becomes the root of the tree,
;and the remaining ones become the fodder for the right subtree. 
;b The time complexity is O(n) for a list of n elements. 
;Exercise 2:65:
(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1)) (list2 (tree->list-2 set2)))
    (let ((union-list (append list1 list2)))
      (list->tree union-list))))
(define (intersection-set set1 set2)
  (if (or (and (null? (car set1)) (null? (cadr set1))) (and (null? (car set2)) (null? (cadr set2))))
      '()
      (let ((list1 (tree->list-2 set1)) (list2 (tree->list-2 set2)))
        (if (or (null? set1) (null? set2) )
            '()
            (let ((x1 (car set1)) (x2 (car set2)))
              (cond ((= x1 x2)
                     (list->tree (cons x1 (intersection-set (cdr set1) (cdr set2)))))
                    ((< x1 x2)
                     (list->tree (intersection-set (cdr set1) set2)))
                    ((< x2 x1)
                     (list->tree (intersection-set set1 (cdr set2))))))))))
;Sets and information retrieval
;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) false)
;        ((equal? given-key (key (car set-of-records)))
;         (car set-of-records))
;        (else (lookup given-key (cdr set-of-records)))))
;Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (car (entry set-of-records))) (entry set-of-records))
        ((< given-key (car (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (car (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
  ;============== 2.3.4. Example: Huffman Ecoding Trees ===================
;Encoding
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE BRANCH: bit"))))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;Exercise 2.67:
(define (sample-tree)
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define (sample-message) '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;Exercise 2.68:
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;order of growth of encode-symbol???
;on average, assuming in the average case we get a symmetric binary tree, we get O(nlogn).
;Using the tree in Exercise 2.71, searching for the least frequent symbol we get O(n),
;since we search the left-branch first (taking O(n) time) and then search the right branch,
;but the right branch will only have one element, so that'll take O(1), which sums together to O(n).
;Searching for the most frequent symbol will take us nO(n)=O(n^2). You can compute that by noticing that
;we have in that case T(n)=T(n-1)+O(n-1). We solve that to get T(n)=O(n^2).
(define (encode-symbol symbol tree)
  (define (symbol-trail symbol tree trail)
    (cond ((null? tree) trail)
          ((leaf? tree)
           (if (eq? (symbol-leaf tree) symbol) trail))
          ((memq symbol (symbols (left-branch tree))) (symbol-trail symbol (left-branch tree) (append trail (list 0))))
          ((memq symbol (symbols (right-branch tree))) (symbol-trail symbol (right-branch tree) (append trail (list 1))))
          (else (error "The symbol isn't represented in the language!:" symbol))))
  (symbol-trail symbol tree '()))
 ;Exercise 2.69: 
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs) 
  (cond ((= (length pairs) 1) pairs)
        ((= (length pairs) 2) (make-code-tree (car pairs) (cadr pairs)))
        (else (successive-merge 
                (adjoin-set 
                  (make-code-tree (car pairs) (cadr pairs)) 
                  (cddr pairs))))))
;Exercise 2.70
(define (lang-for-rock)
  '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define (tree-for-rock)
  (generate-huffman-tree (lang-for-rock)))
(define (rock-lyrics)
  '(Get a job Sha na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
;Ran (encode (rock-lyrics) (tree-for-rock)). Got a list of length 83, so 83 bits.
;The minimum for fixed size. Well, we have 10 different symbols. We need thus (ceiling (log10))*36=4*36= 120+24=144 bits. Thus,
;the variable-length code saves us 144-83=61 bits.
;Exercise 2.71
(define (pairs-5)
  (list (list 's_1 (expt 2 1)) (list 's_2 (expt 2 2)) (list 's_3 (expt 2 3)) (list 's_4 (expt 2 4)) (list 's_5 (expt 2 5))))
(define (huffman-tree-5)
  (generate-huffman-tree (pairs-5)))
(define (pairs-10)
  (append (pairs-5) 
          (list (list 's_6 (expt 2 6)) (list 's_7 (expt 2 7)) (list 's_8 (expt 2 8)) (list 's_9 (expt 2 9)) (list 's_10 (expt 2 10)))))
(define (huffman-tree-10)
  (generate-huffman-tree (pairs-10)))
;the most frequent symbol can be encoded as 1. The least frequent bit will be the maximum path length of the tree, which is n-1. Thus, the
;least frequent symbol will need n-1 bits. In our example, it is \underline{0\dots0}{n-1 times} and the most frequent symbol will just be 1. 
;The trees are symmetric, so we can just do s_n encodes to 0 and s_1 encodes to \underline{1\dots1}{n-1 times}
;Exercise 2.72:
;Answer is a comment above Exercise 2.68.
;============== 2.4 Multiple Representations for Abstract Data ==========
;============== 2.4.1 Representations for Complex Numbers ===============
;arithmetic package
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1_ (angle z2)))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
;============= 2.4.2 Tagged Data =======================
(define (attach-tag type-tag contents)
  (cond ((eq? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (cond ((number? contents) contents)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum: CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;tagged rectangular representation
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a))  (* r (sin a)))))
;tagged polar representation
(define (real-part-polar z) 
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag polar'
              (cons r a)))
;selectors based on tag
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))
;constructors
;it is more efficient to do it this way. just look at the time complexity of each.
;(define (make-from-real-imag x y)
;  (make-from-real-imag-rectangular x y))
;(define (make-from-mag-ang r a)
;  (make-from-mag-ang-polar r a))
;if two programmers wrote these two implementations of complex numbers separately, a third programmer could implement the arithmetic package,
;without ever having to know how the complex numbers are specifically implemented. 
;==============2.4.3. Data-Directed Programming and Additivity ==============
;For this section assume we have two procedures, put and get, for manipulating the operation-and-type TABLE:
;put syntax
;(put <op> <type> <item>) installs the <item> in the table, indexed by the <op> and the <type>
;get syntax
;(get <op> <type>) looks up the <op>, <type> entry in the table and returns the item found there. If no item is found, get returns false.
;for now, we assume put and get come with Scheme, but later (Chapter 3, Section 3.3.3.) we will implement these.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;(define (make-from-real-imag x y)
;  ((get 'make-from-real-imag 'rectangular) x y))
;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 'polar) r a ))
;Exercise 2.73
;dd stands for data-directed
(define (dd-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;a
;We transformed the procedure from Section 2.3.2. into one that uses data-directed programming and that is additive, i.e. one that allows us to 
;install new procedures in our table based on tags without having to change any of the other procedures that we've already installed.
;Let's suppose that we could assimilate the predicates number? and variable?. Then we could write (get 'deriv (operator exp)) and get
;number? variable?, for some (operator exp). What sort of operator would that be? It isn't +,-,*,/, raising to an integer power, or taking roots... Well, that's a contradiction then, 
;because these are all of the possible algebraic operators, i.e. all possible tags! Hence, these two predicates cannot be assimilated into
;the data directed dispatch!  
;b and c
(define (install-deriv-package)
  (define (sum exp var)
    (make-sum (dd-deriv (addend exp) var)
              (dd-deriv (augend exp) var)))
  (define (prod exp var)
    (make-sum
      (make-product (multiplier exp)
                    (dd-deriv (multiplicand exp) var))
      (make-product (dd-deriv (multiplier exp) var)
                    (multiplicand exp))))
  (define (exponentiation exp var)
    (make-product
      (exponent exp)
      (make-product 
        (make-exponentiation (base exp) (- (exponent exp) 1))
        (deriv (base exp) var))))
  (put 'deriv '+ sum)
  (put 'deriv '* prod)
  (put 'deriv '** exponentiation) 
  'done)
;d We just change our put statements, i.e. (put 'deriv '+ sum) \mapsto (put '+ 'deriv sum), and so on. 
;  The reason is that in the end we aren't changing the implementations that we wish to put in the table,
;  just the naming of the rows and columns.
;a.
;Exercise 2.74
;a) I am assuming that the individual divisions' files are structured such that there already exists, for each division, a get-record method. I am assuming there exists an (<operator>, personnel-file) table. The reason I am not assuming there exists an (<operator>, division) table is that this assumption would then require the additional assumption that there exists a method that takes personnel-file as argument and returns the division of that personnel-file. Since each division has exactly one personnel-file, we have a 1-1 correspondence between the two pieces of data. Indexing by personnel-file allows me to make one less assumption and have the same amount of columns. Thus, we have: ;Since there is a 1-1 correspondence between division and personnel-file, we can assume there exists a <operation>, personnel-file table.
(define (get-record employee personnel-file)
  ((get 'get-record personnel-file) employee))
;b) Now, I just assume that each record is structured such that each division can implement and has implemented their own get-salary method, that takes as argument the employee's record and returns the employee's salary. Thus:
(define (get-salary employee personnel-file)
  ((get 'get-salary personnel-file) (get-record employee personnel-file)))
;c) I'm assuming that each individual divisions get-record method returns '() if the employee is not employed in that division, and so the method that we implemented in part a) of this question returns '() in that case, too. We have:
(define (find-employee-record employee division-files)
  (cond ((null? division-files) '())
        ((not (null? (get-record employee (car division-files))))
         (get-record employee (car division-files)))
        (else (find-employee-record employee (cdr division-files)))))
;d) Call the newly acquired division-file new-division-file. Entries corresponding to (get-record, new-division-file) and (get-salary, new-division-file) need to be added to our (<operator>, division-file) table. That is, the newly acquired company needs to implement locally their own versions of get-record, get-salary, then we need to add these implementations to our table, and then the methods defined in a), b), c) will work.
;Message Passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)
(define (apply-generic op arg) (arg op))
;Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op)))))
;Exercise 2.76
;Explicit dispatch:
;  new types:
;    Write new procedures to handle the new types.  
;  new operations:
;    Write the new operations for each of the types.  
;Data directed:
;  new types:
;    This corresponds to new columns in our (<operator>,types) table.
;  new operations:
;    This correspond to new rows in our (<operator>,types) table. 
;Message passing:
;  new types:
;    We would have to write new constructors for each type that handles each operation.
;  new operations:
;    We would have to add new operations into all of our existing objects.
;Which organization would be most appropirate for a system in which new types must often be added?
;  In the explicit dispatch method, we might have collisions between the operations, as they all have to be changed each time a new type is added;
;  I mean they have to be renamed, each time.
;  In the message passing implementation of these new types we would have to create new constructosr for each type, with accompanied new operations for each type.
;  In the data directed implementation, we just have to insert new columns in our table. 
;  If we are dealing with many different new types, it might be cleaner and easier to just deal with one table, i.e. just update one package,
;  so I would say data directed programming is the most convenient. 
;Which organization would be most appropriate for a system in which new operations must often be added?
;  In the explicit dispatch, for each operation we have to write a method for each type.
;  We would have (number of new operations)*(number of types) many new operations to write. This could be ugly and difficult to manage.
;  In the data directed paradigm, we would have to insert new columns and write new methods for each type, which isn't as bad as the explicit dispatch,
;  but maybe we can improve this.
;  In the message passing implementation, we just have to add new operations to handle for each existing type. This wouldn't add any new definitions
;  to our code. It would extend the existing objects... 
;  I prefer the message passing approach for this situation. 
;===========2.5 Systems with Generic Operations===============
;===========2.5.1. Generic Arithmetic Operations==============
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic -mul x y))
(define (div x y) (apply-generic 'div x y))
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number 
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-rational-package)
  ;;internal procedures
  (define (make-rat n d) (cons n d))
  (define (numer x)
    (let ((g (gcd (car x) (cdr x))))
      (/ (car x) g)))
  (define (denom x)
    (let ((g (gcd (car x) (cdr x))))
      (/ (cdr x) g)))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (sub-rat x y) 0))
  (define (=zero? x)
    (= (numer x) 0))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a ))
(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (=zero? z)
    (and (= (real-part z) 0) (= (imag-part z) 0)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;;interface to the rest of the system
  (define (tag x) (attach-tax 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put '=zero? '(rectangular) =zero?)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (=zero? z)
    (= (magnitude z) 0))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;;interface to the rest of the system 
  (define (tag x) (attach-tax 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put '=zero? '(polar) =zero?)
  (put 'make-from-real-imag '(polar) 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar) 
       (lambda (x y) (tag (make-from-mag-ang r a))))
  'done)
(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1_ (angle z2)))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (equ? z w)
    (and
      (= (real-part z) (real-part w)) (= (imag-part z) (imag-part w))))
  (define (=zero? z)
    (apply-generic '=zero? z))
  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'make-from-real-imag '(complex complex)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(complex complex)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-complex-from-real-imag 'complex) r a))
;Exercise 2.77: 
(define (equ? a b)
  (apply-generic 'equ? a b))
(define (=zero? a)
  (apply-generic '=zero? a))
;Here, we have z=(complex rectangular 3 4), which represents z=3+4i.
;1. (magnitude z)
;2. (apply-generic 'magnitude z)
;     type-tags=(map type-tag args)=(map type-tag (z))=(complex)
;     proc=(get op type-tags)=(get 'magnitude (complex))=magnitude
;   (apply magnitude ((rectangle 3 4)))
;3. (magnitude (rectangle 3 4))
;   (apply-generic 'magnitude (rectangle 3 4))
;     type-tags=(rectangle)
;     proc=(get 'magnitude (rectangle))=magnitude
;   (apply magnitude (3 4))
;   5
;apply-generic is called twice. The first time we dispatch back to the
;magnitude procedure from the complex-package, with the complex tag stripped off, 
;then we dispatch to the magnitude procedure in the rectangle-package, with rectangle tag stripped off,
;then this procedure computes the magnitude. 
;Exercise 2.78
;done
;Exercise 2.79
;done
;==============2.5.2. Combining Data of Different Types==============
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
;(put-coercion 'scheme-number 'complex scheme-number->complex)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))
;Exercise 2.81:
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;a.
;(define (exp x y) (apply-generic 'exp x y))
;;following added to Scheme-number package
;(put 'exp '(scheme-number scheme-number)
;     (lambda (x y) (tag (exp x y)))
;Let a b be two scheme-numbers. Then:
;1) (exp a b)
;   (apply-generic 'exp a b)
;     type-tags: (scheme-number scheme-number)
;     proc     : (get 'exp (scheme-number scheme-number))=exp
;2) (apply exp (a b))
;   (exp a b) 
;and we get an infinite loop.
;Let z=(complex rectangle 2 3) w=(complex rectangle 4 5).
;Say we call
;(exp z w)
;we get:
;1) (exp z w)
;   (apply-generic 'exp z w)
;     type-tags: (complex complex)
;     proc: (get 'exp (complex complex))=#f
;2) (apply-generic 'exp (t1->t2 a1) a2)
;     (t1->t2 a1)=a1, since both types are 'complex, so we just get
;   (apply-generic 'exp a1 a2),
;     and we are back to where we started.
;again, we get an infinite loop.
;b.
;He is correct, but his solution doesn't work. The way he tried to solve the issue created a new issue 
;of infinite looping, both in the case where a procedure exists and in the case where it doesn't.
;c.Done. Added a condition that checks if the two types are the same, and if they are, we return an error.
;Exercise 2.82
(define (apply-generic op . args)
  ;;get-coercion-list behaves like (type_1 type_2 ... type_n type) \mapsto (type_1->type type_2->type \dots type_n->type)
  ;;if (eq? type_i type) then (eq? type_i->type identity)
  (define (get-coercion-list type-tags type)
    (if (<= (length (type-tags)) 1)
        ()
        (cond ((eq? (car type-tags) type)
               (cons identity (get-coercion-list (cdr type-tags) type)))
              ((get-coercion (car type-tags) type)
               (cons (get-coercion (car type-tags) type) (get-coercion-list (cdr type-tags) type)))
              (else (get-coercion-list (cdr type-tags) type))))) 
  (define (generate-list-of-n-copies element n)
    (define (iter n l)
      (if (= n 1)
          l
          (iter element (- n 1) (cons element l))
          ))
    (iter n ()))
  ;;should apply coercion at index i in coercion-list to arg at index i in args.
  (define (coerce-args coercion-list args)
    (if (not (= (length (coercion-list)) (length args)))
        (error "Invalid input. Both arguments must be lists of the same length!:" coercion-list args)
        (if (null? coercion-list)
            ()
            (cons ((car coercion-list) (car args)) (coerce-args (cdr coercion-list) (cdr args))))))
  ;;loops through type-tags list, checks to see if the args can be coerced to each type in the type-tags list,
  ;;and if it can, checks if a procedure exists for this coerced type,
  ;;and if it can, it applies this procedure to the coerced arguments and returns the result.
  (define (try-op-on-coerced-type type-tags index op args)
    (if (>= index (length type-tags))
        (error "This operation is not possible on the given arguments!" op args)
        (let ((type (list-ref type-tags index)))
          (if (= (length (get-coercion-list type-tags type)) (length type-tags))
              (let ((coercion-list (get-coercion-list type-tags type)))
                ;;search to see if a procedure exists for the operation and the new type-list
                (let ((type-list (generate-list-of-n-copies type (length type-tags))))
                  (let ((proc-coerced (get op type-list)))
                    (if proc-coerced
                        (let ((args-coerced (coerce-args coercion-list args)))
                          (apply proc-coerced args-coerced))
                        (try-op-on-coerced-type type-tags (+ index 1) op args)))))
              (try-op-on-coerced-type type-tags (+ index 1) op args)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-op-on-coerced-type type-tags 0 op args)))))
;;This example would fail for something like scalar-multiplication, which requires an object of type scalar and an object of type vector. 
;;The procedure above would first try to coerce vector to scalar and then scalar to vector, and give up, saying no procedure exists,
;;even if we define and put into our table a scalar multiplication procedure, after defining scalar and vector. 
;Exercise 2.83:
;We want all lower types to inherit procedures from higher types. Say magnitude is defined for complex numbers, but not for any of the lower types, then we want
;(apply-generic 'magnitude integer-object) to still return a magnitude, since an integer is a complex number... Say the integer is a, then we want to represent that
;by (integer rational real complex a), and then for each of the other types, we just need to add (put 'magnitude 'type), and the apply-generic procedure will
;go from tag to tag, climbing up the hierarchy, until it hits the complex magnitude procedure, and then it can apply it. 
;complex
;  ^
;  |
; real
;  ^
;  |
;rational
;  ^
;  |
;integer
;Assume that integers are tagged with 'integer. 
(define (install-integer-package)
  <stuff>
  ;apply-generic will strip off the integer tag, so we don't explicitly have to worry about doing that in our raise definition here.
  (define (raise a)
    ((get 'make 'rational) a 1))
  (put 'raise '(integer) raise)
  'done)
(define (install-rational-package)
  <stuff>
  ;apply-generic strips off rational tag, leaving us with a pair (numer denom).
  (define (raise r)
    ((get 'make 'real) (/ (car r) (cadr r))))
  (put 'raise '(rational) raise)
  'done)
(define (install-real-package)
  <stuff>
  (define (raise r)
    ((get 'make 'complex) r 0))
  (put 'raise '(complex) raise)
  'done)
(define (install-complex-package)
  <stuff>)
;generic procedure definition. 
(define (raise x)
  (apply-generic 'raise x))
;Exercise 2.84:
(define (apply-generic op . args)
  (define (higher-type type1 type2)
    (cond ;;if one is the lowest type, then not that one.
          ((eq? type1 'integer) type2)
          ((eq? type2 'integer) type1)
          ;;if one is the highest type, then that one. 
          ((eq? type1 'complex) type1)
          ((eq? type2 'complex) type2)
          ;;type1 is either 'real or 'rational, so check for that.
          ((eq? type1 'real) type1)
          ((eq? type1 'rational) type2)
          ;;handle error
          (else "One of the types are not supported!:" (list type1 type2))))
  (define (higher-type? type1 type2)
    (if (eq? (higher-type type1 type2) type1)
        #t
        #f))
  (define (highest-type type-list)
    (define (recursive-checker type type-list)
      (cond ((null? type-list) type) 
            ((higher-type? (car type-list) type) (recursive-checker (car type-list) (cdr type-list)))
            (else (recursive-checker type (cdr type-list)))))
    (recursive-checker (car type-list) type-list))
  ;;this procedure will always be used in apply-generic such that the type of x is always less than type. So I won't check pathological cases. 
  (define (raise-to-type type x)
    (if (eq? (type-tag x) type)
        x
        (raise-to-type type ((get 'raise (type-tag x)) x))))
  (define (raise-args type args)
    (cond ((null? args) ())
          ((eq? (type-tag (car args)) type)
           (cons (car args) (raise-args type (cdr args))))
           (else (raise-to-type type (car args)) (raise-args type (cdr args)))))
  (define (generate-list-of-n-copies element n)
    (define (iter n l)
      (if (= n 1)
          l
          (iter element (- n 1) (cons element l))
          ))
    (iter n ()))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;;else, we want to coerce args to highest type of that array, and try again.
          (let ((highest-type-list (generate-list-of-n-copies (highest-type type-tags) (length type-tags))))
            (let ((coerced-proc (get op highest-type-list)))
              (if coerced-proc
                  (let ((coerced-args (raise-args (highest-type type-tags) args)))
                    (drop (apply coerced-proc coerced-args)))
                  (error "No such operation for the given args!" (list op args)))))))))
;exercise 2.85:
(define (install-integer-package)
  <stuff>
  (put 'equa? '(integer integer) =)
  'done)
(define (install-rational-package)
  <stuff>
  (define (project r) 
    ((get 'make 'integer) (round (/ (car r) (cadr r))))) 
  (define (equa? r s)
    (and (= (numer r) (numer s)) 
         (= (denom r) (denom s))))
  (put 'project '(rational) project)
  (put 'equa? '(rational rational) equa?)
  'done)
(define (install-real-package)
  <stuff>
  (define (project r)
    (define (recursively-build-rat i a)
      ;;recursive loop will end, since no irrational numbers can be expressed in finitely memory. 
      (if (= (floor (* i a)) (* i a))
          ((get 'make 'rational) (* i a) i)
          (recursively-build-rat (+ i 1) a)))
    (recursively-build-rat 1 r))
  (put 'project '(real) project)
  (put 'equa? '(real real) =)
  'done)
(define (install-complex-package)
  <stuff>
  (define (project z)
    ((get 'make 'real) (real-part z)))
  (define (equa? z w)
    (and (= (real-part z) (real-part w)) (= (imag-part z) (imag-part w))))
  'done)
;generic procedure definition. 
(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))
(define (equa? a b)
  (apply-generic 'equa? a b))
(define (drop x)
  (if (not (equa? (raise (project x)) x))
      x
      (drop (project x))))
;exercise 2.86:
;Assuming that apply-generic does some sort of type coercion, we can just install generic 'square, 'sqrt, 'cos, 'sin, and 'atan procedures. 
;I could rewrite all of these in a much simpler way by just outright defining generic 'square, 'sqrt, 'cos, 'sin, and 'atan procedures,
;and make the code look a lot prettier, but the exercise is done and I don't feel like making that change.
;(define (install-scheme-number-package)
;  (define (tag x) (attach-tag 'scheme-number x))
;  (put 'add '(scheme-number scheme-number)
;       (lambda (x y) (tag (+ x y))))
;  (put 'sub '(scheme-number scheme-number)
;       (lambda (x y) (tag (- x y))))
;  (put 'mul '(scheme-number scheme-number)
;       (lambda (x y) (tag (* x y))))
;  (put 'div '(scheme-number scheme-number)
;       (lambda (x y) (tag (/ x y))))
;  (put 'square '(scheme-number scheme-number) square)
;  (put 'sqrt '(scheme-number scheme-number) sqrt)
;  (put 'cos '(scheme-number) cos)
;  (put 'sin '(scheme-number) sin)
;  (put 'atan '(scheme-number scheme-number) atan)
;  (put 'make 'scheme-number 
;       (lambda (x) (tag x)))
;  'done)
;(define (install-rational-package)
;  ;;internal procedures
;  (define (make-rat n d) (cons n d))
;  (define (numer x)
;    (let ((g (gcd (car x) (cdr x))))
;      (/ (car x) g)))
;  (define (denom x)
;    (let ((g (gcd (car x) (cdr x))))
;      (/ (cdr x) g)))
;  (define (add-rat x y)
;    (make-rat (+ (* (numer x) (denom y))
;                 (* (numer y) (denom x)))
;              (* (denom x) (denom y))))
;  (define (sub-rat x y)
;    (make-rat (- (* (numer x) (denom y))
;                 (* (numer y) (denom x)))
;              (* (denom x) (denom y))))
;  (define (mul-rat x y)
;    (make-rat (* (numer x) (numer y))
;              (* (denom x) (denom y))))
;  (define (div-rat x y)
;    (make-rat (* (numer x) (denom y))
;              (* (denom x) (numer y))))
;  (define (square-rat x)
;    (make-rat (square (numer x)) (square (denom x))))
;  (define (sqrt-rat x)
;    (make-rat (sqrt (numer x)) (sqrt (denom x))))
;  (define (cos-rat x)
;    (cos (/ (numer x) (denom x))))
;  (define (sin-rat x)
;    (sin (/ (numer x) (denom x))))
;  (define (atan-rat x y)
;    (atan (/ (numer x) (denom x)) (/ (numer y) (denom y))))
;  ;;interface to the rest of the system
;  (define (tag x) (attach-tag 'rational x))
;  (put 'add '(rational rational)
;       (lambda (x y) (tag (add-rat x y))))
;  (put 'sub '(rational rational)
;       (lambda (x y) (tag (sub-rat x y))))
;  (put 'mul '(rational rational)
;       (lambda (x y) (tag (mul-rat x y))))
;  (put 'div '(rational rational)
;       (lambda (x y) (tag (div-rat x y))))
;  (put 'square '(rational) square-rat)
;  (put 'sqrt '(rational) sqrt-rat)
;  (put 'cos '(rational) cos-rat)
;  (put 'sin '(rational) sin-rat)
;  (put 'atan '(rational rational) atan-rat)
;  (put 'make 'rational
;       (lambda (n d) (tag (make-rat n d))))
;  'done)
;(define (install-rectangular-package)
;  ;;internal procedures
;  (define (real-part z) (car z))
;  (define (imag-part z) (cdr z))
;  (define (make-from-real-imag x y) (cons x y))
;  (define (magnitude z)
;    (apply-generic 'sqrt (apply-generic 'add (apply-generic 'square (real-part z))
;             (apply-generic 'square (imag-part z)))))
;  (define (angle z)
;    (apply-generic 'atan (imag-part z) (real-part z)))
;  (define (make-from-mag-ang r a)
;    (cons (apply-generic 'mul r (apply-generic 'cos a)) (apply-generic 'mul r (apply-generic 'sin a))))
;  
;  ;;interface to the rest of the system
;  (define (tag x) (attach-tax 'rectangular x))
;  (put 'real-part '(rectangular) real-part)
;  (put 'imag-part '(rectangular) imag-part)
;  (put 'magnitude '(rectangular) magnitude)
;  (put 'angle '(rectangular) angle)
;  (put 'make-from-real-imag 'rectangular
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'rectangular
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  'done)
;(define (install-polar-package)
;  ;;internal procedures
;  (define (magnitude z) (car z))
;  (define (angle z) (cdr z))
;  (define (make-from-mag-ang r a) (cons r a))
;  (define (real-part z) (apply-generic 'mul (magnitude z) (apply-generic 'cos (angle z))))
;  (define (imag-part z) (apply-generic 'mul (magnitude z) (apply-generic 'sin (angle z))))
;  (define (make-from-real-imag x y)
;    (cons (apply-generic 'sqrt (apply-generic 'add (apply-generic 'square x) (apply-generic 'square y)))
;          (apply-generic 'atan y x)))
;  ;;interface to the rest of the system 
;  (define (tag x) (attach-tax 'polar x))
;  (put 'real-part '(polar) real-part)
;  (put 'imag-part '(polar) imag-part)
;  (put 'magnitude '(polar) magnitude)
;  (put 'angle '(polar) angle)
;  (put 'make-from-real-imag '(polar) 
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang '(polar) 
;       (lambda (x y) (tag (make-from-mag-ang r a))))
;  'done)
;(define (install-complex-package)
;  ;;imported procedures from rectangular and polar packages
;  (define (make-from-real-imag x y)
;    ((get 'make-from-real-imag 'rectangular) x y))
;  (define (make-from-mag-ang r a)
;    ((get 'make-from-mag-ang 'polar) r a))
;  ;;internal procedures
;  (define (add-complex z1 z2)
;    (make-from-real-imag (apply-generic 'add (real-part z1) (real-part z2))
;                         (apply-generic 'add (imag-part z1) (imag-part z2))))
;  (define (sub-complex z1 z2)
;    (make-from-real-imag (apply-generic 'sub (real-part z1) (real-part z2))
;                         (apply-generic 'sub (imag-part z1) (imag-part z2))))
;  (define (mul-complex z1 z2)
;    (make-from-mag-ang (apply-generic 'mul (magnitude z1) (magnitude z2))
;                       (apply-generic 'add (angle z1_ (angle z2)))))
;  (define (div-complex z1 z2)
;    (make-from-mag-ang (apply-generic 'div (magnitude z1) (magnitude z2))
;                       (apply-generic 'sub (angle z1) (angle z2))))
;  (define (magnitude z)
;    (apply-generic 'magnitude z))
;  ;;interface to rest of the system
;  (define (tag z) (attach-tag 'complex z))
;  (put 'add '(complex complex)
;       (lambda (z1 z2) (tag (add-complex z1 z2))))
;  (put 'sub '(complex complex)
;       (lambda (z1 z2) (tag (sub-complex z1 z2))))
;  (put 'mul '(complex complex)
;       (lambda (z1 z2) (tag (mul-complex z1 z2))))
;  (put 'div '(complex complex)
;       (lambda (z1 z2) (tag (div-complex z1 z2))))
;  (put 'real-part '(complex) real-part)
;  (put 'imag-part '(complex) imag-part)
;  (put 'magnitude '(complex) magnitude)
;  (put 'angle '(complex) angle)
;  (put 'make-from-real-imag '(complex complex)
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang '(complex complex)
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  'done)
;==============2.5.3. Example: Symbolic Algebra================

;Exercise 2.87:
;added =zero? to polynomial package above, and the definition is:
(define (=zero? p)
  (apply-generic '=zero? p))
;since the interpreter reads from top to bottom, I should probably move this code above. Will do that at a later date.
;Exercise 2.88:
;We could do this in two separate ways. We already have a sub procedure for every type below poly, so we could 
;just use that procedure on coefficients to define sub-terms. The following code implements this solution.
(define (install-polynomial-package)
  <stuff>
  (define (sub-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ((t1 (first-term l1))
                  (t2 (first-term l2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (sub-terms (rest-terms l1) l2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (sub-terms (rest-terms l1) l2)))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (sub (coeff t1) (coeff t2)))
                        (sub-terms (rest-terms l1)
                                   (rest-terms l2)))))))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY:" (list p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)
;An alternative solution, as suggested by the hint, is the following:
(define (install-scheme-number-package)
  <stuff>
  (put 'minus '(scheme-number) -)
  'done)
(define (install-rational-package)
  <stuff>
  (define (minus r)
    (make-rat (- (numer r)) (denom r)))
  (put 'minus '(rational) 
       (lambda (r) (tag (minus r))))
  'done)
(define (install-complex-package)
  <stuff>
  (define (minus z)
    (make-from-real-imag (- (real-part z)) (- (imag-part z))))
  (put 'minus '(complex) 
       (lambda (z) (tag (minus z))))
  'done)
(define (install-polynomial-package)
  <stuff>
  (define (minus p)
    (define (minus-term-list L)
      (if (null? L)
          ()
          (cons (make-term (order (first-term L)) (minus (coeff (first-term L)))) (minus-term-list (rest-terms L)))))

    (make-poly (variable p) (minus-term-list (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (minus p2)))
  (put 'minus '(polynomial) minus)
  (put 'sub '(polynomial polynomial) sub-poly)
  'done)
(define (minus p)
  (apply-generic 'minus p))
;The minus procedure is a useful generalization for our entire system, not just for this one exercise of defining sub for polynomials,
;so I think it is preferable. 
;Exercise 2.89:
;The order of a term now is the length of the sublist beginning with that term's coefficient - 1
;Exercise 2.90:
;We are going to split the polynomial package into polynomial-dense and polynomial-sparse packages, implement everything at that level, then unify these in the greater
;polynomial package; this is completely analogous to our treatment of complex numbers!
(define (install-sparse-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ((t1 (first-term l1))
                  (t2 (first-term l2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms l1) l2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms (rest-terms l1) l2)))
                     (else
                       (adjoin-term
                         (make-term (order t1)
                                    (add (coeff t1) (coeff t2)))
                         (add-terms (rest-terms l1)
                                    (rest-terms l2)))))))))
  (define (minus p)
    (define (minus-term-list L)
      (if (null? L)
          ()
          (cons (make-term (order (first-term L)) (minus (coeff (first-term L)))) (minus-term-list (rest-terms L)))))

    (make-poly (variable p) (minus-term-list (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (minus p2)))
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms l))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY:" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY:" (list p1 p2))))
  ;since in our representation there is not a unique way to represent the zero polynomial,
  ;i.e. we could represent the zero polynomial as the empty term-list or as
  ;((0 a_0) (1 a_0) ... (n a_n)), where (=zero? a_i) is true for each i (a_i being either of scheme-number,
  ;rational, complex, or polynomial type).
  (define (=zero? p)
    (define (recursive-checker L)
      (cond ((null? L)  #t)
            ((=zero? (coeff (first-term L)))
             (and #t (recursive-checker (rest-terms L))))
            (else #f))) 
    (recursive-checker (term-list p)))

  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2)))) 
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(sparse) =zero?)
  (put 'minus '(sparse) 
       (lambda (p) (tag (minus p))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (install-dense-package)
  (define (rest-terms term-list)
    (cdr term-list))
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
            (let ((t1 (first-term l1))
                  (ordert1 (- (length (rest-terms l1)) 1))
                  (t2 (first-term l2))
                  (ordert2 (- (length (rest-terms l2)) 1)))
              (cond ((> (ordert1) (ordert2))
                     (cons
                       t1 (add-terms (rest-terms l1) l2)))
                    ((< (ordert1) (ordert2))
                     (cons
                       t2 (add-terms (rest-terms l1) l2)))
                    (else
                      (cons
                        (add t1 t2)
                        (add-terms (rest-terms l1)
                                   (rest-terms l2)))))))))
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (cons
            (mul t1 t2)
            (mul-term-by-all-terms t1 (rest-terms l))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY:" (list p1 p2))))
  (define (minus p)
    (define (minus-term-list L)
      (if (null? L)
          ()
          (cons (minus (first-term L)) (minus-term-list (rest-terms L)))))
    (make-poly (variable p) (minus-term-list (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (minus p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY:" (list p1 p2))))
  (define (=zero? p)
    (define (recursive-checker L)
      (cond ((null? L)  #t)
            ((=zero? (first-term L))
             (and #t (recursive-checker (rest-terms L))))
            (else #f)))
    (recursive-checker (term-list p)))
  (define (tag p) (attach-tag 'dense p))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'minus '(dense) 
       (lambda (p) (tag (minus p))))
  (put 'sub '(dense dense)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(dense dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(dense) =zero?)
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (install-polynomial-package)
  (define (make-sparse-poly var terms)
    ((get 'make 'sparse) var terms))
  (define (make-dense-poly var terms)
    ((get 'make 'dense) var terms))
  (define (add-poly p1 p2)
    (apply-generic 'add p1 p2))
  (define (minus p)
    (apply-generic 'minus p))
  (define (sub-poly p1 p2)
    (apply-generic 'sub p1 p2))
  (define (mul-poly p1 p2)
    (apply-generic 'mul p1 p2))
  (define (=zero? p)
    (apply-generic '=zero? p))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p q) (tag (add-poly p q))))
  (put 'minus (polynomial) 
       (lambda (p) (tag (minus p))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p q) (tag (sub-poly p q))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p q) (tag (mul-poly p q)))) 
  (put '=zero? '(polynomial) =zero?)
  (put 'make-as-dense-poly 'polynomial
       (lambda (var terms) (tag (make-dense-poly var terms))))
  (put 'make-as-sparse-poly 'polynomial
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  'done)
;Exercise 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-term-list) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                      ;build list of terms for result. 
                      (let ((qtnt (make-term new-o new-c)))
                        (let ((rmdr (add-terms L1 (minus (mul-terms qtnt L2)))))
                          (if (< (order (first-term rmdr)) (order t2))
                              ;keep return object consistently a list of two objects,
                              ;the first representing the quotient and the second the remainder.
                              ;the actual final remainder is computer from the final quotient,
                              ;which is being recursively build, here.
                              (list () ())
                              (let ((next-qtnt-c (div (coeff (first-term rmdr)) (coeff t2)))
                                    (next-qtnt-o (- (order (first-term-rmdr)) (order t2))))
                                (let ((next-qtnt (make-term next-qtnt-o next-qtnt-c)))
                                  (let ((next-rmdr (add-terms rmdr (minus (mul-terms next-qtnt L2)))))
                                    (cons next-qtnt (car (div-terms next-rmdr L2)))))))))))
                (let ((final-quotient (cons result rest-of-result)))
                  (let ((final-rmdr (add-terms L1 (minus (mul-terms final-quotient L2)))))
                    (list final-quotient final-rmdr)))))))))
;HIERARCHIES OF TYPES IN SYMBOLIC ALGEBRA
;Exercise 2.92
;By imposing an ordering on variables, extend the polynomial package so that addition and multiplication of polynomials works for polynomials in different variables.
;Let's start by doing this first for dense and sparse polynomials.
;For now, suppose there exists only two possible variables x and y. Let's arrange them in a type-hierarchy where x is higher than y, so if we have p(x) + q(y), then we first
;y->x q(y) to q(x) (by expanding and rearranging such that y are coefficient terms in x), and then performing the computation. 
;To achieve this... we just need to install a y->x procedure for sparse and dense polynomials, and then extend it to the general polynomial package, and 
;then apply it before any algebraic operation! 

;Let p(y)= a_n(x)y^n + a_{n-1}(x)y^{n-1} + \dots + a_0(x). 
;This polynomial is represented as a sparse polynomial as
;(sparse y ((n a_n(x)) (n-1  a_{n-1}(x)) \dots (0  a_0(x))))
;Our y->x procedure must parce through each such a_n(x),
;generating new terms recursively as it goes along.
;This is my first attempt. Need to change it to be more general.
(define (install-sparse-package)
  <stuff>
  (define (y->x p)
    (define (expand-sparse var1 var2 n q)
      ;let q= b_mx^m + ... + b_0.
      ;Generates a new term-list
      (define (build-term-list L)
        (cons (make-term (order (first-term L)) (make-poly var2 (make-term n (coeff (first-term L))))) (build-term-list (rest-terms L))))
      (make-poly var1 (build-term-list (term-list q))))
    (define (expand-dense var1 var2 n q)
      ;let q= b_mx^m + ... + b_0.
      ;(make-poly var1 ((m (make-poly var2 (n b_m))) (m-1 (make-poly var2 (n b_{m-1}))) ... (0 (make-poly var2 (0 b_0)))))
      ;Generates a new term-list
      (define (build-term-list L)
        (cons (make-term (- (length (rest-terms L)) 1) (make-poly var2 (make-term n (car L)))) (build-term-list (rest-terms L))))
      (make-poly var1 (build-term-list (term-list q))))
    (if (eq? (variable p) 'x)
        p
        ;expand p and rearrange. 
        (let ((L (term-list p)))
          (let ((t1 (first-term L)))
            (let ((t1-c (coeff t1))
                  (t1-o (order t1)))
              (let ((rest-poly (make-poly (variable p) (rest-terms (term-list p)))))
                (cond ((eq? (type-tag t1-c) 'sparse)
                       (add (expand-sparse 'x 'y t1-o t1-c) (y->x rest-poly)))
                      ((eq? (type-tag t1-c) 'dense)
                       (add (expand-dense 'x 'y t1-0 t1-c) (y->x rest-poly)))
                      ((eq? (type-tag t1-c) 'polynomial)
                       (if (eq? (type-tag (cdr t1-c)) 'sparse)
                           (add (expand-sparse 'x 'y t1-o (contents t1-c)) (y->x rest-roly))
                           (add (expand-dense 'x 'y t1-o (contents t1-c)) (y->x rest-poly))))
                      (else
                        (add (make-poly 'x (make-term 0 (make-poly 'y (list t1)))) (y->x rest-poly))))))))))
  ;I'm only writing the algebraic operations that are different, and omitting put statements since those are unchanged.
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))

        ;assume only 'x and 'y variables are possible.
        (if (eq? (variable p1) 'y)
            (add-poly (y-> x p1) p2)
            (add-poly p1 (y->x p2)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        ;assume only 'x and 'y variables are possible.
        (if (eq? (variable p1) 'y)
            (mul-poly (y-> x p1) p2)
            (mul-poly p1 (y->x p2))))))

(define (install-dense-package)

  <stuff>
  ;Here the polynomial p(y)=a_n(x)y^n + a_{n-1}y^{n-1} + \dots + a_0(x) is represented simply as
  ;(dense y (a_n(x) a_{n-1}(x) ... a_0(x)))
  ;Now, the issue is that... we can't just blindly build term lists, and then sum them, because then we lose track of degree... We need a way around this,
  ;maybe we can build by degree. By that I mean we can write a procedure that takes in a polynomial in y and a number n, and expands
  ;and returns the coefficient of order n of the same polynomial considered as being in the variable x...Then, we could just recursively build a list of all the 
  ;degree coefficients, and that would be the full expansion of the polynomial in y considered as a polynomial in x, and that would be our final output. 
  (define (y->x p)
    ;p is a polynomial in y. As above, let p(y)=a_n(x)y^n + a_{n-1}y^{n-1} + \dots + a_0(x). Then,
    ;as a dense polynomial, it just has term-list (a_n(x) a_{n-1}(x) ... a_0(x))
    ;Now, our goal is to go through this list, expanding each polynomial in x, and pulling out degree n terms. 
    ;Later, we will think about how we can find the maximum degree, and then recursively, build our fully expanded term list. One problem at a time.
    (define (element-then-k-zeros element k)
      (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (append result (list 0)))))
      (iter k (list element)))
    (define (build-n-degree-term p n)
      ;L looks like (a_n(x) a_{n-1}(x) ... a_0(x))
      (define (parse-sparse-poly a k)
        (let ((L-a (term-list a)))
          (if (= (order (first-term L-a)) n)
              (mul-poly (make-poly 'y (element-then-k-zeros 1 (+ k 1))) (first-term L-a))
              (parse-sparse-poly (make-poly (variable a) (rest-terms L-a))))))
      (define (parse-dense-poly a k)
        (let ((L-a (term-list a)))
          (if ((= (- (length (rest-terms (term-list L-a))) 1) n))
              (mul-poly (make-poly 'y (element-then-k-zeros 1 (+ k 1))) (first-term L-a))
              (parse-dense-poly (make-poly (variable a) (rest-terms L-a))))))
      (let ((L (term-list p)))
        (let ((ord (- (length L) 1))
              (a (first-term L)))
          ;ord is the power of y.
          (cond (((type-tag a) 'sparse)
                 (add (parse-sparse-poly a ord) (build-n-degree-term (make-poly (variable p) (rest-terms L)))))
                (((type-tag a) 'dense)
                 (add (parse-dense-poly a ord) (build-n-degree-term (make-poly (variable p) (rest-terms L)))))
                (((type-tag a) 'polynomial)
                 (if (eq? (type-tag (contents a)) 'sparse)
                     (add (parse-sparse-poly (contents a) ord) (build-n-degree-term (make-poly (variable p) (rest-terms L))))
                     (add (parse-dense-poly (contents a) ord) (build-n-degree-term (make-poly (variable p) (rest-terms L))))))
                (else
                  (add a (build-n-degree-term (make-poly (variable p) (rest-terms L)))))))))
    ;now, we just need to find the maximum power of x in p(y)'s expansion. For that we can write a method that iterates through the coefficients,
    ;and checks. 
    (define (max-power-var p var)
      (define (max-power-sparse a min-bound)
        (let ((L (term-list a)))
          (let ((t1 (first-term L)))
            (let ((ord (order t1)))
              (if (> ord min-bound)
                  (max ord (max-power-sparse (make-poly var (rest-terms L)) ord))
                  (max min-bound (max-power-sparse (make-poly var (rest-terms L)) min-bound)))))))
      (define (max-power-dense a)
        (- (length (term-list a)) 1))
      (let ((L (term-list p)))
        (let ((a (first-term L)))
          (cond ((eq? (type-tag a) 'sparse)
                 (max (max-power-sparse a 0) (max-power-var (make-poly (variable p) (rest-terms L)) var)))
                ((eq? (type-tag a) 'dense)
                 (max (max-power-dense a 0) (max-power-var (make-poly (variable p) (rest-terms L)) var)))
                ((eq? (type-tag a) 'polynomial)
                 (if (eq? (type-tag (contents a)) 'sparse)
                     (max (max-power-sparse (contents a) 0) (max-power-var (make-poly (variable p) (rest-terms L)) var))
                     (max (max-power-dense (contents a) 0) (max-power-var (make-poly (variable p) (rest-terms L)) var))))
                (else 
                  (max 0 (max-power-var (make-poly (variable p) (rest-terms L)) var)))))))
    (define (build-term-list p i)
      (if (= i 0)
          ()
          (cons (build-n-degree-term p i) (build-term-list p (- i 1)))))
    (let ((max-power (max-power-var p (variable p))))
      (let ((term-list (build-term-list p max-power)))
        (make-poly 'x term-list))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (if (eq? (variable p1) 'y)
            (add-poly (y->x p1) p2)
            (add-poly p1 (y->x p2)))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2))) 
        (if (eq? (variable p1) 'y)
            (mul-poly (y->x p1) p2)
            (mul-poly p1 (y->x p2))))))
;the install-polynomial-package procedure is unchanged, since addition and multiplication at that level are defined recursively by their definitions at the lower levels(in sparse and dense implementations)
;=====================================
;Extended exercise; Rational functions
;=====================================
;Exercise 2.93:
;we know how to handle addition, subtraction, multiplication of polynomials.
(define (add a b)
  (apply-generic 'add a b))
(define (sub a b)
  (apply-generic 'sub a b))
(define (mul a b)
  (apply-generic 'mul a b))
(define (install-rational-package)
  <stuff>
  ;;internal procedures
    (define (make-rat n d)
      (let ((g (greatest-common-divisor n d)))
        (let ((a (div n g))
              (b (div d g)))
           (cons a b))))
  (define (numer x)
    (car x))
  (define (denom x)
    (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
;tested it out on paper, because we dont actually have
;a get and put method, I cant actually run this code... 
;Exercise 2.94:
(define (install-scheme-number-package)
  <stuff>
  (put 'greatest-common-divisor '(scheme-number scheme-number) 
       (lambda (a b) (tag (gcd a b))))
  'done)
;We assume that polynomials are always represented as sparse polynomials,
;and so that all the methods that we've written so far for sparse polynomials
;are tucked under <stuff> below.
(define (install-polynomial-package)
  <stuff>
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-term-list) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        ;build list of terms for result. 
                        (let ((qtnt (make-term new-o new-c)))
                          (let ((rmdr (add-terms L1 (minus (mul-terms qtnt L2)))))
                            (if (< (order (first-term rmdr)) (order t2))
                                ;keep return object consistently a list of two objects,
                                ;the first representing the quotient and the second the remainder.
                                ;the actual final remainder is computer from the final quotient,
                                ;which is being recursively build, here.
                                (list () ())
                                (let ((next-qtnt-c (div (coeff (first-term rmdr)) (coeff t2)))
                                      (next-qtnt-o (- (order (first-term-rmdr)) (order t2))))
                                  (let ((next-qtnt (make-term next-qtnt-o next-qtnt-c)))
                                    (let ((next-rmdr (add-terms rmdr (minus (mul-terms next-qtnt L2)))))
                                      (cons next-qtnt (car (div-terms next-rmdr L2)))))))))))
                  (let ((final-quotient (cons result rest-of-result)))
                    (let ((final-rmdr (add-terms L1 (minus (mul-terms final-quotient L2)))))
                      (list final-quotient final-rmdr)))))))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (mul-terms (make-poly (variable a) (make-term 0 (/ 1 (gen-gcd-for-question a)))) a)
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (pseudoremainder-terms a b)
    (define (integerizing-factor p q)
      (expt (first-term (term-list q)) (+ 1 (- (order (first-term (term-list p))) (order (first-term (term-list q)))))))
    (cadr (div-terms (mul-terms (make-poly (variable a) (make-term 0 (integerizing-factor a b))) a) b)))
  (define (gcd-poly a b)
    (if (eq? (variable a) (variable b))
        (make-poly (variable a) (gcd-terms (term-list a) (term-list-b)))
        (error "Polynomials not in the same variable!": (list (variable a) (variable b)))))
  (put 'greatest-common-divisor '(polynomial polynomial) 
       (lambda (p q) (tag (gcd-poly p q))))
  'done)
;Checked it out on paper. It's the euclidean algorithm. assumed div-terms works, this works.
;I can't actually check if it works until we implement get and put. That is done sometime in the next chapter.
;Exercise 2.95
;a.We get a 1458/13^2 - 2916/13^2 + 1458/13^2, which is the right answer, up to a scalar multiple.
;Also, implemented pseudoremainder-terms and changed gcd-terms to use it.
;b. I will implement a generalized gcd procedure, to make things easy. 
;gcd(a,b,c)=gcd(gcd(a,b) c)
;I implemented it, and moved it to the top of this file. Here is a version useful for these questions, though.
(define (gen-gcd-for-question l)
  (define (rec arg-list)
    (cond ((= (length arg-list) 1) (coeff (car arg-list))) 
          ((= (length arg-list) 2) (gcd (coeff (car arg-list)) (coeff (cadr arg-list))))
          ((> (length arg-list) 2) (gcd (gcd (coeff (car arg-list)) (coeff (cadr arg-list))) (rec (cddr arg-list))))))
  (if (< (length l) 2) 
      (error "You need to provide at least two arguments. You gave:" (length l))
      (rec l)))
;Exercise 2.97:
;a.
;takes a term-list and returns its order. Since a sparse polynomial does not have its list ordered
;according to the other of the terms, we aren't assured that the first term will be of the highest order,
;so we have to find the max value, term by term.
;b
(define (install-scheme-package)
  <stuff>
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  'done)
(define (install-polynomial-package)
  <stuff>
  (define (term-list->order l)
    (define (recursive-checker l running-max)
      (cond ((null? l) running-max)
            ((> (order (first-term l)) running-max)
             (recursive-checker (cdr l) (order (first-term l))))
            (else (recursive-checker (cdr l) running-max))))
    (recursive-checker l 0))
  ;assuming (eq? (variable n) (variable d)) 
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (let ((O2 (term-list->order g))
            (O1 (max (term-list->order n) (term-list->order d))))
        (let ((c (expt (coeff (first-term g)) (+ 1 (- O1 O2)))))
          (let ((c-as-term-list (list (make-term 0 c))))
            (let ((n_ (mul-terms n c-as-term-list))
                  (d_ (mul-terms d c-as-term-list)))
              ;Since by defn g | n_,d_, and since div-terms returns a list whose first coordinate is the quotient and second is the remainder,
              ;we get the following definitions for nn and dd.
              (let ((nn_ (car (div-terms n_ g)))
                    (dd_ (car (div-terms d_ g))))
                ;let gg be the gcd of the coefficients of the numerator and the denominator
                (let ((gg (gen-gcd-for-question (append nn_ dd_))))
                  (let ((gg-as-term-list (list (make-term 0 gg))))
                    (let ((nn (div-terms nn_ gg-as-term-list))
                          (dd (div-terms dd_ gg-as-term-list)))
                      (list nn dd)))))))))))
  (define (reduce-poly p q)
    (if (eq? (variable p) (variable q))
        (let ((term-lists (reduce-term (term-list p) (term-list q))))
          (list (make-poly (variable p) (car term-lists))
                (make-poly (variable q) (cadr term-lists))))
        (error "THE POLYNOMIALS MUST BE IN THE SAME VARIABLE!")))
  (put 'reduce '(polynomial polynomial) reduce-poly)
  'done)
(define (reduce p q)
  (apply-generic 'reduce p q))
(define (install-rational-package)
  <stuff>
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make '(rational)
       (lambda (p q) (tag (make-rat p q))))
  'done)
;=========== 3. Modularity, Objects, and State ============
;=========== 3.1 Assignment and Local State ===============
;=========== 3.1.1 Local State Variables ==================
(define balance 100)
;begin syntax:
;(begin <exp_1> <exp_2> ... <exp_n>)
;explanation: begin evaluates <exp_1> ... <exp_n>, and then has as its return value the return value of <exp_n>.
;set syntax:
;(set! <name> <new-value>)
;explanation: set just sets a new value for the variable <name>.
;the syntax of new-withdraw is different from what we've seen thus far.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds!"))))
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
;the point is that the two are separate objects, with their own local variables.
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!!!!!!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) balance)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)
;Exercise 3.1
(define (make-accumulator a)
  (let ((initial-value a))
    (lambda (b)
      (begin (set! initial-value (+ initial-value b))
             initial-value))))
;Exercise 3.2
(define (make-monitored f)
  (let ((calls 0))
    (define (dispatch m)
      (if (eq? m 'how-many-calls?)
          calls
          (begin (set! calls (+ calls 1))
                 (f m))))
    dispatch))
;Exercise 3.3
;specifying password as a formal argument to make-account makes password a local environment variable inside make-account.
;(define (make-account balance password)
;  (define (call-the-cops)
;    (display "WE ARE NOW CALLING THE POLICE DO NOT MOVE OR YOU WILL POOP!"))
;  (let ((password-attempts 0))
;    (define (withdraw amount)
;      (if (>= balance amount)
;          (begin (set! balance (- balance amount))
;                 balance)
;          "Insufficient funds!!!!!!"))
;    (define (deposit amount)
;      (set! balance (+ balance amount))
;      balance)
;    (define (dispatch p m)
;      (if (eq? p password)
;          (cond ((eq? m 'withdraw) withdraw)
;                ((eq? m 'deposit) deposit)
;                (else (error "Unknown request: MAKE-ACCOUNT" m)))
;          (begin (set! password-attempts (+ password-attempts 1))
;                 (if (>= password-attempts 7)
;                     (call-the-cops)
;                     (lambda (x) "Incorrect password")))))
;          dispatch))
;==========3.1.2. The Benefits of Introducing Assignment==========
;fact: Probability that any two integers, chosen at random, are coprime is 6/pi^2
;We use this fact to estimate pi:
;random-init is some initial integer. rand-update is a well-defined function.
;(define rand (let ((x random-init))
;               (lambda ()
;                 (set! x (rand-update x))
;                 x)))
;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;(define (cesaro-test)
;  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))
;Exercise 3.5

;P represents a predicate in R^2 that defines some jordan closed subset.
(define (random-in-range low high)
  (let ((range (- high low)))
    (if (= range 0)
        (+ low 0)
        (+ low (random range)))))
;(x1 y1) (x2 y2) give us the diagonal of a rectangle containing the region specified by P.
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (exper)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((area-rectangle (* (- x2 x1) (- y2 y1))))
    (* area-rectangle (monte-carlo trials exper))))

;to estimate pi, we just need to check a unit circle centered at zero, so we want:
;x^2+y^2<1, so P is
(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))
(define (pi-estimate trials)
  (estimate-integral in-unit-circle? -1 1 -1 1 trials))
;Exercise 3.6:
;until we implement rand-update, rand-init, we can't use this... 
(define (rand m)
  (let ((x rand-init))
    (define generate
      (lambda ()
        (set! x (rand-update x))
        x))
    (define (reset num)
      (set! x  num))
    (cond ((eq? m 'generate) (generate))
          ((eq? m 'reset) reset)
          (else (error "Only currently supports 'generate and 'reset functionality. You entered:" m)))))
;==================3.1.3. The Costs of Introducing Assignment==================
;Exercise 3.7:
(define (make-account balance . passwords)
  (define (call-the-cops)
    (display "WE ARE NOW CALLING THE POLICE DO NOT MOVE OR YOU WILL POOP!"))
  (let ((password-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds!!!!!!"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (add-password p)
      (set! passwords (append (list p) passwords)))
    (define (dispatch p m)
      (if (or (eq? p passwords) (not (null? (memq p passwords))))
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'add-password) add-password)
                (else (error "Unknown request: MAKE-ACCOUNT" m)))
          (begin (set! password-attempts (+ password-attempts 1))
                 (if (>= password-attempts 7)
                     (call-the-cops)
                     (lambda (x) "Incorrect password")))))
    dispatch))
(define (make-joint acc p p-new)
  (begin ((acc p 'add-password) p-new)
         acc))
;Exercise 3.8
;define a procedure f st (+ (f 0) (f 1)) returns 0, if evaluated from left to right and 1 if evaluated from right to left... 
;Clearly, we need to use assignment...
(define (f n)
  (let ((m 1))
    (begin (set! m (* n m))
           m)))
;left to right: (f 0) sets m to 0, so that (f 1) sets m to zero, and so the sum is 0.
;right to left: (f 1) sets m to 1, so that (f 0) returns zero, and so the sum is 1. 
;==================3.2 The Environment Model of Evaluation==================
;==================3.2.1 The Rules for Evaluation==================
;Procedures are created in one way only: by evaluating a \lambda-expression. 
;  This produces a procedure whose code is obtained from the text of the lambda-expression 
;  and whose environment is the environment in which the lambda expression was evaluated to produce the procedure.
;The environment model of procedure application can be summarized by two rules:
;  -A procedure object is applied to a set of arguments by constructing a frame, binding the formal parameters of the procedure to the arguments of the call, and then evaluating the body
;   of the procedure in the context of the new environment constructed. The new frame has as its enclosing environment the environment part of the procedure object being applied.
;  -A procedure is created by evaluating a \lambda-expression relative to a given environment. The resulting procedure object is a pair consisting of the text of the
;   \lambda-expression and a pointer to the environment in which the procedure was created.
;Evaluating the expression (set! <variable> <value>) in some environment locates the binding of the variable in the environment and changes that binding to indicate the new value.
;That is, one finds the first frame in the environment that contains a binding for the variable and modifies that frame. If the variable is unbound in the environment, then set! signals an error.
;==================3.2.2. Applying Simple Procedures==================
;Exercise 3.9
;done on paper.
;==================3.2.3. Frames as the Repository of Local State==================
;the code in which the lambda expression is evaluated to create the object is the environment to which the procedure points.
;Exercise 3.10:
;(let ((<var> <exp>)) <body>) is syntactic sugar for ((lambda (<var>) <body>) <exp>)
;(define (make-withdraw initial-amount)
;  (let ((balance initial-amount))
;    (lambda (amount)
;      (if (>= balance amount)
;          (begin (set! balance (- balance amount))
;                 balance)
;          "Insufficient funds!"))))
;In order to look at the environment structure generated by this definition, lets substitute out the syntactic sugar:
;(define (make-withdraw initial-amount)
;  ((lambda (balance)
;    (lambda (amount)
;      (if (>= balance amount)
;          (begin (set! balance (- balance amount))
;                 balance)
;          "Insufficient funds!"))) 
;   initial-amount))
;done diddly did the rest on paper. with this definition, when we run (define W1 (make-withdraw 100)), we create an frame.
;==================3.2.4 Internal Definitions==================
;Exercise 3.11:
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
            (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  dispatch)
;Show the environment structure generated by the sequence of interactions
;(define acc (make-account 50))
;((acc 'deposit) 40)
;((acc 'withdraw) 60)
;Will do this on paper...)
;The local states for the two accounts are kept distinct by defining different frames, where balance, and all of the locally defined procedures are bound. 
;(define acc2 (make-account 100)). Then we create a new environment E2 where balance is 100, and evaluate the body of make-account there, 
;creating the three procedure objects whose corresponding environments are E2, and return dispatch. This dispatch is different than the dispatch created when
;evaluating (define acc (make-account 50)). To withdraw, deposit, whatever, we create a new environment where m is set to withdraw, deposit, or whatever, and then
;evaluating (deposit m) there. We return the procedure we want, create a new environment, pointing to the environment that the procedure that we got (withdraw, deposit, or whatever)
;points to, which in the case of acc is E1 and in the case of acc2 is E2, and then evaluate the body of the procedure with the argument given in the amount we wish to withdraw, deposit, 
;or whatever. E1 and E2 are distinct, and so are all intermediary frames created in dispatching, withdrawing, or depositing, so none of the environment structure is shared, besides
;the global environment.
;==================3.3 Modeling with Mutable Data==================
;==================3.3.1 Mutable List Structure==================
;Exercise 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) 
      x 
      (last-pair (cdr x))))
;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;(cdr x) will return (b)
;(define w (append! x y))
;w will return (a b c d)
;(cdr x) is (b) or (cons b ()), which now has been changed by append! to (cons b (cons c (cons d ()))) or (b c d)
;rest was done on paper.
;Exercise 3.13:
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;did it on paper, but its like x_1,->x_2,->...->x_n,->x_1... where a,b represents (cons a b).
;(define z (make-cycle (list 'a 'b 'c)))
;well, we set the cdr of the last pair of ('a 'b 'c), which is (cons 'c ()) to x itself, but the cdr of the last pair of x is now changed to x itself, and so on... so we
;get an eternal loop, like a,->b,->c,->a,->b,->c->..., and so the interpreter can never return any value, since it is eternally computing a value to return!
;Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x ()))
;Let a=(a_1,...a_n). Then (mystery a) works through a locally defined procedure loop. Let us permanently call the first argument to loop x and the second argument to loop y. 
;Loop reverses x iteratively, and keeps track of this reversed list by y. At first we run (loop x ()). We pass () as the second argument because this is the empty list which
;will iteratively be built into the reversed list. At first pass, we save the cdr of x to temp, which is (a_2, ... ,a_n). Then we set the cdr of x to point to our iteratively built
;list y, which at first is just (), so we change x to (cons a_1 ()) or (a_1). Next, we pass temp as x and (a_1) as y. We, again, save the cdr of x (a_3 ... a_n) to temp, and point the
;cdr of x to our iteratively constructed list, so we get (cons a_2 (cons a_1 ()) or (a_2 a_1). We continue this process, until we hit an iteration where x is () and y is (a_n a_{n-1} ... a_1), at which point we return y. The original argument a is changed to (a_1), in the first pass, and is never changed again. After running (mystery a), then, we expect that a is (a_1) and (mystery a) is a reversed, or (a_n a_{n-1} a_1), which answers the next question.
(define v (list 'a 'b 'c 'd))
(define w (mystery v))
;v returns (a) and w returns (d c b a)
;Exercise 3.15: done on paper.
;Exercise 3.16:
;(define (count-pairs x)
;  (if (not (pair? x))
;      0
;      (+ (count-pairs (car x))
;         (count-pairs (cdr x))
;         1)))
;====
;(define z (cons 'c ()))
;(define w (cons 'b z))
;(define x (cons 'a w))
;then (count-pairs x) returns 3.
;====
(define z (cons 'c ()))
(define w (cons z z))
(define x (cons 'a w))
;then (count-pairs x) returns 4.
;===
;(define z (cons 'a ()))
;(define w (cons z z))
;(define x (cons w w))
;then (count-pairs x) returns 7.
;==
;(define z (cons 'c ()))
;(define w (cons 'b z))
;(define x (cons 'a w))
(set-cdr! z x)
;now (make-pair x) is infinite. 
;Exercise 3.17:
;need a procedure that checks to see if current object is equal to object in a list.
(define (eqin? x L)
  (if (null? L) #f
      ;or because we just need one true to get true
      (or (eq? x (car L)) (eqin? x (cdr L)))))
;this program could be improved by checking in the second conditional statement if 
;the car or cdr have already been traversed, as is done in the else statement...
;The procedure as written works in the case of structures containing cyclic graphs...
(define (count-pairs x)
  (let ((pairs ()))
    (define (traverse z)
      (cond ((not (pair? z)) 0)
            ((not (eqin? z pairs))
             (begin (set! pairs (append pairs (list z)))
                    (+ (traverse (car z)) (traverse (cdr z)) 1)))
            ;if it's already been counted, and all possible paths from it have already been counted, then we just return 0.
            (else (cond ((and (pair? (car z)) (not (pair? (cdr z))))
                         (if (eqin? (car z) pairs)
                             0
                             (traverse (car z))))
                        ((and (not (pair? (car z))) (pair? (cdr z)))
                         (if (eqin? (cdr z) pairs)
                             0
                             (traverse (cdr z))))
                        ((and (pair? (car z)) (pair? (cdr z)))
                         (if (and (eqin? (car z) pairs) (eqin? (cdr z) pairs))
                             0
                             (+ (traverse (car z)) (traverse (cdr z)))))
                        (else 0)))))
    (traverse x)))
;Exercise 3.18:
;procedure that examines a list and determines whether it contains a cycle... not too bad.
;idea is to have an external data structure (just a simple list) that keeps track of which nodes have been visited...
;since in the q for this exercise, we are given that the traversal is simply by checking cdrs... we can simply
;return true if we encounter a vertex already in our list and false otherwise.
;if there isn't a cycle, the graph is finite, so we don't have to worry about infinite loops arising! 
(define (contains-cycle? L)
  (let ((pairs ()))
    (define (check-for-cycle li)
      (cond ((not (pair? li)) #f)
            ((not (eqin? li pairs))
             (begin (set! pairs (append pairs (list li)))
                    (or #f (check-for-cycle (cdr li)))))
            (else #t)))
    (check-for-cycle L)))
;Going to cover space and time complexity in depth later (when I get to 3. in the syllabus that I am following), so going to skip this exercise.
;Exercise 3.20:
;done on paper.
;3.3.2. Representing Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define  (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))
;Exercise 3.21:
(define (print-queue queue)
  (display (front-ptr queue)))
;The reason the results are given as they are is that we are printing the pair whose car is a pointer to the sequence of elements in the queue and whose cdr is a pointer to the last pair in the queue.
;This explains why Ben thought that 'the last item is inserted into the queue twice.'
;The reason that when deleting all items in the queue, we still see the last item that was in the queue as the rear-ptr of the queue object is that the delete-queue! procedure
;simply sets the front ptr of the queue to the cdr of the front-ptr of the queue, without changing the rear-ptr of the queue. In the case of there being a queue with 1 element. 
;We have that the front-ptr of the queue is just (cons element ()) and the rear-ptr of the queue is the same thing. Then, when we delete an element. The front-ptr gets
;moved to (cdr (cons element ()))=(), but nothing happens to the rear ptr, so it stays (element). To me, this is a bad way to define the deletion of an element of the queue. We
;should add a condition that checks if (cdr (front-ptr queue)) is null, and if so... the rear-ptr should point to (cdr (front-ptr queue)), as will the front ptr, and we would
;be back to an empty-queue... Anyway, the question doesn't tell me to change the definitions given, merely to write a new print procedure and explain away what is being seen, 
;so I won't adjust the delete-queue! procedure. 
;Just finished Exercise 3.22, and adjusted the delete-queue! procedure to behave as desired!
;Exercise 3.22:
;(define (make-queue)
;  (let ((front-ptr ())
;        (rear-ptr ()))
;    ;We don't return set-front-ptr!, set-rear-ptr! to the user. These are internal setters that will be used by insert-queue! and delete-queue!
;    (define (set-front-ptr! x)
;      (set! front-ptr x))
;    ;x is a pair
;    (define (set-rear-ptr! x)
;      (set! rear-ptr x))
;    (define (empty-queue?)
;      (null? front-ptr))
;    (define (front-queue)
;      (if (not (empty-queue?))
;          (car front-ptr)
;          (error "QUEUE IS EMPTY!")))
;    (define (insert-queue! x)
;      (let ((new-pair (cons x ())))
;        (if (empty-queue?)
;            (begin (set-front-ptr! new-pair)
;                   (set-rear-ptr! new-pair)
;                   front-ptr)
;            (begin (set-cdr! rear-ptr new-pair) 
;                   (set-rear-ptr! new-pair)
;                   front-ptr))))
;    (define (delete-queue!)
;      (if (not (empty-queue?))
;          (if (null? (cdr front-ptr))
;              (begin (set-front-ptr! (cdr front-ptr))
;                     (set-rear-ptr! front-ptr)
;                     front-ptr)
;              (begin (set-front-ptr! (cdr front-ptr))
;                     front-ptr))
;              (error "DELETE! called with an empty queue")))
;      (define (dispatch m)
;        (cond ((eq? m 'front-ptr) front-ptr)
;              ((eq? m 'rear-ptr) rear-ptr)
;              ((eq? m 'empty-queue?) (empty-queue?))
;              ((eq? m 'front-queue) (front-queue))
;              ((eq? m 'insert-queue!) insert-queue!)
;              ((eq? m 'delete-queue!) (delete-queue!))
;              (else (error "Unknown operation!" m))))
;      dispatch))
;Exercise 3.23:)
;The following implementation works. All procedures take \Omega(1) time. 
;Look at deque.jpg to see a picture of this implementation of the deque data structure.
(define (make-deque)
  (cons () ()))
(define (empty-deque? deque)
  (null? (front-deque deque)))
(define (front-deque deque)
  (car deque))
(define (rear-deque deque)
  (cdr deque))
(define (front-insert-deque! deque item)
  (let ((new-pair (cons item ())))
    (cond ((empty-deque? deque)
           (set-car! deque new-pair)
           (set-cdr! deque new-pair)
           (set-cdr! (front-deque deque) (cons (front-deque deque) ())))
          (else 
            (set-cdr! new-pair (cons new-pair ()))
            (set-car! deque (append new-pair (front-deque deque)))
            (set-car! (cdddr (front-deque deque)) (cdr (front-deque deque)))))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item ())))
    (cond ((empty-deque? deque)
           (set-car! deque new-pair)
           (set-cdr! deque new-pair)
           (set-cdr! (front-deque deque) (cons (front-deque deque) ())))
          (else
            (set-cdr! (cdr (rear-deque deque)) (append new-pair (cons (cdr (rear-deque deque)) ())))
            (set-cdr! deque (cddr (rear-deque deque)))))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        ((null? (cddr (front-deque deque)))
         (set-car! deque ())
         (set-cdr! deque ()))
        (else 
          (set-car! deque (cddr (front-deque deque)))
          (set-car! (cdr (front-deque deque)) (front-deque deque)))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        ((null? (cddr (front-deque deque)))
         (set-car! deque ())
         (set-cdr! deque ()))
        ((= (length (front-deque deque)) 4)
         (set-car! (cdr (rear-deque deque)) ())
         (set-cdr! deque (front-deque deque))
         (set-cdr! (cdr (rear-deque deque)) ()))
        (else
          (set-cdr! deque (cdaadr (rear-deque deque)))
          ;the following two lines are so that garbage collector removes the two disconnected last pairs.
          (set-car! (cdddr (rear-deque deque)) ())
          (set-cdr! (cdr (rear-deque deque)) ()))))

;============3.3.3. Representing Tables============
;1d table
;assoc expects a key and a list of records as arguments, and returns the record that has the given key as its car.
;(define (assoc key records)
;  (cond ((null? records) false)
;        ((equal? key (caar records)) (car records))
;        (else (assoc key (cdr records)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))
;2d table
;we construct a 2d table by letting each key point to a subtable... ie
;math: +:43      letters: a:97
;      -:45               b:98
;      *:42
;the subtables dont get a *table* symbol as the first car, but rather just the subtable name.
;drawing a box and pointer graph of the table makes everything clear! 
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                    (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;Exercise 3.24
(define (make-table-1 same-key?)
  (let ((local-table (list '*table*)))
    (define (equ? key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (equ? key (cdr records)))))
    (define (lookup key-1 key-2 local-table)
      (let ((subtable
              (equ? key-1 (cdr local-table))))
        (if subtable
            (let ((record
                    (equ? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value local-table)
      (let ((subtable (equ? key-1 (cdr local-table))))
        (if subtable
            (let ((record (equ? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
;Exercise 3.25:
(define (make-table-generalized)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (table-traverser ks table)
        (let ((key (car ks)))
          (if (null? (cdr ks))
              (let ((record (assoc key (cdr table))))
                (if record
                    (cdr record)
                    #f))
              (let ((subtable
                      (assoc key (cdr table))))
                (if subtable
                    (table-traverser (cdr ks) subtable)
                    #f)))))
      (table-traverser keys local-table))
    (define (insert! keys value)
      (define (make-piece ks v)
        (if (null? (cdr ks))
            (cons (car ks) v)
            (list (car ks) (make-piece (cdr ks) v))))
      (define (table-traverser ks v table)
        (let ((key (car ks)))
          (if (null? (cdr ks))
              (if (not (pair? (cdr table)))
                  (set-cdr! table (list (cons key value)))
                  (let ((record (assoc key (cdr table))))
                    (if record
                        (set-cdr! record value)
                        (set-cdr! table
                                  (cons (cons key value)
                                        (cdr table))))))
              (let ((subtable
                      (assoc key (cdr table))))
                (if subtable
                    (table-traverser (cdr ks) value subtable)
                    (set-cdr! table
                              (cons (make-piece ks value) (cdr table))))))))
      (begin (table-traverser keys value local-table)
             'ok))
    (define (print-table)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup!) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print-table) (print-table))
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
;Exercise 3.26
;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) #f)
;        ((= given-key (car (entry set-of-records))) (entry set-of-records))
;        ((< given-key (car (entry set-of-records)))
;         (lookup given-key (left-branch set-of-records)))
;        ((> given-key (car (entry set-of-records)))
;         (lookup given-key (right-branch set-of-records)))))
;assume we key by integer
(define (make-table-binary)
  (let ((local-table (cons '*table* (cons () (cons () ())))))
    (define (lookup! key)
      (define (recursive-lookup k table)
        (define (key table)
          (car table))
        (define (value table)
          (cadr table))
        (define (left-branch table)
          (car (branches table)))
        (define (right-branch table)
          (cdr (branches table)))
        (cond ((eq? (key table) '*table*)
               (if (null? (left-branch table))
                   #f
                   (recursive-lookup k (left-branch table))))
              ((< k (key table))
               (if (null? (left-branch table))
                   #f
                   (recursive-lookup k (left-branch table))))
              ((> k (key table))
               (if (null? (right-branch table))
                   #f
                   (recursive-lookup k (right-branch table))))
              ((= k (key table))
               (value table))
              (else (error "KEY must be a number!" k))))
      (recursive-lookup key local-table))
    (define (insert! key value)
      ;we don't need more keys than one... If you tell me to insert 5 and nothing is there, that will become the root.
      ;Then if you say 6, I make a right branch with key 6. If you tell me 3, I make a left branch from 5.
      ;If you tell me 9 I make a right branch from 6, if you tell me 7 I make a left branch to 9, and so on. 
      (define (recursive-insert k v table)
        (define (key table)
          (car table))
        (define (value table)
          (cadr table))
        (define (set-value! table v)
          (set-car! (cdr table) v))
        (define (branches table)
          (cddr table))
        (define (left-branch table)
          (car (branches table)))
        (define (right-branch table)
          (cdr (branches table)))
        (define (set-left-branch! table v)
          (set-car! (branches table) v))
        (define (set-right-branch! table v)
          (set-cdr! (branches table) v))
        (define (make-piece k v)
          (cons k (cons v (cons () ()))))
        (cond ((eq? (key table) '*table*)
               (if (null? (left-branch table))
                   (set-car! (branches table) (make-piece k v))
                   (recursive-insert k v (left-branch table))))
              ((< k (key table))
               ;check if there is a left branch, if there is run recursive-insert on it, if not, create it and set its key value to (k v)
               (if (not (null? (left-branch table)))
                   (recursive-insert k v (left-branch table))
                   (set-left-branch! table (make-piece k v))))
              ((> k (key table))
               (if (not (null? (right-branch table)))
                   (recursive-insert k v (right-branch table))
                   (set-right-branch! table (make-piece k v))))
              ((= k (key table))
               (set-value! table v))
              (else (error "Please make sure KEY is a number. You used KEY" k))))
      (begin (recursive-insert key value local-table)
             'ok))
    (define (print)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup!) lookup!)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
;Exercise 3.27
;for this exercise use the definitions
;(define (make-table)
;  (list '*table*))
;(define (lookup key table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (cdr record)
;        false)))
;(define (insert! key value table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (set-cdr! record value)
;        (set-cdr! table
;                  (cons (cons key value)
;                        (cdr table)))))
;  'ok)
;done on paper
;If we just draw out the tree of calls to memo-fib, and evaluate from left to right, we figure it out quite easily. First, insert and lookup are O(n), so let k_1, k_2 \in \mathcal{R} be such that
;T(insert n)<k_1n and T(lookup n)<k_2n. Then we have, by following the calls in our tree diagram, that T(n)=T(insert n)+T(lookup n-2)<k_1n+k_2(n-2)=(k_1+k_2)n -2k_2<(k_1+k_2)n, for all n>0,
;so that T(n) \in O(n)! 
;=============3.3.4 A Simulator for Digital Circuits=============
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
;primitive function boxes
;(get-signal <wire>)
;  returns the current value of the signal on the wire.
;(set-signal! <wire> <new-value>)
;  changes the value of the signal on the wire to the new value.
;(add-action! <wire> <procedure of no arguments>)
;  asserts that the designated procedure should be run whenever the signal on the wire changes value.
;  Such procedures are the vehicles by which changes in the signal value on the wire are
;  communicated to other wires.
;In addition, we will make use of a procedure after-delay that takes
;a time delay and a procedure to be run and executes the given procedure after
;the given delay.
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-or s t)
  (if (or (= s 1) (= t 1))
      1
      0))
(define (logical-and s t)
  (if (and (= s 1) (= t 1))
      1
      0))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
;Exercise 3.28:
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
;Exercise 3.29:
; $p \lor q \equiv \lnot (\lnot p \land \lnot q)$, so
;(define (or-gate a1 a2 output)
;  (let ((w1 (make-wire))
;        (w2 (make-wire))
;        (w3 (make-wire)))
;    (inverter a1 w1)
;    (inverter a2 w2)
;    (and-gate w1 w2 w3)
;    (inverter w3 output))
;  'ok)
;eXERCIse 3.30:
;l1 is the list of the a_k, l2 of the b_k, and l3 of the s_k. c is the first carry wire. 
;(define (full-adder a b c-in sum c-out)
;  (let ((s (make-wire))
;        (c1 (make-wire))
;        (c2 (make-wire)))
;    (half-adder b c-in s c1)
;    (half-adder a s sum c2)
;    (or-gate c1 c2 c-out)
;    'ok))
;assume that (length l1) = (length l2) = (length l3), and that the li all contain only wires.
(define (ripple-carry-adder l1 l2 l3 c)
  (define (make-wire-list n)
    (define (iter product index)
      (if (= index 0)
          product
          (iter (cons (make-wire) product) (- index 1))))
    (iter () n))
  (define (make-ripple-carry-adder-circuit lak lbk lsk lck cdefault)
    (cond ((null? lak)
           'done)
          (else 
            (full-adder (car lak) (car lbk) cdefault (car lsk) (car lck))
            (make-ripple-carry-adder-circuit (cdr lak) (cdr lbk) (cdr lsk) (cdr lck) (car lck)))))
  (let ((lc (make-wire-list (length l1))))
    (make-ripple-carry-adder-circuit l1 l2 l3 lc c)))
;Let the delay of a logical operation be denoted by $d_{<op>}$ for operation $op$, and let d be
;the delay of ripple-carry-adder. Then we have
;$d = n (4d_{and} + 3d_{or} + 2d_{not})$.
;==Representing wires==
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
;Agenda syntax
;(make-agenda returns a new empty agenda
;(empty-agenda? <agenda>) is true if the specified agenda is empty.
;(first-agenda-item <agenda>) returns the first item on the agenda.
;(remove-first-agenda-item! <agenda>) modifies the agenda by removing the first item.
;(add-to-agenda! <time> <action> <agenda>) modifies the agenda by adding the given action procedure 
;to be run at the specified time.
;(current-time <agenda>) returns the current simulation time.
;==Implementing the agenda==
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
;the agenda itself is a one-dimensional table of time segments. Current time (the time of the last action that was processed) is stored 
;at the head of the agenda.
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;==A sample simulation==
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value= ")
                 (display (get-signal wire)))))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;Exercise 3.31:
;suppose we had defined
;(define (accept-action-procedure! proc)
;  (set! action-procedures
;    (cons proc action-procedures)))
;  (add-action! input invert-input) 
;  'ok)
;Our program would have bugs, in this case.
;Say we run
;(set-value! a 1).
;then (or-action-procedure) would run on a, and d would be set to 1. 
;(and-action-procedure) would run on a, and c would remain 0.
;(invert-input) would not run, as c is unchanged, and so e would remain 0.
;(and-action-procedure) would run on d, and s would remain 0.
;Thus s, after changing a to 1 has value 0, as does c, which is not what we want!
;This bug is introduced because we didn't invert the value of e from the start,
;i.e. because accept-action-procedure! didn't run the proc once when adding it to wires c e...
;If it was run, then e would have been set to 1 at the beginning, so that the circuit all together would have produced the right answer.
;Values of wires are initialized to 0, so when setting up a circuit, we must run the procedures which we add to the wires so that
;the initial value of the circuit is correct. Then, changing those values by means of changing currents to the wires of the input of the circuit
;will propagate correctly throughout the circuit! Thus it is necessary to make sure to run the procedures added to wires immediately. 
;Exercise 3.32:
;(define w1 (make-wire))
;(set-signal! w1 0)
;(define w2 (make-wire))
;(set-signal! w2 1)
;(define w3 (make-wire))
;(probe 'w1 w1)
;(probe 'w2 w2)
;(probe 'w3 w3)
;(and-gate w1 w2 w3)
;Let w1, w2, w3 be connected to an and-gate as the line above this one shows.
;In our implementation, to change w1's signal to 1 we call (set-signal! w1 1), which calls each of the procedures on the wire,
;particularly we run (and-action-procedure), which computes new-value as (logical-and (get-signal w1) (get-signal w2)) as 1,
;and then adds (lambda () (set-signal! output new-value)) at the end of the queue at time 3. 
;When we then change the signal of w2 to 0, we, again, run (and-action-procedure), this time called from w2, which computes
;new-value as (logical-and (get-signal w1) (get-signal w2)), this time as 0. We add in our agenda at time 3
;(lambda () (set-signal! output new-value)). Running propagate, then, we remove and run the first entry of the first segment of our agenda,
;in our case there is only one segment, with time 3. The first procedure to be run and removed is (lambda () (set-signal! output new-value)) in a frame 
;where new value is computed as 1. Thus we set the signal of output to 1. `propagate` recursively calls itself. 
;Again, we remove the first entry of the queue of the first segment and run it. This time, we set output to 0. In the end, output is 0, as required.
;Now suppose we organized the actions in the cdr of the segments not as queues, but as ordinary lists, where we insert from the left and remove from the left. 
;Then the first entry would be where new-value is computed as 1 and the second entry would be where new-value is computed as 0. We would remove the first entry,
;and run (lambda () (set-signal! output new-value)), setting the signal of output to 0. Then, we would again remove the first entry and run it.
;We would be running (lambda () (set-signal! output new-value)) with new-value now computed as 1, setting the signal of output to 1. The agenda would now be empty,
;so propagate would return 'done, and our final computed signal value for output would be 1. We would have an and-gate with inputs 1 and 0 and output 1, which
;is incorrect! Hence, we can't simply use a list, we need to use something like a queue. Values need to be assigned in the order that the changes were made, that is
;first in first out! If we make a change and then another change, the consequence of the first change must be computed, THEN the consequence of the second change,
;and this is precisely what a queue does! 
;===============Propagation of Constraints===============
;(define C (make-connector))
;(define F (make-connector))
;(define (celsius-fahrenheit-converter c f)
;  (let ((u (make-connector))
;        (v (make-connector))
;        (w (make-connector))
;        (x (make-connector))
;        (y (make-connector)))
;    (multiplier c w u)
;    (multiplier v x u)
;    (adder v y f)
;    (constant 9 w)
;    (constant 5 x)
;    (constant 32 y)
;    'ok))
;(has-value? <connector>) tells whether the connector has a value
;(get-value <connector>) returns the connector's current value.
;(set-value! <connector> <new-value> <informant>) indicates that the informant is requesting the connector to set its value to the new value.
;(forget-value! <connector> <retractor>) tells the connector that the retractor is requesting it to forget its value.
;(connect <connector> <new-constraint>) tells the connector to particpate in the new constraint.
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)
(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
;Exercise 3.33
;We want our constraint to be .5(a+b)=c, using adder and multiplier constraints as primitives...
(define (averager a b c)
  (let ((w (make-connector))
        (u (make-connector)))
    (adder a b u)
    (multiplier u w c)
    (constant (/ 1 2) w)
    'ok))
;Exercise 3.34
;(define (squarer a b)
;  (multiplier a a b))
;a flaw in this program is that we can set b to be 5, and we wouldn't have enough information to propagate values to a,
;so we can then set a to be 6, and we would have the squarer constraint allow a to be 6 and b to be 5,
;which breaks the squarer constraint.
;Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a 
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (if (= (get-value a) 0)
                (set-value! b 0 me)
                (set-value! b (* (get-value a) (get-value a)) me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request) 
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
;Exercise 3.36:
;done on paper
;Exercise 3.37: 
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
;=========3.4 Concurrency: Time is of the Essence=========
;Exercise 3.38:
;a.
;  1. Peter,Paul,Mary:
;    (set! balance (+ balance 10))
;    (set! balance (- balance 20))
;    (set! balance (- balance (/ balance 2)))
;    The final value of balance is 45.
;  2. Peter, Mary, Paul:
;    (set! balance (+ balance 10))
;    (set! balance (- balance (/ balance 2)))
;    (set! balance (- balance 20))
;    The final value of balance is 35
;  3. Mary, Peter, Paul:
;    (set! balance (- balance (/ balance 2)))
;    (set! balance (+ balance 10))
;    (set! balance (- balance 20))
;    The final value of balance is 40
;  4. Mary, Paul, Peter:
;    Since addition is commutative, this will be the same answer as in case 3, i.e. balance will be 40
;  5. Paul, Peter, Mary:
;    Since addition is commutative, this will be the same as in Case 1, i.e. balance will be 45.
;  6. Paul, Mary, Peter
;    (set! balance (- balance 20))
;    (set! balance (- balance (/ balance 2)))
;    (set! balance (+ balance 10))
;    The final value of balance is 50.
;b.
;  Done on paper.
;  One possibility is that all three access the balance variable at roughly the same time, and Paul and Mary both take out 100 at roughly the same time, both setting the balance variable to 0,
;  but then that Peter decides to take out nothing, and finally sets the balance variable to 100.
;  In the end, Paul and Mary would have taken out 100 each, and the final bank balance would be 100.
;===================3.4.2 Mechanisms for Controlling Concurrency===================
;Exercise 3.39:
;(define x 10)
;(define s (make-serializer))
;(parellel-execute
;  (lambda () (set! x ((s (lambda () (* x x))))))
;  (s (lambda () (set! x (+ x 1)))))
;We can either get a final value of x of 100 101 or 121.
;Let the first argument to parallel-execute be p1 and the second p2.
;If p1 runs then p2, we end up with 101.
;If p1 runs to the serialized sub-procedure, then stops and runs p2 all the way through,
;then continues, we get 100.
;If p2 runs all the way through, then p1 runs all the way through, we get 121.
;110 cannot happen, since (lambda () (* x x)) is serialized and thus cannot be interrupted.
;11 can't happen because we can't leave p2 to run (lambda () (* x x)),
;because the two are serialized.
;Exercise 3.40:
;(define x 10)
;(parallel-execute (lambda () (set! x (* x x)))
;                  (lambda () (set! x (* x x x))))
;Let p1 be the first argument to parallel-execute and p2 the second.
;p1->p2: x is 10^6
;p2->p1: x is 10^6
;p1-access-x->p2-access-x->p2-set-x->p1-set-x: x is 10^2
;p2-access-x->p1-access-x->p1-set-x->p2-set-x:10^3
;p1-access-x->p1-first-x-arg-to-*-referenced->p2-access-and-set-x->p2-second-x-arg-to-*-referenced:x is 10*1000=10000=10^4
;p2-access-x->p2-first-x-arg-to-*-reference->p1-access-and-set-x->p2-rest-of-args-to-*-referenced:x is 10*100*100=10^5
;p2-access-x->p2-first-and-second-x-arg-to-*-referenced->p1-access-and-set-x->p2-third-x-arg-to-*-referenced:x is 10*10*100=10^4
;(define x 10)
;(parallel-execute (s (lambda () (set! x (* x x))))
;                  (s (lambda () (set! x (* x x x)))))
;Then only p1->p2 and p2->p1 can occur, which in both cases produces the answer of 10^6.
;Exercise 3.41:
;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance
;                 (- balance amount))
;               balance)
;        "Insufficient funds"))
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance)
;  (let ((protected (make-serializer)))
;    (define (dispatch m)
;      (cond ((eq? m 'withdraw) (protected withdraw))
;            ((eq? m 'deposit) (protected deposit))
;            ((eq? m 'balance)
;             (protected
;               (lambda () balance)));serialized
;            (else
;              (error "Unknown rquest: MAKE-ACCOUNT"
;                     m))))
;    dispatch))
;We don't need to serialize balance. 
;Assuming accessing and returning the local variable `balance` is just one step, no. 
;Let's look at the various different requirements on concurrency which one may use,
;and check whether or not make-account, without balance serialized, meets those requirements.
;Requirement 1: "No two operations that change any shared state variables can occur at the same time."
;  Okay, well balance doesn't set balance, it only gets it. Thus, as the program was written,
;  we needn't have serialized balance. 
;Requirement 2: 
;  a. "Processes don't have to run sequentially, but they must behave as if they did."
;  b. "There may be more than one possible 'correct' result produced by the same concurrent program.
;    Well, assuming that returning balance just takes one step. The only possibilities are that
;    balance is returned before withdraw/deposit, during the execution of withdraw/deposit,
;    or after the execution of withdraw/deposit. 
;    If before, then we are good.
;    If after, we are good.
;    If during, either balance is returned before or after the new value is set, in any case
;    the behavior is as if balance was executed before or after!
;    Thus, we are good by this requirement too.
;    Hence, we needn't have serialized balance! 
;Exercise 4.42
;The change is safe to make. There is no difference in what concurrency is allowed by the two versions.
;In the first version, we add elements to the serialized set as we dispatch, and if these aren't bound
;in one of the enclosing environments, they are lost. So we temporarily add elements to the serialized set.
;In the second case, we permanently add two elements to the serialized set. In any case, what is in the set
;cannot be run concurrently. In the first case, then, the dispatched protected procedures can never be run
;concurrently. In the second case, the two permanent members of the serialized set cannot be run concurrently. 
;The second case is a more efficient solution, with the exact same behavior. 
;===================Complexity of using multiple shared resources===================
(define (exchange acc1 acc2)
  (let ((difference (- (acc1 'balance) (acc2 'balance))))
    ((acc1 'withdraw) difference)
    ((acc2 'deposit) difference)))
;An example from the book of exchanging three balances can go wrong is the following:
;  Suppose Peter and Paul have access to acc1 acc2 and acc3.
;  Peter might want to exchange the balances in acc1 and acc2.
;  The exchange procedure above computes different before setting the balances.
;  In the time taken to compute the difference, Paul can change the balance of acc1.
;  Then, the exchange would be inaccurate. Thus serializing withdraw and deposit as we've done
;  is insufficient to prevent errors to do with concurrency.
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1 account2)))
;Exercise 3.43 
;Let's say we have n exchanges between the three accounts. Let's denote an exchange of balances between account i and j by e_{ij}
;e_{a_1 b_1} e_{a_2 b_2}...e_{a_n b_n} e_{a_n b_n}.
;Since on all three accounts withdrawals and deposits are serialized,
;for each account there exists a sequence of deposits and withdrawals such that executing this sequence produces the same result as the concurrent execution of exchanges above.
;We have the following sequences:
;((w_11 d_11) (w_12 d_12) ... (w_1n d_1n))
;((w_21 d_21) (w_22 d_22) ... (w_2n d_2n))
;((w_31 d_31) (w_32 d_32) ... (w_3n d_3n))
;
;Since each withdrawal out of one account is a deposit in another, we can rearrange the sequences such that each column forms a bipartite graph with a perfect matching.
;WLOG, let us assume that the sequences above are already arranged in such an order. Then, column i
;(w_1i d_1i)
;(w_2i d_2i)
;(w_3i d_3i)
;The only possibilities of such a matching such that the withdrawals and deposits aren't in the same account are:
;w_1i = d_3i , w_2i = d_1i, and w_3i=d_2i and w_1i = d_2i, w_2i=d_3i, and w_3i=d_1i.
;Since
;  (balance_1 + d_1i - w_1i) + (balance_2 + d_2i - w_2i) + (balance_3 + d_3i - w_3i) =
;  (balance_1 + balance_2 + balance_3) + (d_1i + d_2i+d_3i) - (w_1i + w_2i + w_3i),
;we see in both cases that we can rearrange the last two terms such that they all cancel out. Thus,
;the sum of balances are constant. 
;Since this is true for each column, we conclude that in general the sum of balances is constant.
;This argument relies crucially on serialization, so that if we remove this assumption we find that it isn't true that the sum of the balances is preserved.
;For example,
;Suppose that we run
;(parallel-execute (lambda () (exchange acc1 acc2)) (lambda () (exchange acc3 acc1)))
;where acc1 has 5 dollars, acc2 has 10, and acc3 has 15.
;Then, suppose as the first exchange is executing the withdrawal of 5 dollars from acc1, but before setting the new balance,
;the second exchange deposits 10 dollars into the account. 
;Then the first exchange withdrawal procedure completes, and the balance is set to 0.
;Then the final sum would be 0+10+5+15-10=20, whereas it should be 5+10+15=30.
;I drew a timing diagram on a paper to illustrate this
;Exercise 3.44:
;(define (transfer from-account to-account amount)
;  ((from-account 'withdraw) amount)
;  ((to-account 'deposit) amount))
;Louis Reasoner is wrong. Both of the operationsin transfer are serialized. The difference with the exchange procedure 
;is that computing difference was not serialized, and so the difference could be computed in a time period within which
;deposits or withdrawals could be made, thus rendering the difference wrong. When that difference was deposited into the
;second argument to exchange from the first argument to exchange, you would thus end up with an incorrect sequence of balances.
;Exercise 3.45:
;(define (serialized-exchange acc1 acc2)
;  (let ((serializer1 (acc1 'serializer))
;        (serializer2 (acc2 'serialized)))
;    ((serializer1 (serializer2 exchange))
;     acc1
;     acc2)))
;(define (exchange acc1 acc2)
;  (let ((difference (- (acc1 'balance) (acc2 'balance))))
;    ((acc1 'withdraw) difference)
;    ((acc2 'deposit) difference)))

;(define a1 (make-account))
;((a1 'deposit) 20)
;(define a2 (make-account))
;((a2 'deposit) 100)
;(serialized-exchange a1 a2)
;((serializer1 (serializer 2 exchange)) a1 a2)
;Let s_1 be serializer1 and s_2 be serializer 2.
;Let e_{s_1 s_2} be the doubly serializer exchange procedure. 
;We have that a1 has serialized deposit with the first serializer, and a2 with the second.
;Then, we have:
;(e'' a1 a2)
;(let ((difference (- (acc1 'balance) (acc2 'balance))))
;  ((acc1 'withdraw) difference)
;  ((acc2 'deposit) difference))
;We are currently running e_{s_1 s_2}, and so since the local withdrawal procedure of acc1 is serialized,
;we can't run it until exchange terminates, but exchange can't terminate until the procedure is run,
;so the program never terminates.
;=========== Implementing Serializers ===============
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell #f))
(define (test-and-set! cell)
  (if (car cell) 
      #t 
      (begin 
        (set-car! cell #t) 
        #f)))
;Exercise 3.46
;I did this on paper
;Exercise 3.47
;a

;we can think of a semaphore as a deque of n mutexes. an acquire operation takes the first mutex, acquires it, then puts it at the end of the deque.
;release operation takes the last mutex of the deque, releases it, then puts it at the beginning of the deque. 
;mutex generalization implementation
(define (make-semaphore-1 n)
  (define (make-mutex-deque n)
    (let ((deque (make-deque)))
      (define (iter index)
        (if (= index 0)
            deque
            (begin (front-insert-deque! deque (make-mutex))
                   (iter (- index 1)))))
      (iter n)))
  (let ((mutex-deque (make-mutex-deque n)))
    (define (acquire)
      (let ((front-mutex (car (front-deque mutex-deque))))
        (front-mutex 'acquire)
        (front-delete-deque! mutex-deque)
        (rear-insert-deque! mutex-deque front-mutex)))
    (define (release)
      (let ((rear-mutex (car (rear-deque mutex-deque))))
        (rear-mutex 'release)
        (rear-delete-deque! mutex-deque)
        (front-insert-deque! mutex-deque rear-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "SEMAPHORE operation not supported!:" m))))
    the-semaphore))
;atomic test-and-set! operations implementation
;we can use the deque structure as above, but with cells.
(define (make-semaphore-2 n)
  (define (make-cell-deque n)
    (let ((deque (make-deque)))
      (define (iter index)
        (if (= index 0)
            deque
            (begin (front-insert-deque! deque (list #f))
                   (iter (- index 1)))))
      (iter n)))
  (let ((cell-deque (make-cell-deque n)))
    (define (acquire)
      (let ((front-cell (car (front-deque cell-deque))))
        (if (test-and-set! front-cell)
            (acquire)
            (begin (front-delete-deque! cell-deque)
                   (rear-insert-deque! cell-deque front-cell)))))
    (define (release)
      (let ((rear-cell (car (rear-deque cell-deque))))
        (clear! rear-cell)
        (rear-delete-deque! cell-deque)
        (front-insert-deque! cell-deque rear-cell)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "SEMAPHORE operation not supported!:" m))))
    the-semaphore))
;==============DEADLOCK==============
;Exercise 3.48
(define (make-account-i n balance)
  (let ((id n))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'id) id)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch)))
(define (serialized-exchange-id account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (cond ((= id1 id2)
           'done)
          ((< id1 id2)
           (serializer2 (serializer1 (exchange acc1 acc2)))
           'done)
          ((< id2 id1)
           (serializer1 (serializer2 (exchange acc2 acc1)))
           'done))))
;Exercise 3.49:
;After being stuck trying to find examples of this, and realizing that each example was just another variant of an exchange problem,
;I looked online for solutions. One solution that I found was database mutations, and another a particular example from another textbook.
;Thus, it seems like this question will be better answered when I know a little more CS. I happily skip over it to the remainder of the book.
;=====================3.5 Streams=====================
;===3.5.1 Streams are Delayed Lists===
;stream analogs of list operations:
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr S)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))
;syntax of streams:
;  (cons-stream <a> <b>) is equivalent to (cons <a> (delay <b>))
;  (define (stream-car stream) (car stream))
;  (define (stream-cdr stream) (force (cdr stream)))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
;naive delay implementation:
;(define (delay exp)
;  (lambda () exp))
;force syntax:
;(define (force delayed-object)
;  (delayed-object))
;This can be made more efficient using memoization:
(define (memo-proc proc)
  (let ((already-run? #f) 
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
;can implement delay and force by:
;(define (delay proc)
;  (memo-proc (lambda () proc)))
;(define (force proc)
;  (proc))
;we then define delay so that (delay <exp>) is equivalent to
;(memo-proc (lambda () <exp>))
;Exercise 3.50:
(define (stream-map-1 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map-1
               (cons proc (map stream-cdr argstreams))))))
;Exercise 3.51:
;(define (show x)
;  (display-line x)
;  x)
;(define x 
;  (stream-map show
;              (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)
;done on paper
;Exercise 3.52:
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
;seq is {1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210}
(define y (stream-filter even? seq))
;y is {6 10 28 36 66 78 120 136 190 210}
;sum is 6.
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
;z is {10 15 45 55 105 120 190 210}
;sum is 10.
;(stream-ref y 7)
;136
;====================3.5.2 Infinite Streams====================
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones
  (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream 
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
;Exercise 3.53;
;defines the sequence {2^n}_{n=0}^{\infty}.
;Exercise 3.54:
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
;Exercise 3.55:
(define (partial-sums S)
  (cons-stream (stream-car S) (add-streams (scale-stream ones (stream-car S)) (partial-sums (stream-cdr S)))))
;Exercise 3.56:
;merge just implements an ordered set union, if we look at the streams as totally ordered sets. 
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream
                     s1car
                     (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream
                     s2car
                     (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
;Exercise 3.57:
;done on paper.
;Let's start our counting of the fib numbers from 0, and let A(n) be the number of additions to compute the nth fib number.
;Then in the case where we memoize, A(n)=n-1. In the case where we don't A(n)=\sum_{k=1}^{n-1}F(k), where F(k) is the kth fib number.
;Exercise 3.58:
(define (expand num den radix)
  (cons-stream 
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
;stream of quotients q_i produced in the following:
;  (num*radix) = q_0*den + r_0
;  (r_0*radix) = q_1*den + r_1
;  (r_1*radix) = q_2*den + r_2
;(expand 1 7 0): 
;  {0 0 0 0 ...}
;(expand 3 8 10):
;  (3*10) = 8q_0 + r_0 or
;      30 = 8q_0 + r_0 =
;         = 8* 3 + 6, so q_0  is 3.
;
;  (6*10) = 8*q_1 + r_1 or
;      60 = 8*  7 + 4, so q_1 is 7.
;
;  (4*10) = 8*5 + 0, so q_2=4
;
;  the rest of the stream will just be zero, since the remainder is zero.
;
;  Thus the result is
;
;  {3 7 5 0 0 0 0 0 0 ...}

;Exercise 3.59:
;a:
(define (integrate-series series)
  (define (builder i s)
    (cons-stream (/ (stream-car s) i) (builder (+ i 1) (stream-cdr s))))
  (builder 1 series))
;b:
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cose-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cose-series)))
;Exercise 3.60:
;pretty ugly implementation imo, but I was trying to find one that fit into the outline given by the textbook of:
;(define (mul-series s1 s2)
;  (cons-stream <??> (add-streams <??> <??>)))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2)) (mul-series s1  (stream-cdr s2)))))
;Exercise 3.61:
(define (invert-unit-series S)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr S) (invert-unit-series S)) -1)))

;Exercise 3.62:
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Cannot divide series which begins with zero!")
      (mul-series s1 (invert-unit-series s2))))
(define tan-series
  (div-series sine-series cose-series))

;======================== 3.5.3 Exploiting the Stream Paradigm ========================
;Exercise 3.63
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
;make-tableau defines a sequence of transformed sequences, each one converging faster than the one before.
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
(define (sqrt-stream-1 x)
  (cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) (sqrt-stream-1 x))))
;What happens is if you have already computed (stream-ref ss n), then the delayed cdrs will be saved, and so 
;the next time you run (stream-ref ss n), it is a constant time calculation, but if you want to do
;(stream-ref ss n+1), then we will have to run 
;(stream-map (lambda (guess) (sqrt-improve guess 2)) (stream-cd^nr (sqrt-stream-1 2)))
;but sqrt-stream-1 was not saved to a variable, as it is defined, and so all n of the previous results must now be recomputed,
;before finally returning the n+1th. Thus we get that sqrt-stream-1 takes O(n) time. 
;on the other hand, evaluating (stream-ref s n) and then (stream-ref s n+1) makes us evaluate
;(stream-map (lambda (guess) (sqrt-improve guess x)) guesses)
;but now guesses is a local variable that has all of the previous cdr traversals saved, so it just needs to compute one extra step. 
;If we change our implementation of delay to just (lambda () <exp>) then there will be no advantage to saving guesses as a local variable,
;since the cdr traversals are not saved...
;Exercise 3.64:
(define (stream-limit s r)
  (let ((s0 (stream-car s))
        (s1 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s0)) r)
        s1
        (stream-limit (stream-cdr s) r))))
(define (sqrtt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
;Exercise 3.65:
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
  (partial-sums (ln-summands 1)))
;---------INFINITE STREAMS---------
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
;Exercise 3.66:
(define ip (pairs integers integers))
;Analyzing ip, I came up with the following function for the number of pairs before (i j):
(define (before-pair pair)
  (let ((i (car pair)) (j (cadr pair)))
    (if (> i j)
        (error "The first element of the pair must be less than or equal to the second!")
        (if (= i j)
            (- (expt 2 i) 2)
            (+ (* (- j i 1) (expt 2 i)) (* 3 (expt 2 (- i 1))) -2)))))
;using this formula, we can get all of the answers
;Exercise 3.67:
;We need zero and negative integers.
;want (pairs integers integers) to return all integers, by mixing in an additional stream... so... cant rewrite whole procedure, just gotta adjust the one that exists slightly...
;also, integers are defined as not including zero, so... the definition given below assumes that integers are {1 2 3 4 5 ...}, as we've implemented them.
(define (pairs-1 s t)
  (let ((neg-s (scale-stream s -1))
        (neg-t (scale-stream t -1)))
    (cons-stream
      (list (stream-car s) (stream-car t))
      (interleave
        (interleave 
          (stream-map (lambda (x) (list (stream-car s) x))
                      (stream-cdr t))
          (pairs (stream-cdr s) (stream-cdr t)))
        (cons-stream 
          (list (stream-car s) (stream-car neg-t))
          (cons-stream 
            (list (stream-car neg-s) (stream-car t))
            (cons-stream
              (list (stream-car neg-s) (stream-car neg-t))
              (interleave
                (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s))
                (interleave
                  (stream-map (lambda (x) (list x (stream-car neg-t)))
                              (stream-cdr neg-s))
                  (interleave
                    (stream-map (lambda (x) (list x (stream-car neg-t)))
                                (stream-cdr s))
                    (interleave
                      (stream-map (lambda (x) (list x (stream-car t)))
                                  (stream-cdr neg-s))
                      (interleave
                        (stream-map (lambda (x) (list (stream-car neg-s) x))
                                    (stream-cdr neg-t))
                        (interleave 
                          (stream-map (lambda (x) (list (stream-car s) x))
                                      (stream-cdr neg-t))
                          (stream-map (lambda (x) (list (stream-car neg-s) x))
                                      (stream-cdr t)))))))))))))))
;Exercise 3.68
(define (pairs-2 s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs-2 (stream-cdr s) (stream-cdr t))))
;It doesn't work. The cons-stream in our first definition causes the cdr's evaluation to be delayed, but as Louis defined it, the program tries evaluates the arguments before interleave is applied to them,
;but that leads to an infinite loop, as (stream-cdr s), (stream-cdr t) will never end...
;Exercise 3.69:
(define (triples s t u)
  (let ((txu (pairs t u)))
    (cons-stream
      (append (list (stream-car s)) (stream-car txu))
      (interleave
        (stream-map (lambda (p) (append (list (stream-car s)) p))
                    (stream-cdr txu))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))
(define iiints (triples integers integers integers))
(define pythogorean-triples
  (stream-filter (lambda (p) (= (+ (square (car p))
                                   (square (cadr p)))
                                (square (caddr p)))) iiints))
;Exercise 3.70:
(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (w s1car) (w s2car))
                   (cons-stream
                     s1car
                     (merge-weighted (stream-cdr s1) s2 w)))
                  ((> (w s1car) (w s2car))
                   (cons-stream
                     s2car
                     (merge-weighted s1 (stream-cdr s2) w)))
                  (else
                    (cons-stream s1car 
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1)
                                                              (stream-cdr s2) w)))))))))
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
(define (weighted-pairs s t w)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) w)
      w))) 

;a.
(define (w1 p)
  (let ((i (car p))
        (j (cadr p)))
    (+ i j)))
(define iints1
  (weighted-pairs integers integers w1))
;b.
(define (w2 p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define iints2 
  (stream-filter (lambda (p)
                   (and (not (or (= (remainder (car p) 2) 0) (= (remainder (car p) 3) 0) (= (remainder (car p) 5) 0)))
                        (not (or (= (remainder (cadr p) 2) 0) (= (remainder (cadr p) 3) 0) (= (remainder (cadr p) 5) 0)))))
                 (weighted-pairs integers integers w2)))
;Exercise 3.71:
(define (w3 p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (cube i) (cube j))))
(define iints3 
  (weighted-pairs integers integers w3))
(define (generate-ram nums weight)
  (if (= (weight (stream-car nums)) (weight (stream-car (stream-cdr nums))))
      (cons-stream (stream-car nums)
                   (generate-ram (stream-cdr (stream-cdr nums)) weight))
      (generate-ram (stream-cdr nums) weight)))
(define ramanujan-numbers
  (generate-ram iints3 w3))
;Exercise 3.72:
(define (w4 p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (square i) (square j))))
(define iints4
  (weighted-pairs integers integers w4))
(define (generate-threes nums weight)
  (if (= (weight (stream-car nums)) (weight (stream-car (stream-cdr nums))) (weight (stream-car (stream-cdr (stream-cdr nums)))))
      (cons-stream (stream-car nums)
                   (generate-threes (stream-cdr (stream-cdr (stream-cdr nums))) weight))
      (generate-threes (stream-cdr nums) weight)))
(define threes
  (generate-threes iints4 w4))
;============Streams as signals============
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int))))
;Exercise 3.73:
(define (RC R C dt)
  (define (v i v0)
    (add-streams (scale-stream i R) (integral (scale-stream i (/ 1 C)) v0 dt)))
  v)
;Exercise 3.74:
(define (sign-change-detector a b)
  ;if sign doesn't change, return 0.
  (cond ((or (and (<= b 0) (<= a 0)) (and (>= b 0) (>= a 0))) 0)
        ;if sign changed from negative to positive, return 1.
        ((and (< b 0) (> a 0)) 1)
        ;if sign change from positive to negative, return 1.
        ((and (> b 0) (< a 0)) -1)))
(define (interval a increment)
  (cons-stream a (interval (+ a increment) increment)))
(define (f-stream f stream)
  (cons-stream (f (stream-car stream)) (f-stream f (stream-cdr stream))))
(define sense-data 
  (f-stream cos (interval 0 (/ 3.14 4))))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))
(define zero-crossings
  (make-zero-crossings sense-data 0))
(define zero-crossings-1
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))
;Exercise 3.75
;Louis's implementation averages all previous values, whereas Alyssa just wanted each value to be the average of itself and the value before it in the stream.
(define (make-zero-crossings-2 input-stream last-value last-average-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-average-value)
      (make-zero-crossings-2
        (stream-cdr input-stream) (stream-car input-stream) avpt))))
(define zero-crossings-2
  (make-zero-crossings-2 sense-data 0 0))
;Exercise 3.76:
(define (smooth s)
  (stream-map (lambda (x y) (/ (+ x y) 2)) s (cons-stream 0 s)))
(define (make-zero-crossings-3 input-stream last-value)
  (let ((smooth-stream (smooth input-stream)))
    (cons-stream (sign-change-detector (stream-car smooth-stream) last-value)
                 (make-zero-crossings-3 (stream-cdr input-stream) (stream-car smooth-stream)))))
(define zero-crossings-3
  (make-zero-crossings-3 sense-data 0))
;===========3.5.4 Streams and Delayed Evaluation===========
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
;Exercise 3.77:
;(define (integral delayed-integrand initial-value dt)
;  (cons-stream
;    initial-value
;    (let ((integrand (force delayed-integrand)))
;      (if (stream-null?           the-empty-stream
;          (integral (stream-cdr integrand)
;                    (+ (* dt (stream-car integrand))
;                       initial-value)
;                    dt)))))
;Exercise 3.78:
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream b y) (scale-stream a dy))))
;Exercise 3.79:
(define (solve-2nd-1 a b dt y0 dyo)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y)))
;Exercise 3.80:
(define (RLC R L C dt)
  (define (circuit vC0 iL0)
    (define vC-stream (integral (delay dvC) vC0 dt))
    (define iL-stream (integral (delay diL) iL0 dt))
    (define dvC 
      (scale-stream iL-stream (- (/ 1 C))))
    (define diL 
      (add-streams (scale-stream vC-stream (/ 1 L)) (scale-stream iL-stream (- (/ R L)))))
    (define (circstates s1 s2)
      (cons-stream (cons (stream-car s1) (stream-car s2)) (circstates (stream-cdr s1) (stream-cdr s2)))) 
    (circstates vC-stream iL-stream))
  circuit)
;==========================3.5.5 Modularity of Functional Programs and Modularity of Objects==========================
(define random-init 1)
(define (rand-update num)
  (let ((a (+ num 1)) (m (expt 2 10)))
    (modulo (+ (* num a) 1) m)))
(define random-numbers
  (cons-stream 
    random-init
    (stream-map rand-update random-numbers)))
(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(define cesaro-stream
  (map-successive-pairs
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define pi
  (stream-map
    (lambda (p) (sqrt (/ 6 p)))
    (monte-carlo cesaro-stream 0 0)))
;Exercise 3.81:
;this is inchmeal implementation. I had another one that was a lot uglier that I removed because I prefer this one, even though this one doesn't really keep with the requirements stated in the  book of having this be a 
;functional implementation of the object oriented one, but whatever...
(define (pre-rand-stream default-value upperbound)
  (define (rand-gen num)
    (let ((a (+ num 1) ))
      (remainder (+ (* num a) 1) upperbound)))
  (define (new-num msg old-num)
    (cond ((and (pair? msg) (eq? (car msg) 'reset) (number? (cadr msg)))
           cadr-msg)
          ((eq? msg 'reset)
           default-value)
          ((eq? msg 'generate)
           (rand-update old-num))))
  (define (make-randseq msgs)
    (define randseq
      (cons-stream (new-num (stream-car msgs) default-value)
                   (stream-map new-num randseq (stream-cdr msgs))))
    randseq)
  make-randseq) 
(define (list->stream L)
  (if (null? L)
      the-empty-stream
      (cons-stream (car L) (list->stream (cdr L)))))
;Exercise 3.82:
;so, a predicate defines a region, and we enclose that region in a rectangle. We run monte-carlo experiments in the rectangle to get the proportion of randomly selected points which lie in the region.
;Then, the integral is the area of the rectangle times the proportion of points in the region...
(define (rand-in-range-stream lower-bound upper-bound)
  (cons-stream (random-in-range lower-bound upper-bound) (rand-in-range-stream lower-bound upper-bound)))
(define (estimate-integral P x1 x2 y1 y2)
  (define test-point-stream 
    (stream-map (lambda (x y) (P x y)) (rand-in-range-stream x1 x2) (rand-in-range-stream y1 y2)))
  (scale-stream (monte-carlo test-point-stream 0 0) (* (- x2 x1) (- y2 y1)))) 
(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))
(define pi-stream (estimate-integral in-unit-circle? -1 1 -1 1))
;=======A functional-programming view of time=======
(define (stream-withdraw balance amount-stream)
  (cons-stream
    balance
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))
;=============4.Metalinguistic Abstraction=============
;=============4.1 Metalinguistic Abstraction=============
;=============4.1.1 The Core of the Evaluator=============
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else 
          (error "Unknown expression type: EVAL" exp))))
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Uknown procedure type: APPLY" procedure))))
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
;we interpret the `if` as being outside of the language being implemented. (eval (if-predicate exp) env) yields a predicate in the language being implemented. true? translates that to something
;intelligible outside of the language being implemented... thus true? translates a predicate from the language being implemented to the implementation language.
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
;Exercise 4.1:
;the following forces the first element in cons to be evaluated first, then the second, and so on, irrespective of whether or not cons is implemented to evaluate from left to right or right to left:
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((evald-exp (eval (first-operand exps) env)))
        (cons (evald-exp (list-of-values (rest-operands exps) env))))))
;can just write a method that reverses the list, and then evaluate using list-of-values-lr. Downside is that then list-of-values-rl takes at least O(n) time to evaluate... 
(define (reverse-list l)
  (define (iter index li result)
    (if (= index 0)
        result
        (iter (- index 1) (cdr li) (cons (car li) result))))
  (iter (length l) l '()))
(define (list-of-values-rl exps env)
  (let ((reversed-exps (reverse-list exps)))
    (list-of-values-lr reversed-exps env)))
;4.1.2 Representing Expressions
