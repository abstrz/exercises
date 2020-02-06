;;========1.Boolean Logic========
;;========1.1 Background========
;;-We call f(v_1 ... v_n)=(w_1 ... w_m) a boolean function if v_i,w_j \in
;; \set{0, 1}
;;========1.1.2 Gate Logic========
;;-A gate is a physical device that implements a boolean function
;;-If we have f(v_1 v_2 ... v_n)=... w_m), then the physical gate
;; representing f will have n input pins and m output pins.
;;-Gates are made up of transistors
;;-We will use the word chip and gate interchangeably
;;-Because of algebraic properties of boolean algebra operators, we get
;; multiple equivalent ways of implementing the same boolean functions.
;; The goal from an engineering perspective is to find the most efficient one.
;;========1.2.2 Basic Logic Gates========
;;-Introduce not, or, and, xor, and multiplexer. As the others are primitives in
;; most language, I won't write it here.
;;We capture a multiplexer in lisp code as follows:
;;a bus is a multi-bit array!
;;We can capture an n-bit not gate as such
;;========1.3 Implementation:========
;;Take nand gate as primitive, and build the other gates from this.
;;Going to write it all out in lisp, to help me understand how these work, but...
;;Will also write out the actual diagrams on paper to see what they'd look like in HDL
(define (bit? arg)
  (or (= arg 0) (= arg 1)))
(define (bus? args)
  (if (null? args)
    #t
    (and (bit? (car args)) (bus? (cdr args)))))
(define (bit-proc proc args)
  (if (bus? args)
    (apply proc args)
    (error "Arguments must be bits!")))
(define (nand a b)
  (define (nand-proc a b)
    (if (and (= a 1) (= b 1))
      0
      1))
  (bit-proc nand-proc (list a b)))
(define (bitnot a)
  (nand a a))
(define (bitand a b)
  (bitnot (nand a b)))
(define (bitor a b)
  (bitnot (bitand (bitnot a) (bitnot b))))
(define (xor a b)
  (bitor (bitand (bitnot a) b) (bitand a (bitnot b))))
(define (busnot bus)
  (define (recursively subbus)
    (if (null? subbus)
      ()
      (cons (bitnot (car subbus)) (recursively (cdr subbus)))))
  (if (bus? bus)
    (recursively bus)
    (error "Argument must be a bus!")))
(define (busand bus1 bus2)
  (define (recursively subbus1 subbus2)
    (if (null? subbus1)
      ()
      (cons (bitand (car subbus1) (car subbus2)) (recursively (cdr subbus1) (cdr subbus2)))))
  (if (and (bus? bus1) (bus? bus2))
    (recursively bus1 bus2)
    (error "Arguments must be busses!")))
(define (busor bus1 bus2)
  (define (recursively subbus1 subbus2)
    (if (null? subbus1)
      ()
      (cons (bitor (car subbus1) (car subbus2)) (recursively (cdr subbus1) (cdr subbus2)))))
  (if (and (bus? bus1) (bus? bus2))
    (recursively bus1 bus2)
    (error "Arguments must be busses!")))
(define (busmux bus1 bus2 sel)
  (if (= sel 0)
    bus1
    bus2))
(define (multiplexer a b sel)
  (if (bus? (list a b sel))
    (if (= sel 0)
      a
      b)
    (error "Arguments must be bits!")))
;;We capture a demultiplexer in lisp code as follows:
(define (demultiplexer in sel)
  (if (bus? (list in sel))
    (if (= sel 0)
      (list in 0)
      (list 0 in))
    (error "Arguments must be bits!")))

(define (mux4way bus1 bus2 bus3 bus4 selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0)) bus1)
        ((and (= (car selbus) 0) (= (cadr selbus) 1)) bus2)
        ((and (= (car selbus) 1) (= (cadr selbus) 0)) bus3)
        ((and (= (car selbus) 1) (= (cadr selbus) 1)) bus4)))
(define (mux8way bus1 bus2 bus3 bus4 bus5 bus6 bus7 bus8 selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus1)
        ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus2)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus3)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus4)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus5)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus6)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus7)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus8)))
(define (dmux4way in selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0)) (list in 0 0 0))
        ((and (= (car selbus) 0) (= (cadr selbus) 1)) (list 0 in 0 0))
        ((and (= (car selbus) 1) (= (cadr selbus) 0)) (list 0 0 in 0))
        ((and (= (car selbus) 1) (= (cadr selbus) 1)) (list 0 0 0 in))))
(define (dmux8way in selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus1)
        ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus2)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus3)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus4)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus5)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus6)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus7)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus8)))
(define (nbitor . bits)
  (define (recursively bus)
    (if (null? bus)
      0
      (bitor (car bus) (recursively (cdr bus)))))
  (recursively bits))


;;========2. Boolean Arithmetic========
;;========2.1 Background========

;;returns a bus with same number of bits as two argument bits, assumes both arguments are busses of the same number of bits!
(define (bitwise-addition bits1 bits2)
  (define (reverse-list l)
    (define (iteratively l result)
      (if (null? l)
        result
        (iteratively (cdr l) (cons (car l) result))))
    (iteratively l ()))
  (let ((rb1 (reverse-list bits1))
        (rb2 (reverse-list bits2)))
    (define (adder carry sb1 sb2)
      (if (null? sb1)
        ()
        (let  ((a (+ carry (car sb1) (car sb2))))
          (cond ((= a 3)
                 (cons 1 (adder 1 (cdr sb1) (cdr sb2))))
                ((= a 2)
                 (cons 0 (adder 1 (cdr sb1) (cdr sb2))))
                ((= a 1)
                 (cons 1 (adder 0 (cdr sb1) (cdr sb2))))
                ((= a 0)
                 (cons 0 (adder 0 (cdr sb1) (cdr sb2))))))))
    (reverse-list (adder 0 rb1 rb2))))

;;Let's represent binary numbers as lists such that say 101010101 is represented as (1 0 1 0 1 0 1 0 1). We want a function that can take base 10 numbers and transform them into binary numbers, and vice versa.

;;transform list of bits representing base 2 number to base 10 number
(define (num2->num10 l)
  (let ((n (- (length l) 1)))
    (define (recursively sublist i)
      (cond ((null? sublist) 0)
            ((= (car sublist) 0) (recursively (cdr sublist) (- i 1)))
            (else (+ (* (car sublist) (expt 2 i)) (recursively (cdr sublist) (- i 1))))))
    (recursively l n)))

;;transforms base 10 number to list of bits representing base 2 number
(define (num10->num2 n)
  (define (greatest-power-divisor a)
    (define (recursively i a)
      (if (>= (/ a (expt 2 (+ i 1))) 1)
        (recursively (+ i 1) a)
        i))
    (recursively 0 a))
  (define (list-of-pwrs r)
    (let ((pwr (greatest-power-divisor r)))
      (if (<= r 0)
        ()
        (cons pwr (list-of-pwrs (- r (expt 2 pwr)))))))
  (define (make-binary pwrs)
    (define (list-of-zeros n)
      (if (= n 0)
        ()
        (cons 0 (list-of-zeros (- n 1)))))
    (let ((n (+ (car pwrs) 1)))
      (let ((initial (list-of-zeros n)))
        (define (kth-place-1! k l)
          (if (null? l)
            'done
            (if (= (- (length l) 1) k)
              (set-car! l 1)
              (kth-place-1! k (cdr l)))))
        (define (recursively subpwrs)
          (if (null? subpwrs)
            'done
            (begin (kth-place-1! (car subpwrs) initial)
                   (recursively (cdr subpwrs)))))
        (recursively pwrs)
        initial)))
  (make-binary (list-of-pwrs n)))

;;as we have the formula [-x]_2=[2^n - x]_10, this should be straight forward
(define (twos-complement bits)
  (let ((n (length bits))) 
    (define (add-leading-zeros l)
      (if (= (length l) n)
        l
        (add-leading-zeros (cons 0 l))))
    (add-leading-zeros (num10->num2 (- (expt 2 n) (num2->num10 bits))))))








