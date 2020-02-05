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
;;========1.1.3 Actual Hardware Construction========
;;========1.1.4 Hardware Description Language (HDL)========
;;========1.2 Specification========
;;========1.2.1 The Nand Gate========
;;nand gate is captured in lisp by: (not (and a b))
(define (bit? arg)
  (or (= arg 0) (= arg 1)))
(define (bus? args)
  (if (null? args)
    #t
    (and (bit? (car args)) (bus? (cdr args)))))
(define (bitand a b)
  (if (bus? (list a b))
    (if (and (= a 1) (= b 1))
      1
      0)
    (error "Arguments must be bits!")))
(define (bitnot a)
  (if (bit? a)
    (if (= a 0)
      1
      0)
    (error "Arguments must be bits!")))
(define (bitor a b)
  (if (bus? (list a b))
    (if (or (= a 1) (= b 1))
      1
      0)
    (error "Arguments must be bits!")))
(define (xor a b)
  (bitor (bitand a (bitnot b)) (bitand (bitnot a) b)))
(define (nand a b)
  (bitnot (bitand a b)))
;;========1.2.2 Basic Logic Gates========
;;-Introduce not, or, and, xor, and multiplexer. As the others are primitives in
;; most language, I won't write it here.
;;We capture a multiplexer in lisp code as follows:
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
      (cons in 0)
      (cons 0 in))
    (error "Arguments must be bits!")))
;;a bus is a multi-bit array!
;;We can capture an n-bit not gate as such
;;========1.2.3 Multi-Bit Versions of Basic Gates========
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
(define or)
;;========1.2.4 Multi-Way Versions of Basic Gates========
(define (nbitor bus)
  (if (null? bus)
    0
    (bitor (car bus) (nbitor (cdr bus)))))
;;multi-way/multi-bit multiplexor: selects one of m n-bit input buses and outputs it to a sngle n-bit output bus. The selection is specified by k=log_2 m selection bits.
;;We need exactly k bits to make all possible selections of the m busses. 
;;In this book we will need a 4-way 16-bit multiplexor and an 8-way 16-bit multiplexor, so we have:
;;selbus has 2 bits. each bus has 6 bits.
(define (mux4way6 bus1 bus2 bus3 bus4 selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0)) bus1)
        ((and (= (car selbus) 0) (= (cadr selbus) 1)) bus2)
        ((and (= (car selbus) 1) (= (cadr selbus) 0)) bus3)
        ((and (= (car selbus) 1) (= (cadr selbus) 1)) bus4)))
(define (mux8way16 bus1 bus2 bus3 bus4 bus5 bus6 bus7 bus8 selbus)
  (cond ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus1)
        ((and (= (car selbus) 0) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus2)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus3)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 0)) bus4)
        ((and (= (car selbus) 0) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus5)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 0)) bus6)
        ((and (= (car selbus) 1) (= (cadr selbus) 0) (= (caddr selbus) 1)) bus7)
        ((and (= (car selbus) 1) (= (cadr selbus) 1) (= (caddr selbus) 1)) bus8)))



