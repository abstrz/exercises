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


;It works because the set of all subsets is simply the set of all subsets of the cdr of the set union the set of the one missing element union any those subsets... 
;=========2.2.3 Sequences as Conventional Interfaces ==========
;qweqwe
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
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
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
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
;if two programmers wrote these two implementations of complex numbers separately, a third programmer could implement the arithmetic package,
;without ever having to know how the complex numbers are specifically implemented. 
;==============2.4.3. Data-Directed Programming and Additivity ==============
;For this section assume we have two procedures, put and get, for manipulating the operation-and-type table:
;(put <op> <type> <item>) installs the <item> in the table, indexed by the <op> and the <type>
;(get <op> <type>) looks up the <op>, <type> entry in the table and returns the item found there. If no item is found, get returns false.
;for now, we assume put and get come with Scheme, but later (Chapter 3, Section 3.3.3.) we will implement these.
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
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;;interface to the rest of the system
  (define (tag x) (attach-tax 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
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
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;;interface to the rest of the system 
  (define (tag x) (attach-tax 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag '(polar) 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar) 
       (lambda (x y) (tag (make-from-mag-ang r a))))
  'done)
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
;    Write new procedures to handle the new types. If there are n new types, we would have to write n new procedures.
;  new operations:
;    Write the new operations for each of the types. If there are n new operations, we would have to write n*(number-of-types) new procedures. 
;Data directed:
;  new types:
;    This corresponds to new columns in our (<operator>,types) table.
;  new operations:
;    This correspond to new columns in our (<operator>,types) table. 
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

