;Code based on Exercise 4.24 from SICP.

;=========DATA STRUCTURES FOR TESTING===========

;table for pkgs
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


;queue for installer
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define  (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons () ()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item ())))
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



;=========THE FIRST EVALUATOR TO BE TESTED=========

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
;save a copy of the apply procedure implemented in scheme.
(define apply-in-underlying-scheme
  apply)
;implement the metacircular evaluator apply procedure.
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
            "Unknown procedure type: APPLY" procedure))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      ()
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
      ()
      (let ((evald-exp (eval (first-operand exps) env)))
        (cons (evald-exp (list-of-values (rest-operands exps) env))))))
;can just write a method that reverses the list, and then evaluate using list-of-values-lr. Downside is that then list-of-values-rl takes at least O(n) time to evaluate... 
(define (reverse-list l)
  (define (iter li result)
    (if (null? li)
        result
        (iter (cdr li) (cons (car li) result))))
  (iter l ()))
(define (list-of-values-rl exps env)
  (let ((reversed-exps (reverse-list exps)))
    (list-of-values-lr reversed-exps env)))
;4.1.2 Representing Expressions
;The only self-evaluating items are numbers and strings:
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
;Varaibles are represented by symbols
(define (variable? exp) (symbol? exp))
;Quotations have the form (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;quoted? is defined in terms of the procedure tagged-list?, which identifies lists beginning with a designated symbol:
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;Assignments have the form (set! <var> <val>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;Definitions have the form
;(define <var> <value>) or
;(define (<var> <parameter_1> ... <parameter_n>) <body>)
;The latter form is syntactic sugar for
;(define <var>
;  (lambda (<parameter_1>...<parameter_n>)
;    <body>))
;The corresponding syntax procedures are the following:
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;formal params
                   (cddr exp)))) ;body
;lambda expressions are lists that begin with the symbol lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;We also provide a constructor for lambda expressions, whic his used by definition-value above:
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;Conditionals begin with if and have a predicate, a consequent, and an alternative. 
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
;We provide a constructor for if expressions, used by cond->if to transform `cond` expressions to `if` expressions:
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
;begin packages a seq  of expressions into a single expression. We incldue syntax operations on begin expressions to extract the actual sequence from the begin expression, as well as selectors that return the first expression and the rest of the expressions in the sequence.
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
;We also include a constructor sequence->exp (for use by cond->if) that transforms a seq into a single exp, using begin if necessary:
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;A procedure application is any compound expression that is not one of the above expression types. The car of the expression is the operator, and the cdr is the list of operands.
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;We include syntax procedures that extract the parts of a cond expression, and a procedure cond->if that transforms cond expressions into if expressions.
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))





;=========THE SECOND EVALUATOR TO BE TESTED=========

(define (eval-after-analyze exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

;Self evaluating expression
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;Quoted expression
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

;Variable expression (lookup of variable depends on environment so done in execution step)
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;Assignment expression 

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

;if expressions

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                    (cproc env)
                    (aproc env)))))

;lambda expressions

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))


;sequence expressions
;Takes (exp1 exp2 ... expn) and outputs (lambda (env) (lambda (env) (... (lambda (env) (exp1 env) (exp2 env)) ...) (expn-1 env)) (expn env))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

;application expressions
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env))
             aprocs)))))
;execute-apply is analog of apply in this new system
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))))
        (else 
          (error "Unknown procedure type: EXECTUE-APPLICATION"
                 proc))))




(define (primitive-proc? _pkg)
  (tagged-list? (pkg-proc _pkg) 'primitive))


(define (pkg name proc eval)
  (if (tagged-list? proc 'primitive)
    (list 'primitive name (lambda (env) (define-variable! name proc env) 'ok))
    (list 'non-primitive name (lambda (env) (eval proc env) 'ok))))

(define (pkg-data _pkg)
  (cdr _pkg))

(define (pkg-name _pkg)
  (car (pkg-data _pkg)))

(define (pkg-proc _pkg)
  (cdr (pkg-data _pkg)))

(define (set-pkg-name! _pkg _name)
  (set-car! (pkg-data _pkg) _name))

(define (set-pkg-proc! _pkg _proc)
  (set-cdr! (pkg-data _pkg) _proc))

(define (make-pkg_table eval env)
  (let ((pkgs (make-table-generalized)))

    ;query q needs to be of the form (primitive name) or (non-primitive name)
    (define (query-pkgs query)
      ((pkgs 'lookup!) query))

    (define (update _pkg)
      (let ((query (list (pkg-tag _pkg) (pkg-name _pkg))))
        (let ((db_pkg ((pkgs 'lookup) query)))
          (if db_pkg
            (if (not (eq? (pkg-proc _pkg) (pkg-proc _db_pkg)))
              (set-proc! _db_pkg (pkg-proc _pkg)))
            ((pkgs 'insert!) query _pkg)))))

    (define (add-pkg name proc)
      (update (pkg name proc eval)))


    ;=============Common and simple procedures====================
    ;these primitives must be represented in a way that
    ;**common math procedures**
    (define (initialize-primitives)
      (define (initialize-primitive-arithmetic env)
        (add-pkg '= (list 'primitive =) env)
        (add-pkg '+ (list 'primitive +) env)
        (add-pkg '- (list 'primitive -) env)
        (add-pkg '* (list 'primitive *) env)
        (add-pkg '/ (list 'primitive /) env)
        'ok)
      (define (initialize-primitive-modular-arithmetic env)
        (add-pkg 'remainder (list 'primitive remainder) env)
        (add-pkg 'quotient (list 'primitive quotient) env)
        (add-pkg 'modulo (list 'primitive modulo) env)
        'ok)
      (define (initialize-primitive-boolean env)
        (add-pkg 'not (list 'primitive not) env) 
        'ok)

      (define (initialize-primitive-relations env)
        (add-pkg '< (list 'primitive <) env)
        (add-pkg '<= (list 'primitive <=) env)
        (add-pkg '> (list 'primitive >) env)
        (add-pkg '>= (list 'primitive >=) env)
        (add-pkg 'eq? (list 'primitive eq?) env)
        'ok)

      (initialize-primitive-arithmetic env)
      (initialize-primitive-modular-arithmetic env)
      (ininitialize-primitive-boolean env)
      (initialize-primitive-relations env))
    (define (initialize-non-primitives)
      (add_pkg 'inc '(define (inc x) (+ x 1)))
      (add_pkg 'dec '(define (dec x) (- x 1)))
      (add_pkg 'identity '(define (identity x) x))
      (add_pkg 'square '(define (square x) (* x x)))
      (add_pkg 'cube '(define (cube x) (* x x x)))
      (add_pkg 'average '(define (average . args) (/ (sum args) (length args))))
      (add_pkg 'logB '(define (logB B x)
                        (/ (log x) (log B))))
      (add_pkg 'fib '(define (fib n)
                       (if (= n 0) 
                         0
                         (if (= n 1) 
                           1
                           (+ (fib (- n 1)) (fib (- n 2)))))))
      (add_pkg 'fact '(define (fact n)
                        (if (= n 1) 1 (* n (fact (- n 1))))))
      ;**boolean procedures**
      (add_pkg 'false? '(define (false? x) (eq? x false)))

      (add_pkg 'true? '(define (true? x) (not (false? x))))

      (add_pkg 'and '(define (and p1 p2)
                       (if (true? p1)
                         (if (true? p2)
                           true
                           false)
                         false)))
      (add_pkg'or '(define (or p1 p2)
                     (if (true? p1)
                       true
                       (if (true? p2)
                         true
                         false))))
      (add_pkg 'number-list? '(define (number-list? input)
                                (cond ((number? input) #t)
                                      ((symbol? input) #f)
                                      ((pair? input) 
                                       (and (number? (car input)) (number-list? (cdr input))))
                                      (else #t))))
      ;**pair procedures** 
      (add_pkg 'append '(define (append x y)
                          (if (null? x) 
                            y 
                            (cons (car x) (append (cdr x) y)))))
      (add_pkg 'map '(define (map proc . args)
                       (define (mini-map proc arg)
                         (if (null? arg)
                           ()
                           (cons (proc (car arg)) (mini-map proc (cdr arg)))))
                       (define (map-proc proc . args)
                         (if (null? args)
                           ()
                           (cons (mini-map proc (car args)) (map-proc proc (cdr args)))))
                       map-proc))    
      (add_pkg 'filter '(define (filter pred? L)
                          (cond ((null? L) ())
                                ((pred? (car L)) (cons L (filter pred? (cdr L))))
                                (else (filter pred? (cdr L)))))))
    (initialize-primitives)
    (intialize-non-primitives)
    (define (dispatch m)
      (cond ((eq? 'add) add-pkg)
            ((eq? 'remove) rm-pkg)
            ((eq? 'query) query-pkgs)
            ((eq? 'update) update)
            ((eq? 'pkgs) pkgs)))
    dispatch))

(define (query-pkgs query)
      ((pkgs 'lookup!) query))

    (define (update _pkg)
      (let ((query (list (pkg-tag _pkg) (pkg-name _pkg))))
        (let ((db_pkg ((pkgs 'lookup) query)))
          (if db_pkg
            (if (not (eq? (pkg-proc _pkg) (pkg-proc _db_pkg)))
              (set-proc! _db_pkg (pkg-proc _pkg)))
            ((pkgs 'insert!) query _pkg)))))

    (define (add-pkg name proc)
      (update (pkg name proc eval)))

(define (search-queue queue)
  (let ((qlist (front-ptr queue)))
    (define (installer _items env)
      (define (install-all _items env)
        (let ((_item (car _items)))
          (if (null? _items)
            'done
            (begin ((item-proc _item) env)
                   (install-all (cdr _items))))))

      (define (install-by-number _items env number)
        (if (and (>= number 0) (< number (length _items)))
          (let ((_item (list-ref _items number)))
            (newline)
            (display "Installing ")
            (display (item-name _item))
            (display " ...")
            (newline)
            ((item-proc _item) env))
          (begin (newline)
                 (display "Your input number must satisfy 0<=number<")
                 (display (length _items))
                 (display "! ")
                 (newline))))

      (define (install-by-#list _items env . _numbers)
        (let ((len (length _numbers)))
          (cond ((null? _numbers) 'done)
                ((= len 1) (install-by-number _items env (car _numbers)))
                ((> len 1) (begin ((install-by-number _items env (car _numbers)))
                                  (install-by-#list _items env (cdr _numbers)))))))

      (define (print-items _pkgs)
        (let ((range (enumerate-interval 0 (length _pkgs))))
          (define (print-iter indexing seq)
            (if (not (null? seq))
              (begin (newline)
                     (display (car indexing))
                     (display ". ")
                     (display (car seq))
                     (display ". ")
                     (newline)
                     (print-iter (cdr indexing) (cdr seq)))))
          (print-iter range store_items)))

      (define (vendor)
        (display "**********************************************************************")
        (newline)
        (newline)
        (display "s: to install from store.")
        (newline)
        (display "q to quit.")
        (newline)
        (newline)
        (let ((input (read)))
          (cond ((eq? input 's)
                 (display "a to install all in store." )
                 (newline)
                 (display "p to install primitives.")
                 (newline)
                 (display "0 1 2 3 ... to install non-primitives by indexing in store database.")
                 (newline)
                 (display "l to list out packages in store")
                 (newline)
                 (display "q to quit")
                 (newline)
                 (set! input (read))
                 (cond ((eq? input 'a)
                        (install-all store env)
                        (vendor))
                       ((number? input)
                        (install-by-number store_items env input))



                       ((number-list? input)
                        (install-by-#list store_items env input)
                        (vendor))
                       ((eq? input 'p)
                        (install-primitives env)
                        (vendor))
                       ((eq? input 'l)
                        (print-items store_items)
                        (vendor))
                       ((eq? input 'q)
                        (newline)
                        'Exiting...)
                       (else (display "I didn't understand. Either press s to install from store or q to quit.")
                             (newline)
                             (vendor))))
                ((eq? input 'q)
                 (newline))
                (else (begin (display "I didn't understand. Either press s to install from store or q to quit.")
                             'Exiting...)))))    
      (vendor))   



    ;
    ;
    ;
    ;Okay, our installer will take a list of environments, and run each through the installer.

    (define env (setup-environment))
    (define my_store_1 ((store eval) env))
    ;(define my_store_2 ((store eval-after-analyze) env))





    ;===================TESTING===================
    ;analyze at execution time.

    (define (test-seq eval env)
      (eval '(fib 10) env)
      (eval '(fact 10) env)
      (eval '(append '(a b c d e) '(f g h i j)) env))


    (define (n-calls prod eval env n)
      (if (= n 0)
        'done
        (begin (prod eval env)
               (n-calls prod eval env (- n 1)))))

    (define (timed eval n)
      (let ((starttime (runtime)))
        (n-calls test-seq eval (test-env eval) n)
        (- (runtime) starttime)))



    ;(define (timed-evall n)
    ;  (let ((starttime (runtime)))
    ;    (n-calls test-seq evall test-evall-env n)
    ;    (- (runtime) starttime)))

    (define (%diff-evals eval1 eval2 n)
      (let ((teval (timed eval1 n))
            (tevall (timed eval2 n)))
        (abs (/ (- tevall teval) teval))))

    ;(%diff-eval eval evall 50) returns  .28
    ;(%diff-eval eval evall 100) returns .44 
    ;(%diff-eval eval evall 200) returns .44
    ;(%diff-evals eval evall 400) returns .44
    ;(%diff-evals eval evall 800) returns .44

    ;Thus evall is roughly .44 percent faster than eval! We've achieved a nice efficiency boost by performing more of the syntactical analysis on expressions, before executing them...


