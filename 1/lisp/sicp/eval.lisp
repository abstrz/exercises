(defun setcar (l v) (setf (car l) v))
(defun setcdr (l v) (setf (cdr l) v))

;;;;=============4.1.1 The Core of the Evaluator=============
(defun eval (exp env)
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

(defun apply (procedure arguments)
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
(defun list-of-values (exps env)
  (if (no-operands? exps)
      null
    (cons (eval (first-operand exps) env)
	   (list-of-values (rest-operands exps) env))))
(defun eval-if (exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))
(defun eval-sequence (exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))
(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
(defun self-evaluating? (exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
;;Varaibles are represented by symbols
(defun variable? (exp) (symbol? exp))
;;Quotations have the form (quote <text-of-quotation>)
(defun quoted? (exp) (tagged-list? exp 'quote))
(defun text-of-quotation (exp) (cadr exp))
;;quoted? is defined in terms of the procedure tagged-list?, which identifies lists beginning with a designated symbol:
(defun tagged-list? (exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))
;;Assignments have the form (set! <var> <val>)
(defun assignment? (exp) (tagged-list? exp 'set!))
(defun assignment-variable (exp) (cadr exp))
(defun assignment-value (exp) (caddr exp))
(defun definition? (exp) (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(defun definition-value (exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp) ;;formal params
                 (cddr exp)))) ;;body
(defun lambda? (exp) (tagged-list? exp 'lambda))
(defun lambda-parameters (exp) (cadr exp))
(defun lambda-body (exp) (cddr exp))
(defun make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(defun if? (exp) (tagged-list? exp 'if))
(defun if-predicate (exp) (cadr exp))
(defun if-consequent (exp) (caddr exp))
(defun if-alternative (exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))
(defun begin? (exp) (tagged-list? exp 'begin))
(defun begin-actions (exp) (cdr exp))
(defun last-exp? (seq) (null? (cdr seq)))
(defun first-exp (seq) (car seq))
(defun rest-exps (seq) (cdr seq))
;;We also include a constructor sequence->exp (for use by cond->if) that transforms a seq into a single exp, using begin if necessary:
(defun sequence->exp (seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(defun make-begin (seq) (cons 'begin seq))
(defun application? (exp) (pair? exp))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))
(defun no-operands? (ops) (null? ops))
(defun first-operand (ops) (car ops))
(defun rest-operands (ops) (cdr ops))
(defun cond? (exp) (tagged-list? exp 'cond))
(defun cond-clauses (exp) (cdr exp))
(defun cond-else-clause? (clause)
  (eq? (cond-predicate clause) 'else))
(defun cond-predicate (clause) (car clause))
(defun cond-actions (clause) (cdr clause))
(defun cond->if (exp) (expand-clauses (cond-clauses exp)))
(defun expand-clauses (clauses)
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
(defun and? (exp)
  (tagged-list? exp 'and))
(defun and-expressions (exp)
  (cdr exp))
(defun first-exp (exps)
  (car exps))
(defun rest-exps (exps)
  (cdr exps))
(defun last-exp? (exps)
  (null? (rest-exps exps)))
(defun eval-and-exps (exps env)
  (let ((e (eval (first-exp exps) env)))
    (cond ((not e) #f) ;;if e is false return false
          ((and (last-exp? exps) e) #t) ;;if e is the last expression and it is true, then return true.
          (else (eval-and (rest-exps exps) env)))))
(defun eval-and (exp env)
  (eval-and-exps (and-expressions exp) env))
(defun or? (exp)
  (tagged-list? exp 'or))
(defun or-expressions (exp) ;;same as begin and and versions, but... question specified to define appropriate syntax procedures, so although redundant I am attempting to satisfy constraints of question.
  (cdr exp))
(defun eval-or-exps (exps env)
  (if (null? exps)
    #f
    (let ((e (eval (first-exp exps) env)))
      (if e 
        #t  ;;if any expression is true, we return true
        (eval-or (rest-exps exps) env)))))  ;;else, try next ones. if none are true, we will eventually run (eval-or null env), at which point we will return false, as required.
(defun eval-or (exp env)
  (eval-or-exps (or-expressions exp) env))

(defun and->if (exps)
  (if (null? exps)
    '#t
    (let ((first (first-exp exps))
	  (rest (rest-exps exps)))
      (make-if (list 'not first) '#f (and->if rest)))))
(defun eval-and (exp env)
  (eval-if (and->if (and-expressions exp)) env))
(defun or->if (exps)
  (if (null? exps)
      '#f
    (let ((first (first-exp exps))
	  (rest (rest-exps exps)))
      (make-if first '#t (or->if rest)))))
(defun eval-or (exp env)
  (eval-or (or->if (or-expressions exp)) env))
(defun cond-=>-clause? (clause)
  (eq? (car (cond-actions clause)) '=>))
(defun test=>resp (clause)
  (list (cadr (cond-actions clause)) (cond-predicate clause)))
(defun expand-clauses (clauses)
  (if (null? clauses)
      'false
    (let ((first (car clauses))
	  (rest (cdr clauses)))
      (if (cond-else-clause? first)
	  (if (null? rest)
	      (sequence->exp (cond-actions first)) ;;turn the sequence of actions into an expression
	    (error "ELSE clause isn't last: COND->IF" clauses))
	(if (cond-=>-clause? first)
	    (make-if (cond-predicate first) 
		     (test=>resp first)
		     (expand-clauses rest))
	  (make-if (cond-predicate first)
		   (sequence->exp (cond-actions first))
		   (expand-clauses rest)))))))
(defun let-varexps (exp)
  (car (cdr exp)))
(defun let-vars (varexps)
  (if (null? varexps)
      null
    (cons (caar varexps) (let-vars (cdr varexps)))))
(defun let-exps (varexps)
  (if (null? varexps)
      null
    (cons (cadar varexps) (let-exps (cdr varexps)))))
(defun let-body (exp)
  (caddr exp))
(defun let->combination (exp)
  (cons (make-lambda (let-vars (let-varexps exp)) (let-body exp)) (let-exps (let-varexps exp))))
(defun varexps->nested (varexps body)
  (if (null? varexps)
      body
    (list 'let (list (car varexps)) (varexps->nested (cdr varexps) body))))
(defun let*->nested-lets (exp)
  (varexps->nested (let-varexps exp) (let-body exp)))
(defun let-varexps (exp)
  (car (cdr exp)))
(defun let-vars (varexps)
  (if (null? varexps)
      null
    (cons (caar varexps) (let-vars (cdr varexps)))))
(defun let-exps (varexps)
  (if (null? varexps)
      null
    (cons (cadar varexps) (let-exps (cdr varexps)))))
(defun let-body (exp)
  (caddr exp))
;;selectors for named-let
(defun named-let-varexps (exp)
  (caddr exp))
(defun named-let-vars (varexps)
  (let-vars varexps))
(defun named-let-exps (varexps)
  (let-exps varexps))
(defun named-let-proc (exp)
  (cadr exp))
(defun named-let-body (exp)
  (cadddr exp))

(defun named-let->begin-seq (exp)
  (make-begin
   (list (list 'define (cons (named-let-proc exp) (named-let-vars (named-let-varexps exp))) (named-let-body exp))
	  (cons (named-let-proc exp) (named-let-exps (named-let-varexps exp))))))
(defun let->combination (exp)
  (cond ((= (length exp) 4)
	 (named-let->begin-seq exp))
	((= (length exp) 3)
	 (cons (make-lambda (let-vars (let-varexps exp)) (list (let-body exp))) (let-exps (let-varexps exp))))
	(else (error "Unsupported LET format!"))))

(defun let? (exp) (tagged-list? exp 'let))
(defun let*? (exp) (tagged-list? exp 'let*))

(defun eval-let (exp env)
  (eval (let->combination exp) env))

(defun eval-let* (exp env)
  (eval (let*->nested-lets exp) env))

(defun eval (exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((let? exp) (eval-let exp env))
	((let*? exp) (eval-let* exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else 
	 (error "Unknown expression type: EVAL" exp))))
(defun for (i m inc proc)
  (if (< i m)
      (begin (proc i)
	     (for (inc i) m inc proc))))
(defun do-while (i pred proc)
  (if (pred i)
      (begin (proc i)
	     (do-while (proc i) pred proc)))) 
(defun assign->set! (exp)
  (cons 'set! (cdr exp)))
(defun defun->define (exp)
  (cons 'define (cdr exp)))

(defun true? (x) (not (eq? x false)))
(defun false? (x) (eq? x false))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))

(defun procedure-parameters (p) (cadr p))

(defun procedure-body (p) (caddr p))

(defun procedure-environment (p) (cadddr p))

(defun enclosing-environment (env) (cdr env))
(defun first-frame (env) (car env))
(defun the-empty-environment null)
(defun make-frame (variables values)
  (cons variables values))
(defun frame-variables (frame) (car frame))
(defun frame-values (frame) (cdr frame))
(defun add-binding-to-frame! (var val frame)
  (setcar frame (cons var (car frame)))
  (setcdr frame (cons val (cdr frame))))
(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
	(error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars fals))))
(defun lookup-variable-value (var env)
  (defun env-loop (env)
    (defun scan (vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Didn't find variable:" var)
      (let ((frame (first-frame env)))
	(let ((vars (frame-variables frame))
	      (vals (frame-values frame)))
	  (scan vars vals))))))
(defun set-variable-value! (var val env)
  (defun env-loop (env)
    (defun scan (vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars)) (setcar vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
	(scan (frame-variables frame)
	      (frame-values frame)))))
  (env-loop env))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (defun scan (vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars)) (setcar vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(defun make-unbound-frame! var frame)
(defun unbind var vars vals)
(cond ((and (eq? (cadr vars) var) (null? (cddr vars)))
       (setcdr vars null)
       (setcdr vals null))
      ((and (eq? (cadr vars) x) (not (null? (cddr vars))))
       (setcdr vars (cddr vars))
       (setcdr vals (cddr vals)))
      ((and (eq? (car vars) var) (not (null? (cddr vars))))
       (setcar vars (cadr vars))
       (setcdr vars (cddr vars))
       (setcar vals (cadr vals))
       (setcdr vals (cddr vals)))
      ((and (eq? (car vars) var) (null? (cddr vars)))
       (setcar vars (cadr vars))
       (setcdr vars null)
       (setcar vals (cadr vals))
       (setcdr vals null))
      (else (unbindd var (cdr vars) (cdr vals)))))
(let ((vars (frame-variables frame))
      (vals (frame-values frame)))
  (if (not (= (length vars) (length vals)))
      (error "There must be as many variables as values!")
    (if (> (length vars) 1)
	(unbind var vars vals)
      (if (eq? (car vars) var)
	  (begin (setcar frame null)
		 (setcdr frame null)))))))
(defun make-unbound! var env)
(if (eq? env the-empty-environment)
    'done
  (let ((frame (first-frame env)))
    (begin (make-unbound-frame! var frame))
    (make-unbound! var (enclosing-environment env)))))
;;4.1.4. Running the Evaluator as a Program:
(define primitive-procedures
  (list (list 'null? null?)
	 (list '+ +)
	 (list '- -)
	 (list '* *)
	 (list '/ /)
	 (list 'eq? eq?)
	 (list '< <)
	 (list '> >)
	 (list '= =)))
(defun primitive-procedure-names)
(map car primitive-procedures))
(defun primitive-procedure-objects)
(map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(defun setup-environment)
(let ((initial-env
       (extend-environment (primitive-procedure-names)
			   (primitive-procedure-objects)
			   the-empty-environment)))
  (define-variable! 'true true initial-env)
  (define-variable! 'false false initial-env)
  initial-env))
(define genv (setup-environment))

;;primitive-procedures are of the form (primitive <implementation-in-lisp>)
(defun primitive-procedure? proc)
(tagged-list? proc 'primitive))
(defun primitive-implementation proc) (cadr proc))


(defun apply-primitive-procedure proc args)
(apply-in-underlying-scheme (primitive-implementation proc) args))

;;driver-loop is a read-eval-print loop, which means it is a program that reads input evaluates it and prints the result, then restarts.

(define input-prompt ";; M-Eval input:")
(define output-prompt ";; M-Eval value:")
(defun driver-loop)
(prompt-for-input input-prompt)
(let ((input (read)))
  (if (not (or (eq? input 'q) (eq? input 'quit)))
      (begin (let ((output (eval input env)))
	       (announce-output output-prompt)
	       (user-print output))
	     (driver-loop)))))

(defun prompt-for-input string)
(newline) (newline) (display string) (newline))
(defun announce-output string)
(newline) (display string) (newline))

;;define our own print procedure to avoid printing environment part of compound procedures, which may contain cycles.
(defun user-print object)
(if (compound-procedure? object)
    (display (list 'compound-procedure
		    (procedure-parameters object)
		    (procedure-body object)
		    '<procedure-env>))
  (display object)))

;;Exercise 4.14:
;;We assume Louis and Eva are evaluating expressions made up of the primitives of the subset of Scheme which we've implemented thus far...
;;Let's consider what's happening. Suppose first we type in the definition of map:
;;(eval '(defun map proc l)
;;         (if (null? l)
;;             null
;;             (cons (proc (car l)) (map proc (cdr l)))))
;;      env)

;;evaluating (map (lambda (x) (car x)) '((a b) (c d) (e f)))
;;in the driver-loop gives the correct output of (a c e)

;;now if we take Louis' approach and make map a primitive of our language
;;(define primitive-procedures
;;  (list (list 'car car)
;;        (list 'cdr cdr)
;;        (list 'cons cons)
;;        (list 'null? null?)
;;        (list 'map map)))
;;(define env (setup-environment))
;;evaluating (map (lambda (x) (car x)) '((a b) (c d) (e f)))
;;in the driver-loop gives "The object null, passed as the first argument to car, is not the correct type."

;;Why?

;;Well, let's see...
;;evaluating our expression in the driver-loop is just running the following eval, saving it, and displaying it:

;;(eval '(map (lambda (x) (car x)) '((a b) (c d) (e f))) env)

;;now, that is an application, so eval directs the evaluation to
;;
;;(apply (eval (operator '(map (lambda (x) (car x)) '((a b) (c d) (e f)))) env)
;;       (list-of-values (operands '(map (lambda (x) (car x)) '((a b) (c d) (e f)))) env))

;;(eval (operator '(map (lambda (x) (car x)) '((a b) (c d) (e f)))) env) returns
;;(primitive #[compiled-procedure 16 ("list" #x5f) #x1a #xbfac42])
;;namely, a primitive procedure.

;;(list-of-values (operands '(map (lambda (x) (car x)) '((a b) (c d) (e f)))) env) returns
;;((procedure (x) ((car x)) (((false true car cdr cons null? map) #f #t (primitive #[compiled-procedure 12 ("list" #x1) #x1a #xbee762]) (primitive #[compiled-procedure 13 ("list" #x2) #x1a #xbee7d2]) (primitive #[compiled-procedure 14 ("list" #x3) #x14 #xbee83c]) (primitive #[compiled-procedure 15 ("list" #x5) #x14 #xbee8dc]) (primitive #[compiled-procedure 16 ("list" #x5f) #x1a #xbfac42])))) ((a b) (c d) (e f)))

;;which is just the compound procedure generated by the lambda and then the list of lists '((a b) (c d) (e f))

;;now, then, apply sees that the procedure is primitive, and so runs apply-primitive-procedure
;;(apply-primitive-procedure
;;  (eval 'map env)
;;  (list-of-values '((lambda (x) (car x)) '((a b) (c d) (e f))) env))
;;(apply-in-underlying-scheme
;;  map
;;  (list-of-values '((lambda (x) (car x)) '((a b) (c d) (e f))) env))

;;but now apply-in-underlying-scheme accepts scheme syntax, but our data structure for expressions is different than that of scheme, hence the error.

;;===============4.1.5 Data as Programs===============
;;Exercise 4.15:
;;Show that there cannot exist a procedure halts? that correctly determines whether p halts on a for any procedure p and object a.
;;If you had such a procedure halts?, you could implement the following program:
;;(defun run-forever) (run-forever))
;;(defun try p)
;;  (if (halts? p p) (run-forever) 'halted))
;;then consider evaluating (try try)...
;;
;;Okay, so if we run (try try) and it halts, then (halts? try try) is false, so (try try) does not halt, contradiction.
;;                                 it doesnt halt, (halts? try try) is true, which means (try try) halts, contradiction.
;;thus no such halts? procedure can exist!

;;===============4.1.6 Internal Definitions===============
;;motivating example for reworking handling of internal definitions:
;;(defun f x)
;;  (defun even? n) (if (= n 0) true (odd? (- n 1))))
;;  (defun odd? n) (if (= n 0) false (even? (- n 1))))
;;  <rest of body of f>)
;;if we evaluate expressions that use even? odd? before they are defined, we will run into errors. To avoid this, we alter the syntax of lambda expressions like so:
;;(lambda <vars>     ---------> (lambda <vars>
;;  (define u <e1>)               (let ((u '*unassigned*)
;;  (define v <e2>)                     (v '*unassigned*))
;;  <e3>)                           (set! u <e1>)
;;                                  (set! v <e2>)
;;                                  <e3>))
;;Exercise 4.16
;;a:
(defun lookup-variable-value var env)
(defun env-loop env)
(defun scan vars vals)
(cond ((null? vars)
       (env-loop (enclosing-environment env)))
      ((eq? var (car vars)) 
       (if (eq? (car vals) '*unassigned*)
	   (error "The value of the variable is not assigned!" var)
	 (car vals)))
      (else (scan (cdr vars) (cdr vals)))))
(if (eq? env the-empty-environment)
    (error "Unbound variable" var)
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
	  (frame-values frame)))))
(env-loop env))

;;b:
(defun internaldefs exps)
(cond ((null? exps) null)
      ((definition? (first-exp exps)) (cons (first-exp exps) (internaldefs (rest-exps exps))))
      (else (internaldefs (rest-exps exps)))))
(defun no-defs-exps exps)
(cond ((null? exps) null)
      ((definition? (first-exp exps)) (no-defs-exps (rest-exps exps)))
      (else (cons (first-exp exps) (no-defs-exps (rest-exps exps))))))
(defun defs->var-unassigned-pairs defs)
(if (null? defs)
    null
  (let ((def (car defs)))
    (cons (list (definition-variable def) '*unassigned*) (defs->var-unassigned-pairs (cdr defs))))))
(defun defs->set!-statements defs)
(if (null? defs)
    null
  (let ((def (car defs)))
    (cons (list 'set! (definition-variable def) (definition-value def)) (defs->set!-statements (cdr defs))))))

(defun scan-out-defines body)
(let ((defs (internaldefs (lambda-body body)))
      (nodefs (no-defs-exps (lambda-body body))))
  (let ((unassigned-pairs (defs->var-unassigned-pairs defs))
	(set!-statements (defs->set!-statements defs)))
    (append (append (list 'let unassigned-pairs) set!-statements) nodefs))))

;;c:
;;if we implement the change in procedure-body, then each time we retrieve the procedure-body the computation has to be made, which is a waste of resources...
;;also, in the global-environment lambda functions will be represented differently than they are used when they are retrieved, which does more to confuse than anything...
;;thus I will make the change to make-procedure and not procedure-body:
;;(defun make-procedure parameters body env)
;;  (list 'procedure parameters (scan-out-defines (make-lambda parameters body)) env))

;;Exercise 4.17:
;;Did this on paper. Suppose we have
;;(define t1 (lambda <vars> (define u <e1>) (define u <e2>) <e3>))
;;then evaluating (t1 args) would lead to us creating a new environment where vars:args, and the body is evaluated. Thus when <e3> is evaluated, we have just created one new frame,
;;which points to the global environment.
;;OTOH
;;(define t2 (lambda <vars> (let ((u '*unassigned*) (v '*unassigned*)) (set! u <e1>) (set! v <e2>) <e3>)))
;;then evaluating (t2 args) would lead to us creating a new environment where vars:args, then the body is evaluated, but the let in the body is syntactic sugar for
;;((lambda (u v) (set! u <e1>) (set! v <e2>) <e3>) '*unassigned* '*unassigned*), which when evaluated creates a new environment where u and v are both bound to *unassigned*, pointing to the
;;environment in which the lambda was called, which is the environment where vars are bound to args (which points to env). We then finally evaluate
;;(set! u <e1>) (set! u <e2>) <e3>,
;;so by the time <e3> is evaluated in this second case, we have constructed two frames.
;;This extra frame makes no difference in the behavior of a correct program because of our way of interacting with the environment. We search for a variable value from the most recent frame, to the oldest, so if a variable value can be found in one way of writing the program so can it in the other. definitions will be made a frame out, but that doesnt change anything since the definition values are searched for by the lookup-variable-value procedure which as was just described can find variables values irrespective of how we write the program... if a variable value can be set! in the first way it can also be set in the second and vice versa, too...

;;Assuming that our interpreter evaluates expressions from left to right, the following will work:
(defun scan-out-defines-1 body)
(let ((defs (internaldefs (lambda-body body)))
      (nodefs (no-defs-exps (lambda-body body))))
  (append defs nodefs)))

;;Exercise 4.18:
;;The procedure will look like
;;(lambda (f y0 dt)
;;  (let ((y '*unassigned*) (dy '*unassigned*))
;;    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
;;      (set! y a)
;;      (set! dy b))
;;    y))
;;This will not work, as (stream-map f y) will attempt to run on the first element of y, which is still '*unassigned* at the moment when it is evaluated, triggering an error.

;;If they are scanned out as shown in the text, we would get:

;;(lambda (f y0 dt)
;;  (let ((y '*unassigned*) (dy '*unassigned*))
;;    (set! y (integral (delay dy) y0 dt))
;;    (set! dy (stream-map f y))
;;    y))

;;This does work, as when y is set, dy is not evaluated, therefore its value is not retrieved, rather the evaluation of (delay dy) returns (lambda null dy)
;;Then, when dy! is set to (stream-map f y), we get (cons-stream (f (stream-car y)) (stream-map f (stream-cdr y))),
;;and (stream-car y) returns y0, which is well-defined, and so we get
;;(cons-stream (f y0) (stream-map f (stream-cdr y))).
;;Now, when we return y, we have the (force (delay dy)) forcing us to lookup the variable value of dy, which is (cons-stream (f y0) (stream-map f (stream-cdr y)))

;;Exercise 4.19:
;;Let's take a look at Ben's perspective, i.e. sequentially evaluating the expressions:
;;Ben is right, if the interpreter evaluates sequentially, because then we would have a set to 1 off bat, and so b would be set to 11, then a would be changed to 5, and (f x) would output 16.
;;((lambda (a)
;;  (defun f x)
;;    (define b (+ a x))
;;    (define a 5)
;;    (+ a b)
;;  (f 10))) 1)

;;Let's take a look at Alyssas argument by evaluating the procedure as we would in Exercise 4.16:
;;(lambda (a)
;;  (defun f x)
;;    (define b (+ a x))
;;    (define a 5)
;;    (+ a b)
;;  (f 10)))
;;becomes
;;(lambda (a)
;;  (let ((f '*unassigned*))
;;    (set! f (lambda (x)
;;              (define b (+ a x))
;;              (define a 5)
;;              (+ a b)))
;;    (f 10)))
;;becomes
;;(lambda (a)
;;  (let ((f ' *unassigned*))
;;    (set! f (lambda (x)
;;              (let ((b '*unassigned*) (a '*unassigned*))
;;                (set! b (+ a x))
;;                (set! a 5)
;;                (+ a b))))
;;    (f 10)))
;;Thus, Alyssa is right. If we evaluate by the mechanism introducing in exercise 4.16 we would get an error, as b would be set when a is undefined.

;;Let's consider Eva's view:
;;(lambda (a)
;;  (defun f x)
;;    (define b (+ a x))
;;    (define a 5)
;;    (+ a b)
;;  (f 10)))
;;Well, if the definitions occur first and are simultaneous, then indeed a would be set to 5 at the same time as b would be set to 15, and so indeed (+ a b) would return 20.
;;So if the evaluator worked as Eva suggests, she would be right.

;;Well, it seems like all three are right depending on how the evaluator behaves, which boils down to implementation...

;;Finally, passing the following into the MIT Scheme interpreter produces the error "Can't define name;; already free: a" error.
;;(define test
;;  (lambda (a)
;;    (defun f x)
;;      (define b (+ a x))
;;      (define a 5)
;;      (+ a b))
;;    (f 10)))
;;so the MIT-Scheme interpreter doesn't allow you to define variables in environments where those variables are already defined... Our interpreter specifies that we set! variable values
;;when the variables are already bound in the environment that we wish to define them...


;;Now to try to devise a way to make the interpreter work as Eva prefers...

;;Perhaps the interpreter can type check operands and see if definitions exist locally which would make the types correct before executing any operations on operands of a given type..
;;but... that would require further expanding the syntax of our language, and even then we might produce new bugs... so...

;;Exercise 4.20:
;;a:
;;(letrec ((<var1> <exp1>) ... (<varn> <expn>)) <body>) --> (let ((<var1> '*unassigned*) (<var2> '*unassigned*) ... (<varn> '*unassigned*))
;;                                                            (set! <var1> <exp1>)
;;                                                            (set! <var2> <exp2>)
;;                                                            ...
;;                                                            (set! <varn> <expn>)
;;                                                            <body>)

;;We will define syntax selectors.

(defun letrec-bindings exp)
(cadr exp))
(defun letrec-body exp)
(cddr exp))

(defun letrec-unassigned-variables bindings)
(if (null? bindings)
    null
  (let ((binding (car bindings)))
    (cons (list (car binding) '*unassigned*) (letrec-unassigned-variables (cdr bindings))))))
(defun letrec-set-variable-values bindings)
(if (null? bindings)
    null
  (let ((binding (car bindings)))
    (cons (list 'set! (car binding) (cadr binding)) (letrec-set-variable-values (cdr bindings))))))

(defun letrec->let exp)
(let ((bindings (letrec-bindings exp))
      (body (letrec-body exp)))
  (let ((unassigned-variables (letrec-unassigned-variables bindings))
	(set-expressions (letrec-set-variable-values bindings)))
    (append (append (list 'let unassigned-variables) set-expressions) body))))

(define test '(letrec ((<var1> <exp1>) (<var2> <exp2>) (<var3> <exp3>)) <body>))

;;b
;;(defun f x)
;;  (letrec
;;    ((even? (lambda (n)
;;              (if (= n 0) true (odd? (- n 1)))))
;;     (odd? (lambda (n)
;;             (if (= n 0) false (even? (- n 1))))))
;;    <rest of body of f>))
;;Drew the environments on paper.

;;For the letrec implementation, assuming that the syntax is manipulated like letrec->let, in evaluating (f x) with x=5,
;;we create an environment where x:5, then evaluate (f x). Then we create an environment that points to this environment where
;;even?:*unassigned* odd?:*unassgined*. In this environment we evaluate the set! expressions and the <rest of body of f>.
;;Since the set expressions which reference the variables even?, odd? are only made in an environment where those can be referenced,
;;we don't run into any issues, and our mutually recursive procedures behave correctly.

;;Now, let's consider the case where we only use let instead of letrec. Then we have
;;(f 5) = ((let ((even? (lambda (n)
;;                 (if (= n 0) true (odd? (- n 1)))))
;;               (odd? (lambda (n)
;;                 (if (= n 0) false (even? (- n 1))))))
;;          <rest of body of f>) 5)
;;      = (((lambda (even? odd?)
;;             <rest of body of f>)
;;          (lambda (n)
;;            (if (= n 0) true (odd? (- n 1))))
;;          (lambda (n)
;;            (if (= n 0) false (even? (- n 1)))))
;;          5)
;;First we create the environment where x:5, then in this environment we evaluate the expression, but that is a procedure application, and so in this environment where x:5
;;we evaluate the operator lambda and the operand lambdas. The operand procedures thus have their environments as the environment where x:5, and then a new environment is made where
;;even? odd? are set to these operands, but then the odd? and even? in the body of the operand lambdas are going to be searched for in the environment where x:5 and then in the global environment,
;;and nothing will be found. An error will be triggered.

;;Exercise 4.21:
(defun fact x) 
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 x))
;;a: done.
;;b:
;;We wish to rewrite the following using lambdas only:
(defun f x)
(defun even? n)
(if (= n 0) true (odd? (- n 1))))
(defun odd? n)
(if (= n 0) false (even? (- n 1))))
(even? x))
;;Before we do that, let's understand what the body of fact does, since it will be analogous to our solution:
;;((lambda (n)
;;     ((lambda (fact) (fact fact n))
;;      (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
;;   x)
;;=(substitute x in for n)
;;((lambda (fact) (fact fact x))
;; (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))
;;=(substitute operand lambda for fact)
;;((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) x)
;;=(substitute in the first operand lambda for ft in body of operator lambda and x for k in same body)
;;(if (= x 1) 1 (* x ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) (- x 1))))

;;Now we see why this procedure works!
;;Okay, lets now complete the exercise:
(defun f x)
((lambda (even? odd?) (even? even? odd? x))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))))
;;===============4.1.7 Separating Syntactic Analysis From Execution===============
;;Lets see exactly how the following is evaluated by following conceptually the code of our implementation (we assume arithmetic operators are primitives in env for simplicity).
;;(eval '(defun factorial n)
;;         (if (= n 1) 1 (* n (factorial (- n 1))))) env)
;;=
;;(eval-definition '(defun factorial n)
;;                    (if (= n 1) 1 (* n (factorial (- n 1))))) env)
;;=
;;(define-variable! 'factorial '(proc n (if (= n 1) 1 (* n (factorial (- n 1)))) env)))
;;so that our environment looks like (((factorial <stuff>) ((proc n (if (= n 1) 1 (* n (factorial (- n 1)))) env) <stuff>)) <rest-frames>)
;;Now consider:
;;(eval '(factorial 4) env)
;;=
;;(apply (proc n (if (= n 1) 1 (* n (factorial (- n 1)))) env) (4))
;;=
;;Let env' be (((n) (4)) ((factorial <stuff>) ((proc n (if (= n 1) 1 (* n (factorial (- n 1)))) env) <stuff>)) <rest-frames>),
;;More generally, let's just add a prime whenever we expand the previous environment by the parameters and argument of the call we are currently computing, for simplicity.
;;=
;;(eval-sequence
;;  (if (= n 1) 1 (* n (factorial (- n 1)))) env')
;;and now this would trigger the following operations, with the last being the return value of the eval-sequence procedure:
;;(eval '(* n (factorial (- n 1))) env')
;;Now, as mentioned, we assume * is a primitive in our language, so we get
;;=
;;(apply-in-underlying-scheme * (list-of-values n (factorial (- n 1)) env'))
;;=
;;(apply-in-underlying-scheme * (4 (eval '(factorial (- n 1)) env')))
;;=
;;(apply-in-underlying-scheme * (4 (apply-in-underlying-scheme * (3 (eval '(factorial (- n 2)) env'')))))
;;=
;;...
;;=
;;(apply-in-underlying-scheme * (4 (apply-in-underlying-scheme * (3 (apply-in-underlying-scheme * (2 (eval '(factorial (-n 3)) env''')))))))
;;now the final eval returns 1, and we are done.
;;We see that a case analysis and syntax analysis was done by eval on factorial 4 times, ignoring any syntax parsing and case analysis that would be done on the subtraction subexpressions.
;;We increase efficiency by separating syntax analysis from execution:

(defun eval-after-analyze exp env) ((analyze exp) env))

(defun analyze exp)
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

;;Self evaluating expression
(defun analyze-self-evaluating exp)
(lambda (env) exp))

;;Quoted expression
(defun analyze-quoted exp)
(let ((qval (text-of-quotation exp)))
  (lambda (env) qval)))

;;Variable expression (lookup of variable depends on environment so done in execution step)
(defun analyze-variable exp)
(lambda (env) (lookup-variable-value exp env)))

;;Assignment expression

(defun analyze-assignment exp)
(let ((var (assignment-variable exp))
      (vproc (analyze (assignment-value exp))))
  (lambda (env)
    (set-variable-value! var (vproc env) env)
    'ok)))
(defun analyze-definition exp)
(let ((var (definition-variable exp))
      (vproc (analyze (definition-value exp))))
  (lambda (env)
    (define-variable! var (vproc env) env)
    'ok)))
(defun analyze-if (exp)
(let ((pproc (analyze (if-predicate exp)))
      (cproc (analyze (if-consequent exp)))
      (aproc (analyze (if-alternative exp))))
  (lambda (env) (if (true? (pproc env))
		    (cproc env)
		  (aproc env)))))
(defun analyze-lambda (exp)
(let ((vars (lambda-parameters exp))
      (bproc (analyze-sequence (lambda-body exp))))
  (lambda (env) (make-procedure vars bproc env))))
(defun analyze-sequence (exps)
  (defun sequentially (proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (defun loop (first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
      (loop (sequentially first-proc (car rest-procs))
	    (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))
(defun analyze-application (exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
	    aprocs)))))
(defun execute-application (proc args)
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
(defun analyze-let (exp)
(analyze-application (let->combination exp)))


;;==================4.2. Variations on a Scheme -- Lazy Evaluation==================
;;==================4.2.1 Normal Order and Applicative Order================
(defun unless (condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(defun unless-condition (exp)
  (cadr exp))
(defun unless-usual (exp)
  (caddr exp))
(defun unless-exceptional (exp)
  (cadddr exp))
(defun unless->if (exp)
  (make-if (unless-condition exp) (unless-exceptional exp) (unless-usual exp)))

;;On one hand, we could, within our evaluator, define unless as a special form, and then, for example, compute fib as in exercise 4.25,
;;but on the other hand, at that point, unless is just syntactic sugar for if... so we might as well just use an if statement...

;;=============== 4.2.2. An Interpreter with Lazy Evaluation===================
(defun eval-lazy (exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment-lazy exp env))
	((definition? exp) (eval-definition-lazy exp env))
	((if? exp) (eval-if-lazy exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp)
	 (eval-sequence-lazy (begin-actions exp) env))
	((application? exp)
	 (apply-lazy (actual-value (operator exp) env)
		     (operands exp)
		     env))
	(else 
	 (error "Unknown expression type: EVAL" exp))))
(defun actual-value (exp env)
  (force-it (eval-lazy exp env)))

(defun eval-assignment-lazy (exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval-lazy (assignment-value exp) env)
		       env)
  'ok)

(defun eval-definition-lazy (exp env)
  (define-variable! (definition-variable exp)
    (eval-lazy (definition-value exp) env)
    env)
  'ok)

(defun eval-if-lazy (exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-lazy (if-consequent exp) env)
    (eval-lazy (if-alternative exp) env)))

(defun eval-sequence-lazy (exps env)
  (cond ((last-exp? exps)
	 (eval-lazy (first-exp exps) env))
	(else
	 (eval-lazy (first-exp exps) env)
	 (eval-sequence-lazy (rest-exps exps) env))))

(defun apply-lazy (procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence-lazy
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env)
	   (procedure-environment procedure))))
	(else (error "Unknown procedure type: APPLY" procedure))))

(defun list-of-arg-values (exps env)
  (if (no-operands? exps)
      null
    (cons (actual-value (first-operand exps) env) (list-of-arg-values (rest-operands exps) env))))
(defun list-of-delayed-args (exps env)
  (if (no-operands? exps)
      null
    (cons (delay-it (first-operand exps) env)
	  (list-of-delayed-args (rest-operands exps) env))))

(defun delay-it (exp env)
  (list 'thunk exp env))
(defun thunk? (obj)
  (tagged-list? obj 'thunk))
(defun thunk-exp (thunk) (cadr thunk))
(defun thunk-env (thunk) (caddr thunk))

(defun evaluated-thunk? (obj)
  (tagged-list? obj 'evaluated-thunk))
(defun thunk-value (evaluated-thunk)
  (cadr evaluated-thunk))

(defun force-it (obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
	   (setcar obj 'evaluated-thunk)
	   (setcar (cdr obj) result) 
	   (setcdr (cdr obj) null) ;;forget env to save space.
	   result))
	((evaluated-thunk? obj) (thunk-value obj))
	(else obj)))
(defun force-it-non-memoized obj)
(if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
  obj))
(define input-prompt-lazy ";; L-Eval input:")
(define output-prompt-lazy ";; L-Eval value:")
(defun driver-loop-lazy)
(prompt-for-input input-prompt-lazy)
(let ((input (read)))
  (let ((output
	 (actual-value
	  input env)))
    (announce-output output-prompt-lazy)
    (user-print output)))
(driver-loop-lazy))
(defun eval-hybrid (exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment-hybrid exp env))
	((definition? exp) (eval-definition-hybrid exp env))
	((if? exp) (eval-if-hybrid exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
	((begin? exp)
	 (eval-sequence-hybrid (begin-actions exp) env))
	((application? exp)
	 (apply-hybrid (actual-value-hybrid (operator exp) env)
		       (operands exp)
		       env))
	(else 
	 (error "Unknown expression type: EVAL" exp))))

(defun eval-if-hybrid exp env)
(if (true? (actual-value-hybrid (if-predicate exp) env))
    (eval-hybrid (if-consequent exp) env)
  (eval-hybrid (if-alternative exp) env)))
(defun eval-sequence-hybrid exps env)
(cond ((last-exp? exps)
       (eval-hybrid (first-exp exps) env))
      (else
       (eval-hybrid (first-exp exps) env)
       (eval-sequence-hybrid (rest-exps exps) env))))
(defun eval-definition-hybrid exp env)
(define-variable! (definition-variable exp)
  (eval-hybrid (definition-value exp) env)
  env)
'ok)
(defun eval-assignment-hybrid exp env)
(set-variable-value! (assignment-variable exp)
		     (eval-hybrid (assignment-value exp) env)
		     env)
'ok)



(defun apply-hybrid procedure arguments env)
;;primitive procedure handling needs to take into consideration whether params are to be memoized or not.
(cond ((primitive-procedure? procedure)
       (apply-primitive-procedure
	procedure
	(list-of-hybrid-arg-values arguments env)))
      ;;treatment relative to param-type
      ((compound-procedure? procedure) 
       (eval-sequence-hybrid
	(procedure-body procedure)
	(extend-environment
	 (procedure-parameters procedure)
	 (list-of-hybrid-args (procedure-parameters procedure) arguments env)
	 (procedure-environment procedure))))
      (else (error "Unknown procedure type: APPLY" procedure))))
;;we only have to worry about memoization of parameters when forcing the procedure comes into play, which doesn't happen until
(defun lazy? param)
(or (eq? (cadr param) 'lazy) (eq? (cadr param) 'lazy-memo)))

(defun list-of-hybrid-arg-values exps env)
(if (no-operands? exps)
    null
  (cons (actual-value-hybrid (first-operand exps) env) (list-of-arg-values (rest-operands exps) env))))

(defun list-of-hybrid-args params arguments env)
(cond ((null? params) null)
      ((lazy-memo? (car params))
       (cons (delay-it-memo (car arguments) env) (list-of-hybrid-args (cdr params) (cdr arguments) env)))
      ((lazy? (car params))
       (cons (delay-it (car arguments) env) (list-of-hybrid-args (cdr params) (cdr arguments) env)))
      (else (cons (car arguments) (list-of-hybrid-args (cdr params) (cdr arguments) env)))))
(defun delay-it-memo exp env)
(list 'thunk-memo exp env))

(defun force-it-hybrid obj)
(cond ((thunk-memo? obj)
       (let ((result (actual-value-hybrid (thunk-exp obj) (thunk-env obj))))
	 (setcar obj 'evaluated-thunk)
	 (setcar (cdr obj) result) 
	 (setcdr (cdr obj) null) ;;forget env to save space.
	 result))
      ((thunk? obj)
       (actual-value-hybrid (thunk-exp obj) (thunk-env obj)))
      ((evaluated-thunk? obj) (thunk-value obj))
      (else obj)))

(defun thunk-memo? obj)
(tagged-list? obj 'thunk-memo))
(defun actual-value-hybrid exp)
(force-it-hybrid (eval-hybrid exp env)))


;;=========================4.2.3 Streams as Lazy Lists=========================
