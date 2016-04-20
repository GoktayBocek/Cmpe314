;;Goktay Back to Racket

#lang plai-typed

;define  cal
;Grammar of surface language
;;  cal ->  number
;;  cal ->  cal+ cal
;;  cal ->  cal* cal
;;  cal -> ( cal)
;; Alphabet: [+, *, (), **, -, number]

;;  cal is a typed defined as follows,
;;  cal = <number>
;;  cal =(add < cal> < cal>)
;;       (mult < cal> < cal>)
;;       (sub < cal> < cal>)
;;       (div < cal> < cal>)
;;       (exp < cal> < cal>)

(define-type  cal
  [ cal-num (n : number)]
  [ cal-add (nml :  cal) (nmr :  cal)]
  [ cal-mult (nml :  cal) (nmr :  cal)]
  [ cal-sub (nml :  cal) (nmr :  cal)]
  [ cal-div (nml :  cal) (nmr :  cal)]
  [ cal-exp (nml :  cal) (nmr :  cal)])

; Grammar for  calC
;;  calC -> number
;;  calC -> symbol
;;if-greater-than-zero -> < cal> < cal> < cal>
;;appC -> < cal> < cal>

(define-type  calC
  [ calC-num (n : number)]
  [ calC-id (s : symbol)]
  [if-greater-than-zero (pred :  calC)(trueState :  calC)(falseState :  calC)]
  [appC (fun : symbol) (arg :  calC)]
  [binaryOpC (op : symbol) (l : calC) (r : calC)])

;; exponentiation function
(define(** u t)
  (cond
    ((= u 1) t)
    (else
     (* t(**(sub1 u) t)))))

;;Tests
( cal-num 7)
( cal-add ( cal-num 3) ( cal-num 4))
( cal-mult ( cal-num 4) ( cal-num 3))
( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 30))
( cal-mult ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 5))


;;-----------------------------------------------------------------------------------------------------------------------------------------------------

;; CORE LANGUAGE
;;the core language
;; eval  cal -> number
;; evaluate an  cal expression

(define (eval [expr :  cal])
  (type-case  cal expr
    [ cal-num (n) n]
    [ cal-add (nml nmr) (+ (eval nml) (eval nmr))]    ; cal-add add function
    [ cal-mult (nml nmr) (* (eval nml) (eval nmr))]   ; cal-mult mult function
    [ cal-sub (nml nmr) (- (eval nml) (eval nmr))]    ; cal-sub sub function
    [ cal-div (nml nmr) (/ (eval nml) (eval nmr))]  ; cal-div ))  ; cal-div div function
    [ cal-exp (nml nmr) (** (eval nml) (eval nmr))])) ; cal-exp ))  ; cal-exp exp function


;;testing core
;; ( cal-num 7) -> 7
;; ( cal-add ( cal-num 3) ( cal-num 4)) -> 7
;; ( cal-div ( cal-num 5) ( cal-num 5)) -> 1
;; ( cal-mult ( cal-num 4) ( cal-num 3)) -> 12
;; ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 30)) -> 37
;; ( cal-mult ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 5)) -> 60


(test (eval ( cal-num 7))  7)
(test (eval ( cal-num 5))  5)
(test (eval ( cal-add ( cal-num 3) ( cal-num 4)))  7)
(test (eval ( cal-sub ( cal-num 5) ( cal-num 2)))  3)
(test (eval ( cal-div ( cal-num 8) ( cal-num 1)))  8)
(test (eval ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35)))  42)
(test (eval ( cal-mult ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 5)))  35)
(test (eval ( cal-add ( cal-div ( cal-num 20) ( cal-num 5)) ( cal-num 35)))  39)

;; parse s-expression ->  cal
;; convert a quoted s expression into the equivalent  cal form

(define (parse [s : s-expression]) :  cal
  (cond
    [(s-exp-number? s) ( cal-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) ( cal-add (parse (second sl)) (parse (third sl)))]
         [(*) ( cal-mult (parse (second sl)) (parse (third sl)))]
         [(-) ( cal-sub (parse (second sl)) (parse (third sl)))]
         [(/) ( cal-div (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;;testing parse
;; '7 -> ( cal-num 7)
;; '(+ 3 4) -> ( cal-add ( cal-num 3) ( cal-num 4))
;; '(+ (+ 3 4) 35) -> ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35))

(test (parse '7) ( cal-num 7))
(test (parse '2) ( cal-num 2))
(test (parse '(+ 3 4)) ( cal-add ( cal-num 3) ( cal-num 4)))
(test (parse '(/ 20 4)) ( cal-div ( cal-num 20) ( cal-num 4)))
(test (parse '(* 2 8)) ( cal-mult ( cal-num 2) ( cal-num 8)))
(test (parse '(+ (+ 3 4) 35)) ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35)))
(test (parse '(+ (- 3 4) 35)) ( cal-add ( cal-sub ( cal-num 3) ( cal-num 4)) ( cal-num 35)))
(test (parse '(/ (* 3 4) 6)) ( cal-div ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 6)))

;;-----------------------------------------------------------------------------------------------------------------------------------------------------

;; PARSER FOR PREFIX
;; parse s-expression ->  cal
;; convert a quoted s expression into the equivalent  cal form
;; examples
;; '7 -> ( cal-num 7)
;; '(+ 3 4) -> ( cal-add ( cal-num 3) ( cal-num 4))
;; '(+ (+ 3 4) 35) -> ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35))

(define (parse-prefix [s : s-expression]) :  cal
  (cond
    [(s-exp-number? s) ( cal-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) ( cal-add (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(*) ( cal-mult (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(-) ( cal-sub (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(**) ( cal-exp (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [else (error 'parse-prefix "invalid list input")]))]
    [else (error 'parse-prefix "invalid input")]))


(test (parse-prefix '7) ( cal-num 7))
(test (parse-prefix '2) ( cal-num 2))
(test (parse-prefix '(+ 3 4)) ( cal-add ( cal-num 3) ( cal-num 4)))
(test (parse-prefix '(- 20 4)) ( cal-sub ( cal-num 20) ( cal-num 4)))
(test (parse-prefix '(* 2 8)) ( cal-mult ( cal-num 2) ( cal-num 8)))
(test (parse-prefix '(+ (+ 3 4) 35)) ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35)))
(test (parse-prefix '(+ (- 3 1) 35)) ( cal-add ( cal-sub ( cal-num 3) ( cal-num 1)) ( cal-num 35)))
(test (parse-prefix '(- (* 3 4) 6)) ( cal-sub ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 6)))


;; PARSER FOR INFIX
;; parse s-expression ->  cal
;; convert a quoted s expression into the equivalent  cal form
;; examples
;; '7 -> ( cal-num 7)
;; '(3 + 4) -> ( cal-add ( cal-num 3) ( cal-num 4))
;; '(2 + 3) -> ( cal-add ( cal-num 2) ( cal-num 3))
;; '(2 - (3 * 4)) ( cal-sub ( cal-num 2)( cal-mult ( cal-num 3)( cal-num 4))))
;; '((3 + 4) + 35) -> ( cal-add ( cal-add ( cal-num 3) ( cal-num 4)) ( cal-num 35))
;; '((2 + 3) ** (3 * 4))) ( cal-exp ( cal-add ( cal-num 2)( cal-num 3))( cal-mult ( cal-num 3)( cal-num 4))))

(define (parse-infix [s : s-expression]) :  cal
  (cond
    [(s-exp-number? s) ( cal-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([s1 (s-exp->list s)])
       (case (s-exp->symbol (second s1))
         [(+) ( cal-add (parse-infix (first s1)) (parse-infix (third s1)))]
         [(*) ( cal-mult (parse-infix (first s1)) (parse-infix (third s1)))]
         [(-) ( cal-sub (parse-infix (first s1)) (parse-infix (third s1)))]
         [(**) ( cal-exp (parse-infix (first s1)) (parse-infix (third s1)))]
         [else (error 'parse-infix "invalid list input")]))]
    [else (error 'parse-infix "invalid input")]))

;;Tests
(test (parse-infix '2) ( cal-num 2))
(test (parse-infix '5) ( cal-num 5))
(test (parse-infix '6) ( cal-num 6))
(test (parse-infix '7) ( cal-num 7))
(test (parse-infix '8) ( cal-num 8))
(test (parse-infix '(3 + 4)) ( cal-add ( cal-num 3) ( cal-num 4)))
(test (parse-infix '(2 - 5)) ( cal-sub ( cal-num 2) ( cal-num 5)))
(test (parse-infix '(6 * 7)) ( cal-mult ( cal-num 6) ( cal-num 7)))
(test (parse-infix '(2 ** 3)) ( cal-exp ( cal-num 2) ( cal-num 3)))
(test (parse-infix '(3 + 4)) ( cal-add ( cal-num 3) ( cal-num 4)))
(test (parse-infix '(2 - (3 * 4))) ( cal-sub ( cal-num 2) ( cal-mult ( cal-num 3)( cal-num 4))))
(test (parse-infix '((6 * 7) + 35)) ( cal-add ( cal-mult ( cal-num 6)( cal-num 7))( cal-num 35)))
(test (parse-infix '((1 - 2) * 5)) ( cal-mult ( cal-sub ( cal-num 1)( cal-num 2))( cal-num 5)))
(test (parse-infix '((2 ** 3) - 5)) ( cal-sub ( cal-exp ( cal-num 2)( cal-num 3))( cal-num 5)))
(test (parse-infix '((3 + 4) - 5)) ( cal-sub ( cal-add ( cal-num 3)( cal-num 4))( cal-num 5)))
(test (parse-infix '((10 - 9) ** (3 * 4))) ( cal-exp ( cal-sub ( cal-num 10)( cal-num 9))( cal-mult ( cal-num 3)( cal-num 4))))
(test (parse-infix '((2 + 3) + (3 * 4))) ( cal-add ( cal-add ( cal-num 2)( cal-num 3))( cal-mult ( cal-num 3)( cal-num 4))))
(test (parse-infix '((5 + 5) - (3 + 4))) ( cal-sub ( cal-add ( cal-num 5)( cal-num 5))( cal-add ( cal-num 3)( cal-num 4))))
(test (parse-infix '((1 + 1) * (5 * 6))) ( cal-mult ( cal-add ( cal-num 1)( cal-num 1))( cal-mult ( cal-num 5)( cal-num 6))))
(test (parse-infix '((2 + 3) ** (3 * 4))) ( cal-exp ( cal-add ( cal-num 2)( cal-num 3))( cal-mult ( cal-num 3)( cal-num 4))))

;;output-reverse-polish  cal -> list of s-expression
;;output the  cal as the reverse polish commands needed to evaluate it
;; template
;;(define (output-reverse-polish [expr :  cal])
;; (type-case  cal expr
;; [ cal-num (n) ..]
;; [ cal-add (nml nmr)(... (output-reverse-polish nml)(output-reverse-polish nmr))...]
;; [ cal-mult (nml nmr)(... (output-reverse-polish nml)(output-reverse-polish nmr))...]
;; [ cal-sub (nml nmr)(... (output-reverse-polish nml)(output-reverse-polish nmr))...]
;; [ cal-exp (nml nmr)(... (output-reverse-polish nml)(output-reverse-polish nmr))...]

;; examples
;; ( cal-num 7) -> '(7)
;; ( cal-add ( cal-num 3) ( cal-num 4)) -> '(4 3 +)
;; ( cal-mult ( cal-num 3) ( cal-num 4)) -> '(4 3 *)
;; ( cal-add ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 9)) -> '(3 4 * 9 +)
;; ( cal-mult ( cal-num 3) ( cal-add ( cal-num 4) ( cal-num 9))) -> '(3 4 9 + *)

(define (output-reverse-polish [expr :  cal])
  (type-case  cal expr
    [ cal-num (n) (list (number->s-exp n))]
    [ cal-add (nml nmr) (append (append (output-reverse-polish nml) (output-reverse-polish nmr)) (list (symbol->s-exp '+)))]
    [ cal-mult (nml nmr) (append (append (output-reverse-polish nml) (output-reverse-polish nmr)) (list (symbol->s-exp '*)))]
    [ cal-sub (nml nmr) (append (append (output-reverse-polish nml) (output-reverse-polish nmr)) (list (symbol->s-exp '-)))]
    [ cal-div (nml nmr) (append (append (output-reverse-polish nml) (output-reverse-polish nmr)) (list (symbol->s-exp '/)))]
    [ cal-exp (nml nmr) (append (append (output-reverse-polish nml) (output-reverse-polish nmr)) (list (symbol->s-exp '**)))]))

(test (output-reverse-polish ( cal-num 1)) (s-exp->list '(1)))
(test (output-reverse-polish ( cal-num 2)) (s-exp->list '(2)))
(test (output-reverse-polish ( cal-num 3)) (s-exp->list '(3)))
(test (output-reverse-polish ( cal-num 4)) (s-exp->list '(4)))
(test (output-reverse-polish ( cal-num 5)) (s-exp->list '(5)))
(test (output-reverse-polish ( cal-add ( cal-num 5) ( cal-num 7))) (s-exp->list '(5 7 +)))
(test (output-reverse-polish ( cal-add ( cal-num 10) ( cal-num 12))) (s-exp->list '(10 12 +)))
(test (output-reverse-polish ( cal-add ( cal-num 3) ( cal-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish ( cal-sub ( cal-num 4) ( cal-num 5))) (s-exp->list '(4 5 -)))
(test (output-reverse-polish ( cal-mult ( cal-num 2) ( cal-num 6))) (s-exp->list '(2 6 *)))
(test (output-reverse-polish ( cal-mult ( cal-num 10) ( cal-num 2))) (s-exp->list '(10 2 *)))
(test (output-reverse-polish ( cal-add ( cal-num 3) ( cal-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish ( cal-add ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish ( cal-sub ( cal-num 5) ( cal-add ( cal-num 6) ( cal-num 7)))) (s-exp->list '(5 6 7 + -)))
(test (output-reverse-polish ( cal-add ( cal-num 10) ( cal-add ( cal-num 1) ( cal-num 2)))) (s-exp->list '(10 1 2 + +)))
(test (output-reverse-polish ( cal-add ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish ( cal-add ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 9))) (s-exp->list '(3 4 * 9 +)))

"Example outputs"
(output-reverse-polish ( cal-num 7))
(output-reverse-polish ( cal-add ( cal-num 3) ( cal-num 4)))
(output-reverse-polish ( cal-mult ( cal-num 3) ( cal-num 4)))
(output-reverse-polish ( cal-add ( cal-mult ( cal-num 3) ( cal-num 4)) ( cal-num 9)))
(output-reverse-polish ( cal-mult ( cal-num 3) ( cal-add ( cal-num 4) ( cal-num 9))))

"Parser -> reverse polish output" 
(output-reverse-polish (parse-prefix '(+ 99 (* 5 8))))
"Parser -> evaluation" 
(eval (parse-prefix '(+ 99 (* 5 8))))

;unparser-infix
;Contract 
; cal -> s-expression
;This function takes  cal type object and unparses it into infix bracketed s-expression.
(define (unparser-infix [expr :  cal])
  (type-case  cal expr
    [ cal-num (n) (list (number->s-exp n))]
      ( cal-add (nml nmr) (append (append (unparser-infix nml) (list (symbol->s-exp '+))) (unparser-infix nmr)))
      ( cal-mult (nml nmr) (append (append (unparser-infix nml) (list (symbol->s-exp '*))) (unparser-infix nmr)))
      ( cal-sub (nml nmr) (append (append (unparser-infix nml) (list (symbol->s-exp '-))) (unparser-infix nmr)))
      ( cal-div (nml nmr) (append (append (unparser-infix nml) (list (symbol->s-exp '/))) (unparser-infix nmr)))
      ( cal-exp (nml nmr) (append (append (unparser-infix nml) (list (symbol->s-exp '**))) (unparser-infix nmr)))))

;test

(test (unparser-infix ( cal-num 1)) (s-exp->list '(1)))
(test (unparser-infix ( cal-num 2)) (s-exp->list '(2)))
(test (unparser-infix ( cal-num 3)) (s-exp->list '(3)))
(test (unparser-infix ( cal-num 4)) (s-exp->list '(4)))
(test (unparser-infix ( cal-num 5)) (s-exp->list '(5)))
(test (unparser-infix ( cal-add ( cal-num 5) ( cal-num 7))) (s-exp->list '(5 + 7)))
(test (unparser-infix ( cal-add ( cal-num 3) ( cal-num 6))) (s-exp->list '(3 + 6)))
(test (unparser-infix ( cal-add ( cal-num 4) ( cal-num 5))) (s-exp->list '(4 + 5)))

;unparser-prefix
;Contract 
;AE -> s-expression
;This function takes AE type object and unparses it into prefix bracketed s-expression.
(define (unparser-prefix [expr :  cal])
  (type-case  cal expr
    [ cal-num (n) (list (number->s-exp n))]
    ( cal-add (nml nmr) (append (list(symbol->s-exp '+)) (append (unparser-prefix nml) (unparser-prefix nmr))))
    ( cal-mult (nml nmr) (append (list(symbol->s-exp '*)) (append (unparser-prefix nml) (unparser-prefix nmr))))
    ( cal-sub (nml nmr) (append (list(symbol->s-exp '-)) (append (unparser-prefix nml) (unparser-prefix nmr))))
    ( cal-div (nml nmr) (append (list(symbol->s-exp '/)) (append (unparser-prefix nml) (unparser-prefix nmr))))
    ( cal-exp (nml nmr) (append (list(symbol->s-exp '**)) (append (unparser-prefix nml) (unparser-prefix nmr))))))

;test

(test (unparser-prefix ( cal-num 1)) (s-exp->list '(1)))
(test (unparser-prefix ( cal-num 2)) (s-exp->list '(2)))
(test (unparser-prefix ( cal-num 3)) (s-exp->list '(3)))
(test (unparser-prefix ( cal-num 4)) (s-exp->list '(4)))
(test (unparser-prefix ( cal-num 5)) (s-exp->list '(5)))
(test (unparser-prefix ( cal-add ( cal-num 3) ( cal-num 4))) (s-exp->list '(+ 3 4)))
(test (unparser-prefix ( cal-add ( cal-num 5) ( cal-num 7))) (s-exp->list '(+ 5 7)))
(test (unparser-prefix ( cal-add ( cal-num 4) ( cal-num 9))) (s-exp->list '(+ 4 9)))


;;-----------------------------------------------------------------------------------------------------------------------------------------------------

; SUGAR LANGUAGE
;;the sugary language

(define-type  cals
  [num-cals (n : number)]
  [plus-cals (l :  cals) (r :  cals)]
  [mult-cals (l :  cals) (r :  cals)]
  [bmin-cals (l :  cals) (r :  cals)] ;;can be expressed with plus and mult
  [umin-cals (r :  cals)]                      ;;can be expressed with plus and mult
  [div-cals  (l :  cals) (r :  cals)])

;;Desugar method
;;  cals ->  cal
;; convert sugary syntax into core language
  
  (define (desugar [sugar :  cals]) :  cal
  (type-case  cals sugar
    [num-cals (n) ( cal-num n)]
    [plus-cals (l r) ( cal-add
                      (desugar l)
                      (desugar r))]
    [mult-cals (l r) ( cal-add
                      (desugar l)
                      (desugar r))]
    [bmin-cals (l r) ( cal-add       ;;translates binary minus to addition of the neg of the second argument
                      (desugar l)
                      ( cal-add
                       ( cal-num -1)
                       (desugar r)))]
    [umin-cals (v) (desugar (bmin-cals (num-cals 0) v))] ;;unary minus translates to subtracting the argument from 0
    [div-cals (l r) ( cal-div      ;;in the core language
                     (desugar l) 
                     (desugar r))]))

;;testing desugar
;; ( cals-num 5) -> 5
;; (plus-cals (num-cals 8) umin-cals (num-cals 5))
;; ( cal-add ( cal-num 8) ( cal-add ( cal-num 0) ( cal-mult ( cal-num -1) ( cal-num 5)
;; (div-cals (num-cals 4) (num-cals 2)
;; ( cal-div ( cal-num 4) ( cal-num 2)

    
(test (desugar (num-cals 9)) ( cal-num 9))
(test (desugar (plus-cals (num-cals 8) (umin-cals (num-cals 5))))
      ( cal-add ( cal-num 8) ( cal-add ( cal-num 0) ( cal-add ( cal-num -1) ( cal-num 5)))))
(test (desugar (div-cals (num-cals 4) (num-cals 2)))
      ( cal-div ( cal-num 4) ( cal-num 2)))

;;-----------------------------------------------------------------------------------------------------------------------------------------------------

;; expt : number number -> number
;; Purpose: To calculate exponentiation of given two number, first number base and second is power.
(define (expt (a : number) (b : number)) : number
  (cond
    ((= b 0) 1)
    ((even? b) (sqr (expt a (/ b 2))))
    (else (* a (expt a (- b 1))))))

;; sqr : number -> number
;; Purpose: To calculate square of given number.
(define (sqr (a : number)) : number
  (* a a))

;; Pair is a well-known data structure in Lisp/Scheme family languages,
;; - since we do not have a data structure in plai-type, 
;; - this is an basic implementation of it.
(define-type pair
  (sym-op (sym : symbol)(op : (number number -> number))))

;; A table for operations, 
;; - by changing just this data structure,
;; - you can add any binary operations.

;; A list of pair(sym-op) as table of operations.
;; Handycap of this is, 
;; - it is complety depending on host language's operations.
(define ops
  (list
   (sym-op '+ +)
   (sym-op '* *)
   ;; Several binary operations added as it seen below
   (sym-op '- -)
   (sym-op '/ /)
   (sym-op '^ expt)
   (sym-op 'custom (λ (x y) (+ (* 2 x) y)))
   ))

;; get-op : symbol -> ((number number) -> number)
;; Purpose : To obtain binary defined operation from operation definition table.
(define (get-op (sym : symbol)) : (number number -> number)
  (sym-op-op (assoc sym ops)))

;; assoc : symbol (listof pair) -> pair
;; Purpose : To associate given symbol with operation defined in a listof pairs.
(define (assoc (s : symbol) (lp : (listof pair))) : pair
  (let ((list-op (filter (lambda (x) (eq? s (sym-op-sym x))) lp)))
    (if (empty? list-op)
        (error 'assoc "Operation not defined")
        (first list-op))))


;; parseC : s-exp ->  calC
;; Purpose : To parseC given s-exp to abstract syntax  calC
;; Template : 
;(define (parseC [s : s-expression]) :  calC
;  (cond
;    [n ...]
;    [id ...]
;    any unary or binary function
;    ))

(define (parseC [s : s-expression]) :  calC
  (cond
    [(s-exp-number? s) ( calC-num (s-exp->number s))]
    [(s-exp-symbol? s) ( calC-id  (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
           (cond
         [(= (length sl) 4)
          (if (symbol=? 'ifzero (s-exp->symbol (first sl)))
              (if-greater-than-zero (parseC (second sl))
                                    (parseC (third sl))
                                    (parseC (fourth sl)))
              (error 'parse "invalid expression "))]
         [(= (length sl) 3)
          (binaryOpC (s-exp->symbol (first sl)) 
                     (parseC (second sl)) (parseC (third sl)))]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parseC (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))

;;test

(test (parseC (number->s-exp 5))( calC-num 5))
(test (parseC (symbol->s-exp 'x))( calC-id 'x))
(test (parseC '(ifzero 4 5 6))(if-greater-than-zero ( calC-num 4)( calC-num 5)( calC-num 6)))
(test (parseC '(ifzero 3 4 5))(if-greater-than-zero ( calC-num 3)( calC-num 4)( calC-num 5)))
(test (parseC '(ifzero 1 3 5))(if-greater-than-zero ( calC-num 1)( calC-num 3)( calC-num 5)))
(test (parseC '(ifzero 2 7 9))(if-greater-than-zero ( calC-num 2)( calC-num 7)( calC-num 9)))
(test (parseC '(ifzero 7 8 9))(if-greater-than-zero ( calC-num 7)( calC-num 8)( calC-num 9)))
(test (parseC '(ifzero 12 15 29))(if-greater-than-zero ( calC-num 12)( calC-num 15)( calC-num 29)))
(test (parseC '(ifzero (factorial n) 1 (* n (factorial (sub1 n)))))(if-greater-than-zero(appC 'factorial (calC-id 'n))
      (calC-num 1)(binaryOpC  '*(calC-id 'n)(appC 'factorial (appC 'sub1 (calC-id 'n))))))



;; Function Definition Structure

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body :  calC)])


; Example function definition namespace.

(define FuncDefNameSpace
  (list
   (fdC 'sqr 'x (parseC '(* x x)))
   (fdC 'sub1 'x (parseC '(+ x -1)))
   (fdC 'neg 'x (parseC '(* x -1)))
   (fdC 'double 'x (parseC '(+ x x)))
   (fdC 'quadruple 'x (parseC '(double (double x))))
   (fdC 'const5 '_ (parseC (number->s-exp 5)))
   (fdC 'factorial 'n (parseC 
                       '(ifzero n 1
                                (* n (factorial (sub1 n))))))
   (fdC 'fa 'x (parseC '(λ (x y) (+ (* 2 y)))))))

;; get-fundef : symbol (listof FunDefC) -> FunDefC
;; Purpose : To find given symbol's(function name/identifier) function definition
;; - from function definition namespace.
;; Template : Basic Structural Recursion
; (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) ...]
;    [else ...(first fds) ...(get-fundef (rest fds))])

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;; subst : number symbol  calC ->  calC
;; Purpose : To substitute symbols with expressions.
;; Template :
; (define (subst [what :  calC] [for : symbol] [in :  calC]) :  calC
;  (type-case  calC in
;    [ calC-num (n) ...]
;    [ calC-id (s) ...]
;    [appC (f a) ...]

(define (subst [what : number] [for : symbol] [in :  calC]) :  calC
  (type-case  calC in
    [calC-num (n) in]
    [calC-id (s) (cond
             [(symbol=? s for) ( calC-num what)] ;; What makes difference for eager evoluation in subst.
             [else in])]
    [appC (f a) (appC f (subst what for a))]
    [binaryOpC (op l r) (binaryOpC op
                                   (subst what for l)
                                   (subst what for r))]
    [if-greater-than-zero (p t f) (error 'sust "error")]))

;; Tests:
(test (subst 4 'x ( calC-num 5))( calC-num 5))
(test (subst 4 'x ( calC-id 'y)) ( calC-id 'y))
(test (subst 4 'x ( calC-id 'x)) ( calC-num 4))
(test (subst 5 'x ( calC-id 'x)) ( calC-num 5))
(test (subst 9 'x ( calC-id 'y)) ( calC-id 'y))

;; interp :  calC (listof FunDefC) -> number
;; Purpose : To evaluate expressions to numbers.
;; Template :
; (define (interp [e :  calC] [fds : (listof FunDefC)]) : number
;  (type-case  calC in
;    [ calC-num (n) ...]
;    [ calC-id (s) ...]
;; if-greater-than-zero :  calC  calC  calC ->  calC
;; an f statement that controls if first argument is zero 
;; -- or not, if it is zero that returns second argunment,
;; --- otherwise third argument. Partially lazy.

(define (interp [e :  calC][fds : (listof FunDefC)]) : number
  (type-case  calC e
    [ calC-num (n) n]
    [ calC-id (_) (error 'interp "problem")]
    [if-greater-than-zero (pred t f)
             (if (< 0 (interp pred fds))
                 (interp t fds)
                 (interp f fds))]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst 
                           (interp a fds) ;; Make it eager evaluation !!
                           (fdC-arg fd)
                           (fdC-body fd))
                          fds))]
    [binaryOpC (op l r)((get-op op)
                        (interp l fds)
                        (interp r fds))]
    ))

;test
(test (interp (parseC (number->s-exp 3)) empty) 3)
(test (interp (parseC '(+ 2 4)) empty) 6)
(test (interp (parseC '(* 3 4)) empty) 12)
(test (interp (if-greater-than-zero ( calC-num 1)( calC-num 1)( calC-num 1)) empty) 1)
(test (interp (if-greater-than-zero ( calC-num 2)( calC-num 3)( calC-num 4)) empty) 3)
(test (interp (if-greater-than-zero ( calC-num -4)( calC-num 7)( calC-num 9)) empty) 9)
(test (interp (if-greater-than-zero ( calC-num -6)( calC-num 7)( calC-num 8)) empty) 8)
(test (interp (if-greater-than-zero ( calC-num 3)( calC-num 5)( calC-num 6)) empty) 5)

;; eval : s-exp -> number
;; Purpose : A wrapper function to evaluate s-exp through our language.
(define (evalC (sexp : s-expression)) : number
  ;(interp (parse sexp) empty))
(interp (parseC sexp) FuncDefNameSpace))

(test (evalC '(+ 3 4)) 7)
(test (evalC '(* 3 4)) 12)
(test (evalC '(sqr 4)) 16)
(test (evalC '(neg 4)) -4)

;;----------------------------------------------------------------------------------------------

;; λ-expression grammar
;; λ-exp -> v
;; λ-exp -> (λ-exp λ-exp)
;; λ-exp -> (λ v λ-exp)
;; where v is a symbol.

;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; - λ-exp that defined above.

(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )
;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

;; parse : s-exp -> λ-exp

(define (parseλ (sexp : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (cond
         [(= 2 (length sexp-list))
          (λ-app (parseλ (first sexp-list))(parseλ (second sexp-list)))]
         [(= 3 (length sexp-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                   (s-exp-symbol? (second sexp-list)))
              (λ-def (s-exp->symbol(second sexp-list))
                     (parseλ (third sexp-list)))
              (error 'parseλ "λ-definition didnt't valid")
              )]
         [else (error 'parseλ "length λ-exp  didnt't valid")]
         ))]
    [else (error 'parseλ "λ-exp didnt't valid")]
    ))


;; Setd as a list.
;; set-union : (listof symbol) (listof symbol) -> (listof symbol)   --  find the union of two sets.
(define (set-union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; Tests:
(test (set-union empty empty) empty)
(test (set-union empty (list 'x)) (list 'x))
(test (set-union (list 'x)(list 'x 'y)) (list 'x 'y))
(test (set-union (list 'x) (list 'x)) (list 'x))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)  --   find the set difference of two sets.


(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; Tests:
(test (set-difference empty (list 'x)) empty)
(test (set-difference (list 'x) empty) (list 'x))
(test (set-difference (list 'x)(list 'x 'y)) empty)
(test (set-difference (list 'x 'y)(list 'x))(list 'y))

;; free-identifier : λ-exp -> (listof symbol)  --  find free identifiers in λ expression.

(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(set-union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))

;; Tests:
(test (free-identifier (parseλ (symbol->s-exp 'x))) (list 'x))
(test (free-identifier (parseλ '(λ x (λ y (y x))))) empty)
(test (free-identifier (parseλ '((λ f y)(λ z z)))) (list 'y))
(test (free-identifier (parseλ '(λ x x))) empty)
(test (free-identifier (parseλ '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parseλ '(λ x (λ y z)))) (list 'z))
(test (free-identifier (parseλ '(λ x y))) (list 'y))

