#lang racket
(define LAMBDA (string->symbol "\u03BB"))

#| don't forget to include the first-line above!
	otherwise there might be troubles
	this file is directly runnable by either
	    racket hello.ss
	or, open it in DrRacket and click run

    for basic syntax introduction, please see slides and hello.ss
|#

(define (lambda? x)
    (cond
        [(eq? x 'lambda) #t]
        [(eq? x 'λ) #t]
        [else #f]
    )
)

(define (normalize-lambda-sign x y)
    (cond
        [(eq? x 'λ) 'λ]
        [(eq? y 'λ) 'λ]
        [else 'lambda]
    )
)

(define (check data x)
    (if
        (eq? data '())
        #f
        (if
            (eq? x (car data))
            #t
            (check (cdr data) x)
        )
    )
)

(define (replace-all old new list)
    (map (lambda (item) (replace old new item)) list)
)

(define (replace-lambda old new body)
    (let*
        (
            [head (car body)]
            [lambdalist (cadr body)]
            [exp (caddr body)]
        )
        (if
            (check lambdalist old)
            body
            (list head lambdalist (replace old new exp))        
        )
    )
)

(define (replace old new body)
    (if 
        (symbol? body)
        (if
            (eq? old body)
            new
            body
        )
        (if
            (list? body)
            (if
                (eq? body '())
                '()
                (let*
                    (
                        [head (car body)]                        
                    )
                    (cond
                        [(eq? head 'quote) body]
                        [(eq? head 'if) (cons 'if (replace-all old new (cdr body)))]
                        [(lambda? head) (replace-lambda old new body)]
                        [else (replace-all old new body)]
                    )
                )
            )
            body
        )
    )
)

(define (expr-compare-lambda-lambda-rest merge xlist ylist xbody ybody)
    (if (eq? xlist '())
        (cons (reverse merge) (cons (expr-compare xbody ybody) '()))
        (let*
            (
                [xhead (car xlist)]
                [yhead (car ylist)]
                [xtail (cdr xlist)]
                [ytail (cdr ylist)]
            )
            (if
                (eq? xhead yhead)
                (expr-compare-lambda-lambda-rest (cons xhead merge) xtail ytail xbody ybody)
                (let*
                    (
                        [merged (string->symbol (string-append (symbol->string xhead) "!" (symbol->string yhead)))]
                    )
                    (expr-compare-lambda-lambda-rest (cons merged merge) xtail ytail (replace xhead merged xbody) (replace yhead merged ybody))
                )
            )
        )
    )
)

(define (expr-compare-lambda-lambda x y)
    (let*
        (
            [xhead (car x)]
            [yhead (car y)]
            [xlist (cadr x)]
            [ylist (cadr y)]
            [xbody (caddr x)]
            [ybody (caddr y)]
        )
        (if
            (= (length xlist) (length ylist))
            (cons (normalize-lambda-sign xhead yhead) (expr-compare-lambda-lambda-rest '() xlist ylist xbody ybody))
            (list 'if '% x y)
        )
    )
)

(define (expr-compare-list-list x y)
    ; Precondition - x and y are not equal
    (cond
        ; Anything inside quote is just data and they are not equal
        [(eq? (car x) 'quote) (list 'if '% x y)]
        [(eq? (car y) 'quote) (list 'if '% x y)]

        ; If both x and y are lambda, then we play the lambda game
        [(and (lambda? (car x)) (lambda? (car y))) (expr-compare-lambda-lambda x y)]

        ; If only one of x and y is lambda
        [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% x y)]

        ; If both x and y are 'if', then we can compare the list literally
        [(and (eq? (car x) 'if) (eq? (car y) 'if)) (expr-compare-list-list-match x y)]

        ; If only one of x and y is 'if', 
        [(or (eq? (car x) 'if) (eq? (car y) 'if)) (list 'if '% x y)]

        ; The rest is function application - if the length is equal we can do this
        [(= (length x) (length y)) (expr-compare-list-list-match x y)]

        ; Otherwise 
        [else (list 'if '% x y)]
    )
)

(define (expr-compare-list-list-match x y)
    ; Precondition - x and y are lists and have same length
    (if (eq? x '())
        '()
        (cons (expr-compare (car x) (car y)) (expr-compare-list-list-match (cdr x) (cdr y)))
    )
)

(define (expr-compare-atom-atom x y)
    (cond
        [(and (boolean? x) (boolean? y)) 
             (if x
                 (if y #t '%)
                 (if y '(not %) #f)
             )
        ]
        [else (list 'if '% x y)]
    )
)

(define (expr-compare x y)
    (cond
        [(equal? x y) x]
        [(equal? x '()) (list 'if '% x y)]
        [(equal? y '()) (list 'if '% x y)]
        [(and (list? x) (list? y)) (expr-compare-list-list x y)]
        [(and (not (list? x)) (list? y)) (list 'if '% x y)]
        [(and (list? x) (not (list? y))) (list 'if '% x y)]
        [else (expr-compare-atom-atom x y)]
    )
)

; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y)
  (and
    (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
    (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
  )
)

; need to have more, and strong, test-cases here
(define test-expr-x
    (list 12
          12
          #t
          #f
          #t
          #f
          '(if #t '(1) (if (#f) 1 2))
     )
)

(define test-expr-y
    (list 
          12
          20
          #t
          #f
          #f
          #t
          '(if #t '(2) (if (#t) 1 3))
     )
)

(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
