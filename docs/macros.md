# JLLL Macros

Macros transform code at expansion time, before evaluation. They receive unevaluated arguments and return code that will be evaluated.

## JLLL-Specific Notes

- Macros are defined with `defmacro`, not `define-syntax`
- Quasiquote (`` ` ``) with unquote (`,`) and unquote-splicing (`,@`) are the primary tools
- No hygiene - macros use simple substitution
- Macro expansion can be inspected with `jlll-macro-expand`

## Defining Macros

### Basic Syntax

```lisp
(defmacro (name params ...) body ...)
```

The macro body should return a list representing the code to be evaluated.

### Simple Example

```lisp
(defmacro (when test . body)
  `(if ,test (begin ,@body) null))

;; Usage:
(when (> x 0)
  (print "positive")
  (print "number"))

;; Expands to:
(if (> x 0) (begin (print "positive") (print "number")) null)
```

## Quasiquote for Code Templates

### Basic Quasiquote

```lisp
`(a b c)              ; => (a b c)
```

### Unquote (`,`)

Insert evaluated expression:

```lisp
(define x 10)
`(value is ,x)        ; => (value is 10)

(defmacro (square x)
  `(* ,x ,x))

(square 5)            ; => (* 5 5) => 25
```

### Unquote-Splicing (`,@`)

Splice list elements:

```lisp
(define nums '(1 2 3))
`(start ,@nums end)   ; => (start 1 2 3 end)

(defmacro (when test . body)
  `(if ,test (begin ,@body) null))

;; body = ((print "a") (print "b"))
;; ,@body splices into: (begin (print "a") (print "b"))
```

## Common Macro Patterns

### Wrapping Expressions

```lisp
(defmacro (unless test . body)
  `(if (not ,test) (begin ,@body) null))

(unless (null? lst)
  (print (car lst)))
```

### Creating Bindings

```lisp
(defmacro (let1 var val . body)
  `((lambda (,var) ,@body) ,val))

(let1 x 10
  (+ x 5))            ; => 15

;; Expands to:
((lambda (x) (+ x 5)) 10)
```

### Defining Variables with Computed Names

```lisp
(defmacro (define-getter name field)
  `(define (,name obj) (peek obj ,(symbol->string field))))

(define-getter get-x x)
;; Expands to:
(define (get-x obj) (peek obj "x"))
```

## The `letrec` Macro

JLLL's `letrec` is implemented as a macro:

```lisp
(defmacro (letrec clauses . body)
  `((lambda ,(map car clauses)
      ,@(map (lambda (clause)
               `(set! ,(car clause) ,(cadr clause)))
             clauses)
      ,@body)
    ,@(map (lambda (c) #f) clauses)))

;; Usage:
(letrec ((even? (lambda (n)
                  (if (= n 0) true (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0) false (even? (- n 1))))))
  (even? 10))

;; Expands to:
((lambda (even? odd?)
   (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
   (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
   (even? 10))
 #f #f)
```

## Debugging Macros

### `jlll-macro-expand`

Expand a macro without evaluating the result:

```lisp
(defmacro (when test . body)
  `(if ,test (begin ,@body) null))

(jlll-macro-expand when (> x 0) (print "yes"))
; => (if (> x 0) (begin (print "yes")) null)
```

### `describe`

Get information about a macro:

```lisp
(describe when)
; => "Macros.
;     Doc: 
;     Arguments: (test . body)
;     Body: (quasiquote (if (unquote test) ...))"
```

## Macro vs Procedure

| Aspect | Macro | Procedure |
|--------|-------|-----------|
| Arguments | Received unevaluated | Received evaluated |
| Execution time | Expansion time | Call time |
| Return value | Code to be evaluated | Final result |
| Use case | Syntax transformation | Computation |

Example demonstrating the difference:

```lisp
;; Macro: arguments not evaluated
(defmacro (debug expr)
  `(begin
     (print "Evaluating: " ',expr)
     ,expr))

(debug (+ 1 2))
; Prints: Evaluating: (+ 1 2)
; Returns: 3

;; Procedure: arguments evaluated first
(define (debug-fn expr)
  (begin
    (print "Value: " expr)
    expr))

(debug-fn (+ 1 2))
; Prints: Value: 3
; Returns: 3
```

## Avoiding Multiple Evaluation

A common pitfall is evaluating an argument multiple times:

```lisp
;; BAD: x evaluated twice
(defmacro (square-bad x)
  `(* ,x ,x))

(square-bad (begin (print "eval!") 5))
; Prints "eval!" twice!

;; GOOD: use let to evaluate once
(defmacro (square-good x)
  `(let ((temp ,x))
     (* temp temp)))

(square-good (begin (print "eval!") 5))
; Prints "eval!" once
```

## Macro Hygiene Warning

JLLL macros are not hygienic. Variable capture can occur:

```lisp
(defmacro (bad-swap a b)
  `(let ((temp ,a))
     (set! ,a ,b)
     (set! ,b temp)))

;; Problem: if user has variable named 'temp', it gets captured
(define temp 100)
(define x 1)
(define y 2)
(bad-swap x y)
; temp variable interferes!
```

Use unique names or `gensym` (if available) for macro-internal variables.
