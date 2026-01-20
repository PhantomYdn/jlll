# JLLL Special Forms

Special forms have evaluation rules that differ from standard procedure calls. The evaluator handles them directly rather than evaluating all arguments first.

## JLLL-Specific Notes

- `define` supports optional parameters with defaults: `(param default)`
- `define` supports immediate evaluation of defaults: `(param !expr)`
- `set!` is aliased to `set`
- `letrec` is implemented as a macro

## define

Binds a value or procedure to a symbol.

### Value Definition

```lisp
(define x 10)
(define name "Alice")
(define nums '(1 2 3))
```

### Procedure Definition

Short form for defining named procedures:

```lisp
(define (square x) (* x x))

;; Equivalent to:
(define square (lambda (x) (* x x)))
```

### Multiple Body Expressions

```lisp
(define (greet name)
  (print "Greeting: ")
  (concat "Hello, " name "!"))  ; returns last expression
```

### Optional Parameters with Defaults

Parameters can have default values using pair syntax `(param default)`:

```lisp
(define (greet (name "World"))
  (concat "Hello, " name "!"))

(greet)           ; => "Hello, World!"
(greet "Alice")   ; => "Hello, Alice!"
```

Default expressions are evaluated at **invocation time** by default:

```lisp
(define counter 0)
(define (next-id (id counter))
  (set! counter (+ counter 1))
  id)

(next-id)         ; => 0
(next-id)         ; => 1
(next-id)         ; => 2
```

### Definition-Time Defaults

Use `!` to evaluate default at definition time:

```lisp
(define (make-user (id !(random 1000)))  ; random called ONCE
  id)

(make-user)       ; => 42 (same each call)
(make-user)       ; => 42
```

### Parameter-Referencing Defaults

Later parameters can reference earlier ones:

```lisp
(define (make-range start (end (+ start 10)))
  (list start end))

(make-range 5)        ; => (5 15)
(make-range 5 20)     ; => (5 20)
```

### Rest Parameters

Collect remaining arguments into a list:

```lisp
(define (sum . nums)
  (apply + nums))

(sum 1 2 3 4)     ; => 10

;; With required parameters:
(define (greet name . titles)
  (concat (apply concat titles) " " name))

(greet "Smith" "Dr." "Prof.")  ; => "Dr.Prof. Smith"
```

## lambda

Creates an anonymous procedure.

```lisp
(lambda (x) (* x x))           ; square function

((lambda (x y) (+ x y)) 3 4)   ; => 7

(define add (lambda (x y) (+ x y)))
(add 1 2)                      ; => 3
```

Lambda supports the same parameter syntax as `define`:

```lisp
;; Optional parameters
(lambda ((x 0) (y 0)) (+ x y))

;; Rest parameters
(lambda (first . rest) (cons first rest))
```

## if

Conditional evaluation.

```lisp
(if test then else)
(if test then)      ; else defaults to null
```

Examples:

```lisp
(if (> x 0) "positive" "non-positive")

(if (null? lst)
    '()
    (cdr lst))

;; Without else:
(if debug (print "debug mode"))  ; returns null if debug is false
```

Falsy values: `false`, `#f`, `null`  
Everything else is truthy (including `0`, `""`, `'()`).

## cond

Multi-branch conditional.

```lisp
(cond
  (test1 expr1 ...)
  (test2 expr2 ...)
  (else expr-default ...))
```

Example:

```lisp
(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))
```

The `else` keyword matches when no other condition matched.

## case

Value-based dispatch.

```lisp
(case expr
  ((val1 val2 ...) result1 ...)
  ((val3) result2 ...)
  (else default-result ...))
```

Example:

```lisp
(case day
  ((mon tue wed thu fri) "weekday")
  ((sat sun) "weekend")
  (else "unknown"))
```

## let

Local variable bindings.

```lisp
(let ((var1 val1)
      (var2 val2))
  body ...)
```

Bindings are evaluated in parallel (outer scope):

```lisp
(let ((x 1)
      (y 2))
  (+ x y))          ; => 3

(define x 10)
(let ((x 1)
      (y x))        ; y binds to outer x (10), not inner x (1)
  (+ x y))          ; => 11
```

## letrec

Recursive local bindings.

```lisp
(letrec ((var1 val1)
         (var2 val2))
  body ...)
```

Bindings can reference each other:

```lisp
(letrec ((even? (lambda (n)
                  (if (= n 0) true (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0) false (even? (- n 1))))))
  (even? 10))       ; => true
```

## begin

Sequence expressions, return last value.

```lisp
(begin
  expr1
  expr2
  ...
  exprN)            ; returns value of exprN
```

Example:

```lisp
(begin
  (print "Starting")
  (set! x (+ x 1))
  x)                ; returns x
```

## quote

Prevent evaluation, return data.

```lisp
(quote x)           ; => x (symbol)
(quote (1 2 3))     ; => (1 2 3) (list)

;; Reader macro shorthand:
'x                  ; => x
'(1 2 3)            ; => (1 2 3)
```

## quasiquote

Template with selective evaluation.

```lisp
(quasiquote (a b c))              ; => (a b c)
(quasiquote (a (unquote x) c))    ; => (a <value-of-x> c)

;; Reader macro shorthand:
`(a ,x c)
`(a ,@lst b)        ; splice lst into result
```

## set / set!

Modify existing binding.

```lisp
(set! var value)
(set var value)     ; alias
```

Example:

```lisp
(define x 10)
(set! x 20)
x                   ; => 20
```

Unlike `define`, `set!` requires the variable to already exist.

## apply

Apply procedure to argument list.

```lisp
(apply proc args-list)
(apply proc arg1 arg2 args-list)
```

Examples:

```lisp
(apply + '(1 2 3))           ; => 6
(apply max '(3 1 4 1 5))     ; => 5
```
