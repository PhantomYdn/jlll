# JLLL Procedures

This document covers procedure definition and invocation, including JLLL's keyword argument system.

## JLLL-Specific Features

JLLL procedures support features not found in standard Scheme:

- **Keyword arguments at call site:** `(func :arg value)`
- **Optional parameters with defaults:** `(param default)`
- **Definition-time vs invocation-time defaults:** `!expr` vs `expr`
- **Unknown keywords passed to environment:** Extra `:key value` pairs are available in the procedure body
- **Mixed positional and keyword:** Later bindings override earlier ones

## Defining Procedures

### Basic Definition

```lisp
(define (name params ...) body ...)

;; Example
(define (add x y) (+ x y))
(add 1 2)        ; => 3
```

### Lambda Expression

```lisp
(lambda (params ...) body ...)

;; Example
(define add (lambda (x y) (+ x y)))
((lambda (x) (* x x)) 5)  ; => 25
```

## Parameter Types

### Required Parameters

Must be provided on every call:

```lisp
(define (greet first last)
  (concat "Hello, " first " " last))

(greet "John" "Doe")      ; => "Hello, John Doe"
(greet "John")            ; ERROR: missing argument
```

### Optional Parameters with Defaults

Use pair syntax `(param default)`:

```lisp
(define (greet (name "World"))
  (concat "Hello, " name "!"))

(greet)                   ; => "Hello, World!"
(greet "Alice")           ; => "Hello, Alice!"
```

Multiple optional parameters:

```lisp
(define (make-point (x 0) (y 0) (color "black"))
  (list x y color))

(make-point)              ; => (0 0 "black")
(make-point 10)           ; => (10 0 "black")
(make-point 10 20)        ; => (10 20 "black")
(make-point 10 20 "red")  ; => (10 20 "red")
```

### Rest Parameters

Collect remaining arguments with `. rest`:

```lisp
(define (sum . nums)
  (apply + nums))

(sum)                     ; => 0
(sum 1)                   ; => 1
(sum 1 2 3 4 5)           ; => 15
```

Combined with required parameters:

```lisp
(define (log level . messages)
  (concat "[" level "] " (apply concat messages)))

(log "INFO" "User " "logged in")  ; => "[INFO] User logged in"
```

## Default Value Evaluation

### Invocation-Time Evaluation (Default)

Default expressions are evaluated each time the procedure is called:

```lisp
(define (log-entry (timestamp (current-time)))
  (list "entry" timestamp))

(log-entry)               ; => ("entry" <current-time>)
;; ... wait ...
(log-entry)               ; => ("entry" <different-time>)
```

Defaults can reference earlier parameters:

```lisp
(define (make-range start (end (+ start 10)))
  (list start end))

(make-range 5)            ; => (5 15)
(make-range 100)          ; => (100 110)
(make-range 5 50)         ; => (5 50)
```

### Definition-Time Evaluation

Use `!` to evaluate once at definition:

```lisp
(define (make-user (id !(random 1000000)))
  (list "user" id))

;; random called once when define is evaluated
(make-user)               ; => ("user" 847293)
(make-user)               ; => ("user" 847293)  ; same id!
```

Comparison:

```lisp
;; Invocation-time: new random each call
(define (gen-id-dynamic (id (random 1000))) id)
(gen-id-dynamic)          ; => 374
(gen-id-dynamic)          ; => 891

;; Definition-time: fixed at definition
(define (gen-id-fixed (id !(random 1000))) id)
(gen-id-fixed)            ; => 127
(gen-id-fixed)            ; => 127
```

## Keyword Arguments

### When Keywords Are Processed

Keyword argument processing activates when:
1. The procedure has **optional parameters with defaults**, OR
2. The call contains keywords AND the procedure has **regular (non-rest) parameters**

**Pure rest parameter procedures** (like `list`) do NOT process keywords:

```lisp
(define (list . items) items)   ; pure rest - no keyword processing
(list :a :b :c)                 ; => (:a :b :c) - keywords as values

(define (foo x) x)              ; has regular param - processes keywords  
(foo :x 10)                     ; => 10
```

### Basic Keyword Calls

Pass arguments by name using `:keyword value`:

```lisp
(define (greet name)
  (concat "Hello, " name "!"))

(greet "Alice")           ; positional
(greet :name "Alice")     ; keyword - same result
```

### Keywords with Optional Parameters

```lisp
(define (make-point (x 0) (y 0))
  (list x y))

(make-point)                  ; => (0 0)
(make-point 10 20)            ; => (10 20)
(make-point :x 10)            ; => (10 0)
(make-point :y 20)            ; => (0 20)
(make-point :y 20 :x 10)      ; => (10 20) - order doesn't matter
```

### Mixed Positional and Keyword

Positional and keyword arguments can be mixed. Later bindings override earlier:

```lisp
(define (foo a b) (list a b))

(foo 1 2)                 ; => (1 2)
(foo :a 10 :b 20)         ; => (10 20)
(foo 1 :b 20)             ; => (1 20)
(foo :a 10 2)             ; => (10 2)

;; Override: positional 1 binds a, then :a 5 rebinds a
(foo 1 :a 5 2)            ; => (5 2)

;; Override: :a 5 binds a, then positional 10 rebinds a
(foo :a 5 10 20)          ; => (10 20)
```

### Keywords with Rest Parameters

Keyword arguments are extracted before rest collection:

```lisp
(define (process a . rest)
  (list a rest))

(process 1 2 3 4)             ; => (1 (2 3 4))
(process :a 10 2 3 4)         ; => (10 (2 3 4))
(process 1 :a 10 2 3 4)       ; => (10 (1 2 3 4))
```

### Unknown Keywords

Keywords not matching any parameter are added to the procedure's environment:

```lisp
(define (flexible x)
  (if (jlll-bound? 'extra)
      (+ x extra)
      x))

(flexible 10)                 ; => 10
(flexible 10 :extra 5)        ; => 15
(flexible :x 10 :extra 5)     ; => 15
```

This enables extensible APIs:

```lisp
(define (http-request url (method "GET"))
  (list url method
        (if (jlll-bound? 'timeout) timeout 30)
        (if (jlll-bound? 'headers) headers '())))

(http-request "http://example.com")
; => ("http://example.com" "GET" 30 ())

(http-request "http://example.com" :method "POST" :timeout 60)
; => ("http://example.com" "POST" 60 ())
```

## Procedure Introspection

### describe

Get information about a procedure:

```lisp
(define (add x y) (+ x y))
(describe add)
; => "Compaund procedure.
;     Arguments: (x y)
;     Body: (+ x y)"
```

### jlll-extract-body

Extract the body of a procedure:

```lisp
(define (square x) (* x x))
(jlll-extract-body square)
; => (lambda (x) (* x x))
```

## Higher-Order Procedures

### Passing Procedures

```lisp
(define (apply-twice f x)
  (f (f x)))

(define (add1 x) (+ x 1))
(apply-twice add1 5)      ; => 7
```

### Returning Procedures

```lisp
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(add5 10)                 ; => 15
```

### Map, Filter, Reduce

```lisp
(map (lambda (x) (* x x)) '(1 2 3 4))
; => (1 4 9 16)

(filter (lambda (x) (> x 2)) '(1 2 3 4))
; => (3 4)

;; No built-in reduce, but can be defined:
(define (reduce f init lst)
  (if (null? lst)
      init
      (reduce f (f init (car lst)) (cdr lst))))

(reduce + 0 '(1 2 3 4))   ; => 10
```

## Summary: Parameter Binding Order

When a procedure is called:

1. **Extract keyword arguments** from the argument list
2. **Bind positional arguments** left-to-right to unbound parameters
3. **Keyword arguments override** any positional bindings
4. **Evaluate defaults** for any remaining unbound optional parameters (left-to-right)
5. **Collect rest arguments** (excluding extracted keywords)
6. **Add unknown keywords** to the procedure environment
7. **Evaluate body** in the resulting environment
