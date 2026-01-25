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

## Exception Handling

JLLL provides two styles of exception handling: Java-style `try`/`catch` and Scheme-style `guard`.

### try

Java-style exception handling with catch and finally clauses.

```lisp
(try
  body...
  (catch [type-spec] var handler...)...
  (finally cleanup...))
```

**Components:**

- `body` - Expressions to evaluate
- `catch` - Exception handler clause (zero or more)
  - `type-spec` - Optional: string class name or predicate function
  - `var` - Symbol to bind the caught exception
  - `handler` - Expressions to evaluate when caught
- `finally` - Optional cleanup code that always runs

**Examples:**

```lisp
;; Basic catch-all
(try
  (risky-operation)
  (catch e
    (println "Error: " (exception-message e))
    default-value))

;; Type-specific catch by class name
(try
  (invoke file "read")
  (catch "java.io.IOException" e
    (println "IO error: " (exception-message e))
    null)
  (catch e
    (println "Other error: " e)
    null))

;; Predicate-based catch
(try
  (network-call)
  (catch (lambda (e) (string-contains? (exception-message e) "timeout")) e
    (println "Timeout, retrying...")
    (retry))
  (catch e
    (fail e)))

;; With finally (always runs)
(try
  (define conn (open-connection))
  (use-connection conn)
  (finally
    (close-connection conn)))

;; Multiple catch clauses are tried in order
(try
  (process-data)
  (catch "java.io.FileNotFoundException" e "file not found")
  (catch "java.io.IOException" e "io error")
  (catch "ru.ydn.jlll.common.JlllException" e "jlll error")
  (catch e "unknown error"))  ; catch-all must be last
```

**Type Matching:**

- **Omitted** - Catches all exceptions: `(catch e handler...)`
- **String** - Matches by Java class name: `(catch "java.io.IOException" e ...)`
- **Predicate** - Function that takes exception, returns boolean: `(catch (lambda (e) ...) e ...)`

When matching by class name, both the exception itself and its underlying cause are checked.

### guard

Scheme-style exception handling (SRFI-34 inspired). Uses cond-like clauses to match exceptions.

```lisp
(guard (var
         (test1 handler1...)
         (test2 handler2...)
         ...
         (else default...))
  body...)
```

**Components:**

- `var` - Symbol to bind the caught exception
- `test` - Expression evaluated with var bound; if true, run handler
- `else` - Optional catch-all clause (must be last)
- `body` - Expressions to evaluate

If no clause matches, the exception is re-raised.

**Examples:**

```lisp
;; Basic guard with else
(guard (err
         (else 
           (println "Caught: " (exception-message err))
           "error"))
  (risky-operation))

;; Multiple conditions
(guard (err
         ((string-contains? (exception-message err) "not found")
          (println "Item not found")
          default-value)
         ((string-contains? (exception-message err) "permission")
          (println "Access denied")
          (request-permission))
         (else
          (raise err)))  ; re-raise unknown errors
  (load-resource name))

;; Guard with type checking
(guard (err
         ((instanceof? err "java.io.IOException")
          "io error")
         ((exception? err)
          "other exception"))
  (process-file))
```

### Exception Accessors

| Primitive | Description |
|-----------|-------------|
| `exception?` | Test if value is a Throwable |
| `exception-message` | Get the error message string |
| `exception-cause` | Get the underlying Java cause (or null) |

### raise and error

```lisp
;; Raise an exception with any value
(raise "error message")
(raise some-exception)

;; Convenience: concatenate args into error message
(error "Item " item " not found in " collection)
```

## Continuations

### call/cc (call-with-current-continuation)

Captures the current continuation as a first-class procedure.

```lisp
(call/cc (lambda (k) body...))
```

The continuation `k` is a procedure that, when called with a value, immediately returns that value from the `call/cc` expression, abandoning the current computation.

**Basic escape:**

```lisp
;; k escapes the computation, returning 3 from call/cc
;; Result: (+ 1 3) = 4
(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))  ; => 4

;; If k is not called, normal return
(+ 1 (call/cc (lambda (k) (+ 2 2))))      ; => 5
```

**Early exit from computation:**

```lisp
;; Search for an item, exit early when found
(define (find-item lst pred)
  (call/cc (lambda (return)
    (for-each (lambda (x)
                (if (pred x) (return x)))
              lst)
    false)))  ; not found
```

**Saved continuations:**

Continuations can be saved and invoked multiple times:

```lisp
(define saved-k false)

;; First evaluation: saves k, returns 3
(+ 1 (call/cc (lambda (k) 
                (set! saved-k k) 
                (k 2))))              ; => 3

;; Later: invoke saved continuation with 10
(saved-k 10)                          ; => 11 (+ 1 10)

;; Can be called again
(saved-k 100)                         ; => 101
```

**Interaction with exceptions:**

Continuations pass through `try`/`catch` and `guard` without being caught:

```lisp
;; Continuation escapes try block - not caught as exception
(+ 1 (try 
       (call/cc (lambda (k) (k 41))) 
       (catch e "not caught")))       ; => 42

;; But errors inside call/cc body are still catchable
(call/cc (lambda (k) 
  (try 
    (raise "error") 
  (catch e "caught"))))               ; => "caught"

;; finally still runs when continuation escapes
(define finally-ran 0)
(+ 1 (try 
       (call/cc (lambda (k) (k 0))) 
       (finally (set! finally-ran 1))))
finally-ran                           ; => 1
```

**Nested continuations:**

```lisp
;; Outer escape
(call/cc (lambda (outer) 
  (+ 1 (call/cc (lambda (inner) 
    (outer 42))))))                   ; => 42

;; Inner escape
(call/cc (lambda (outer) 
  (+ 1 (call/cc (lambda (inner) 
    (inner 9))))))                    ; => 10
```

**Implementation notes:**

- `call/cc` captures the "rest of the computation" at the point it is called
- Continuations are one-shot escape continuations with replay capability
- Each invocation of a saved continuation replays from the call/cc point
- Alias: `call-with-current-continuation`
