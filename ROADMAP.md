# JLLL Feature Roadmap

This roadmap proposes enhancements to make JLLL more comfortable for daily programming.
Features are organized by category, with categories ordered by priority.

**Legend:**
- [ ] Not implemented
- [x] Implemented
- Priorities: Critical > Important > Nice to Have

---

## 1. Exception Handling (Critical)

JLLL currently has **no exception handling** - errors propagate to top level and terminate execution.

### Option A: Scheme-style `guard` (SRFI-34)

The standard Scheme approach using continuations:

```lisp
;; guard - structured exception handling
(guard (err
         ((string? err) (concat "String error: " err))
         (else (concat "Unknown error: " (to-string err))))
  (risky-operation))

;; raise - signal an exception
(raise "Something went wrong")

;; with-exception-handler - low-level handler installation
(with-exception-handler
  (lambda (err) (println "Caught: " err))
  (lambda () (risky-operation)))
```

**Pros:** Standard Scheme, composable, clean syntax
**Cons:** Requires `call/cc` (call-with-current-continuation)

### Option B: Java-style `try`/`catch`

Simpler approach that maps directly to Java:

```lisp
;; try/catch - Java-style exception handling
(try
  (risky-operation)
  (catch err
    (println "Error: " err)
    default-value))

;; try with specific exception types
(try
  (invoke obj "riskyMethod")
  (catch "java.io.IOException" e
    (println "IO error: " (invoke e "getMessage")))
  (catch "Exception" e
    (println "Other error: " e)))

;; try/finally
(try
  (define resource (acquire-resource))
  (use-resource resource)
  (finally
    (release-resource resource)))
```

**Pros:** Familiar to Java developers, easy to implement
**Cons:** Less Lispy, doesn't compose as well

### Checklist

- [x] `raise` - Signal an exception
- [x] `guard` - Scheme-style structured exception handling (Option A)
- [x] `with-exception-handler` - Install exception handler for dynamic extent
- [x] `try`/`catch`/`finally` - Java-style exception handling (Option B)
- [x] `error` - Convenience for `(raise (make-error message))`
- [x] `call/cc` - Call with current continuation (enables advanced control flow)

---

## 2. Input Operations (Critical)

Essential for interactive programs. All input functions accept an optional port argument
and return the EOF object (`#<eof>`) at end of input.

### Proposed Implementation

```lisp
;; Read a single JLLL expression from input
(read)                    ; from *stdin*
(read port)               ; from specific port

;; Read a line as string
(read-line)               ; from *stdin*
(read-line port)          ; from specific port

;; Read single character
(read-char)
(read-char port)

;; Peek at next character without consuming
(peek-char)
(peek-char port)

;; Check if input available
(char-ready?)
(char-ready? port)
```

**Implementation notes:**
- `read` uses JlllTokenizer to parse expressions
- `*stdin*` is bound to BufferedReader (supports mark/reset for peek-char)
- Characters are returned as single-character strings (JLLL has no char type)

### Checklist

- [x] `read` - Read and parse JLLL expression
- [x] `read-line` - Read line as string
- [x] `read-char` - Read single character
- [x] `peek-char` - Peek next character
- [x] `newline` - Output newline (trivial: print empty line)
- [x] `char-ready?` - Check if input available
- [x] `eof-object?` - Test for end-of-file

---

## 3. String Operations (Critical)

Only `concat` exists. Everything else requires verbose Java interop.

### Proposed Implementation

```lisp
;; Length
(string-length "hello")           ; => 5

;; Extraction
(substring "hello" 1 3)           ; => "el"
(string-ref "hello" 0)            ; => #\h (or "h" if no char type)

;; Search
(string-index "hello" "l")        ; => 2 (or #f if not found)
(string-contains? "hello" "ell")  ; => true

;; Transformation
(string-upcase "hello")           ; => "HELLO"
(string-downcase "HELLO")         ; => "hello"
(string-trim "  hello  ")         ; => "hello"
(string-trim-left "  hello")      ; => "hello"
(string-trim-right "hello  ")     ; => "hello"

;; Manipulation
(string-replace "hello" "l" "L")  ; => "heLLo"
(string-split "a,b,c" ",")        ; => ("a" "b" "c")
(string-join '("a" "b" "c") ",")  ; => "a,b,c"

;; Conversion
(string->number "42")             ; => 42
(string->number "3.14")           ; => 3.14
(string->number "ff" 16)          ; => 255 (with radix)
(number->string 42)               ; => "42"
(number->string 255 16)           ; => "ff" (hex)
(string->list "abc")              ; => ("a" "b" "c")
(list->string '("a" "b" "c"))     ; => "abc"

;; Comparison
(string=? "a" "a")                ; => true
(string<? "a" "b")                ; => true
(string>? "b" "a")                ; => true
(string-ci=? "Hello" "hello")     ; => true (case-insensitive)

;; Construction
(make-string 5 "x")               ; => "xxxxx"
(string-append "a" "b" "c")       ; => "abc" (variadic, alias for concat)
```

**Implementation notes:**
- Most are thin wrappers around Java String methods
- JLLL doesn't have a character type - use single-char strings
- `string->list` returns list of single-char strings

### Checklist

- [x] `string-length` - Get string length
- [x] `substring` - Extract substring
- [x] `string-ref` - Character at index (returns single-char string)
- [x] `string-index` - Find substring position
- [x] `string-contains?` - Check if contains substring
- [x] `string-upcase` / `string-downcase` - Case conversion
- [x] `string-trim` / `string-trim-left` / `string-trim-right`
- [x] `string-replace` - Replace occurrences
- [x] `string-split` - Split by delimiter
- [x] `string-join` - Join with delimiter
- [x] `string->number` / `number->string` - Conversion with optional radix
- [x] `string->list` / `list->string` - Conversion
- [x] `string=?` / `string<?` / `string>?` - Comparison
- [x] `string<=?` / `string>=?` - Comparison
- [x] `string-ci=?` / `string-ci<?` - Case-insensitive comparison
- [x] `string-append` - Variadic concatenation (alias for `concat`)
- [x] `make-string` - Create string of repeated chars
- [x] `string-empty?` - Test for empty string

---

## 4. Comparison and Numeric Predicates (Important)

Missing standard operators that require awkward workarounds.

### Proposed Implementation

```lisp
;; Comparison (variadic, like Scheme R5RS)
(<= 1 2 3)                        ; => true (monotonically non-decreasing)
(>= 3 2 1)                        ; => true (monotonically non-increasing)

;; Numeric predicates
(zero? 0)                         ; => true
(positive? 5)                     ; => true
(negative? -3)                    ; => true
(even? 4)                         ; => true
(odd? 3)                          ; => true

;; Integer division (R5RS)
(quotient 13 4)                   ; => 3
(remainder 13 4)                  ; => 1   (sign follows dividend)
(modulo 13 4)                     ; => 1   (sign follows divisor)
(remainder -13 4)                 ; => -1
(modulo -13 4)                    ; => 3

;; Other math
(expt 2 10)                       ; => 1024
(gcd 12 18)                       ; => 6
(lcm 4 6)                         ; => 12
```

**Implementation notes:**
- `<=` and `>=` are simple: `(define (<= a b) (or (< a b) (= a b)))`
- Or implement as variadic in Java for better performance
- `modulo` vs `remainder`: differ for negative numbers (R5RS spec)
- Java's `%` operator is `remainder`, not `modulo`

### Checklist

- [x] `<=` - Less than or equal
- [x] `>=` - Greater than or equal
- [x] `zero?` - Test for zero
- [x] `positive?` - Test for positive
- [x] `negative?` - Test for negative
- [x] `even?` - Test for even integer
- [x] `odd?` - Test for odd integer
- [x] `quotient` - Integer division (truncate toward zero)
- [x] `remainder` - Remainder (sign follows dividend)
- [x] `modulo` - Modulo (sign follows divisor)
- [x] `expt` - Exponentiation
- [x] `gcd` - Greatest common divisor
- [x] `lcm` - Least common multiple

---

## 5. Type Predicates (Important)

Complete the predicate library. Some are marked as TODO in `predicates.jlll`.

### Proposed Implementation

```lisp
(number? 42)                      ; => true
(number? 3.14)                    ; => true
(integer? 42)                     ; => true
(integer? 3.14)                   ; => false
(pair? '(a . b))                  ; => true
(pair? '())                       ; => false (empty list is not a pair)
(atom? 'x)                        ; => true (not a pair)
(atom? '(a b))                    ; => false
```

### Checklist

- [x] `number?` - Test for any number
- [x] `integer?` - Test for integer
- [x] `real?` - Test for real number (non-complex)
- [x] `pair?` - Test for cons pair (note: `'()` is NOT a pair)
- [x] `atom?` - Test for non-pair (convenience)
- [ ] `char?` - Test for character (if char type added)
- [x] `vector?` - Test for Java array/vector
- [x] `port?` - Test for input/output port
- [x] `input-port?` / `output-port?` - Specific port types

---

## 6. List Utilities (Important)

Essential list operations missing from the standard library.

### Proposed Implementation

```lisp
;; Access by index
(list-ref '(a b c d) 2)           ; => c
(list-tail '(a b c d) 2)          ; => (c d)

;; Search (return sublist starting at match, or #f)
(member 'b '(a b c))              ; => (b c)
(member 'd '(a b c))              ; => false
(memq 'b '(a b c))                ; => (b c) using eq?

;; Association list lookup
(assoc 'b '((a . 1) (b . 2)))     ; => (b . 2)
(assoc 'd '((a . 1) (b . 2)))     ; => false
(assq 'b '((a . 1) (b . 2)))      ; => (b . 2) using eq?

;; Higher-order functions
(for-each println '(1 2 3))       ; like map but for side effects
(fold-left + 0 '(1 2 3 4))        ; => 10 (left fold)
(fold-right cons '() '(1 2 3))    ; => (1 2 3) (right fold)
(reduce + '(1 2 3 4 5))           ; => 15 (fold with first element as init)

;; Predicates on lists
(any positive? '(-1 0 1))         ; => true (at least one matches)
(every positive? '(1 2 3))        ; => true (all match)
(find positive? '(-1 0 1 2))      ; => 1 (first match)

;; Construction
(range 5)                         ; => (0 1 2 3 4)
(range 1 5)                       ; => (1 2 3 4)
(range 0 10 2)                    ; => (0 2 4 6 8)
(make-list 3 'x)                  ; => (x x x)
(iota 5)                          ; => (0 1 2 3 4) SRFI-1 alias

;; Transformation
(take '(a b c d e) 3)             ; => (a b c)
(drop '(a b c d e) 3)             ; => (d e)
(take-while positive? '(1 2 -1 3)) ; => (1 2)
(drop-while positive? '(1 2 -1 3)) ; => (-1 3)
(flatten '((1 2) (3 (4 5))))      ; => (1 2 3 4 5)
(zip '(1 2 3) '(a b c))           ; => ((1 a) (2 b) (3 c))

;; Sorting
(sort '(3 1 4 1 5) <)             ; => (1 1 3 4 5)
(sort-by car '((3 a) (1 b) (2 c))); => ((1 b) (2 c) (3 a))

;; Set operations
(remove 2 '(1 2 3 2 4))           ; => (1 3 4) - remove all occurrences
(delete 2 '(1 2 3 2 4))           ; => (1 3 2 4) - remove first occurrence
(remove-duplicates '(1 2 1 3 2))  ; => (1 2 3)

;; Variadic append (current only takes 2 args)
(append '(1) '(2) '(3))           ; => (1 2 3)
```

### Checklist

- [x] `list-ref` - Element at index
- [x] `list-tail` - Sublist from index
- [x] `member` / `memq` / `memv` - Search for element
- [x] `assoc` / `assq` / `assv` - Association list lookup
- [x] `for-each` - Map for side effects
- [x] `fold-left` / `fold-right` - Fold operations
- [x] `reduce` - Fold with first element as initial value
- [x] `any` / `every` - Test predicates across list
- [x] `find` - Find first matching element
- [x] `range` / `iota` - Generate numeric sequence
- [x] `make-list` - Create list of repeated elements
- [x] `take` / `drop` - First/remaining n elements
- [x] `take-while` / `drop-while` - Predicate-based take/drop
- [x] `flatten` - Flatten nested lists
- [x] `zip` - Combine lists pairwise
- [x] `unzip` - Split list of pairs
- [x] `sort` / `sort-by` - Sort list
- [x] `remove` / `delete` - Remove elements
- [x] `remove-duplicates` - Remove duplicates
- [x] Variadic `append` - Extend current 2-arg version (as `append*`)

---

## 7. File and URL I/O (Important)

Currently requires verbose Java interop for any file operation.

### Proposed Implementation

```lisp
;; Simple read/write (like Clojure's slurp/spit)
(slurp "file.txt")                    ; => entire file as string
(slurp "https://example.com/data")    ; => URL content as string
(slurp "classpath:config.jlll")       ; => classpath resource

(spit "file.txt" "content")           ; write/overwrite file
(spit "file.txt" "more" :append true) ; append to file

;; Port-based I/O (Scheme R5RS style)
(define in (open-input-file "file.txt"))
(read-line in)
(close-input-port in)

(define out (open-output-file "out.txt"))
(display "hello" out)
(newline out)
(close-output-port out)

;; With automatic resource cleanup
(call-with-input-file "file.txt"
  (lambda (port)
    (read-line port)))

(call-with-output-file "out.txt"
  (lambda (port)
    (display "hello" port)))

;; File system operations
(file-exists? "file.txt")             ; => true/false
(directory? "mydir")                  ; => true/false
(file-readable? "file.txt")           ; => true/false
(file-writable? "file.txt")           ; => true/false

;; File manipulation
(delete-file "file.txt")
(rename-file "old.txt" "new.txt")
(copy-file "src.txt" "dst.txt")
(make-directory "newdir")
(file-size "file.txt")                ; => size in bytes
(directory-list "mydir")              ; => list of filenames

;; Path manipulation
(path-join "dir" "subdir" "file.txt") ; => "dir/subdir/file.txt"
(path-directory "/a/b/c.txt")         ; => "/a/b"
(path-filename "/a/b/c.txt")          ; => "c.txt"
(path-extension "/a/b/c.txt")         ; => "txt"
```

**Implementation notes:**
- `slurp`/`spit` names from Clojure - concise and memorable
- `slurp` should auto-detect: file path, URL, classpath resource
- Use Java NIO for file operations
- Port-based I/O wraps Java streams

### Checklist

- [x] `slurp` - Read entire resource to string (file, URL, classpath)
- [x] `spit` - Write string to file (with `:append` option)
- [x] `open-input-file` / `open-output-file` - Create ports
- [x] `close-input-port` / `close-output-port` - Close ports
- [x] `call-with-input-file` / `call-with-output-file` - With cleanup
- [x] `file-exists?` - Test file existence
- [x] `directory?` - Test if directory
- [x] `file-readable?` / `file-writable?` - Test permissions
- [x] `delete-file` - Delete file
- [x] `rename-file` - Rename/move file
- [x] `copy-file` - Copy file
- [x] `make-directory` - Create directory
- [x] `file-size` - Get file size in bytes
- [x] `directory-list` - List directory contents
- [x] `path-join` - Join path components
- [x] `path-directory` / `path-filename` / `path-extension` - Path parts
- [x] `current-directory` - Get current working directory

---

## 8. Control Flow Macros (Important)

Missing convenience macros that are standard in most Lisps.

### Proposed Implementation

```lisp
;; when/unless - conditional with implicit begin
(when (> x 0)
  (println "positive")
  (do-something))

(unless (null? lst)
  (process (car lst)))

;; let* - sequential bindings (each can reference previous)
(let* ((x 10)
       (y (+ x 5))       ; can reference x
       (z (* y 2)))      ; can reference x and y
  (+ x y z))             ; => 55

;; do - general iteration (R5RS)
(do ((i 0 (+ i 1))           ; variable, init, step
     (sum 0 (+ sum i)))      ; variable, init, step
    ((>= i 10) sum)          ; exit test, result expression
  (println i))               ; body (optional)

;; Named let - recursive loop
(let loop ((i 0) (sum 0))
  (if (>= i 10)
      sum
      (loop (+ i 1) (+ sum i))))

;; Simple loops (non-standard but practical)
(dotimes (i 10)
  (println i))               ; i goes 0..9

(dolist (x '(a b c))
  (println x))               ; iterate over list
```

**Implementation notes:**
- `when`/`unless` are trivial macros
- `let*` expands to nested `let` forms
- `do` is standard R5RS but complex
- Named `let` requires special handling in `let` macro
- `dotimes`/`dolist` are Common Lisp conveniences

### Checklist

- [x] `when` - Conditional with implicit begin
- [x] `unless` - Negated conditional with implicit begin
- [x] `let*` - Sequential bindings
- [x] `do` - General iteration (R5RS style)
- [x] Named `let` - Recursive loop binding (via `do` macro implementation)
- [x] `dotimes` - Counted iteration
- [x] `dolist` - List iteration

---

## 9. Symbol and Macro Utilities (Important)

Features for metaprogramming and writing hygienic macros.

### Proposed Implementation

```lisp
;; Generate unique symbols for macro hygiene
(gensym)                              ; => G__1234
(gensym "temp")                       ; => temp__1234

;; Symbol predicates and manipulation
(symbol=? 'foo 'foo)                  ; => true
(symbol->string 'foo)                 ; => "foo" (already have to-string)
(string->symbol "foo")                ; => foo (already have to-symbol)
```

**Implementation notes:**
- `gensym` uses atomic counter for uniqueness
- Critical for writing macros that don't capture variables
- Simple Java implementation with `AtomicLong`

### Checklist

- [x] `gensym` - Generate unique symbol
- [x] `symbol=?` - Symbol equality (currently use `equal?`)

---

## 10. Math Completion (Nice to Have)

Fix documented-but-missing functions and add utilities.

### Proposed Implementation

```lisp
;; Missing trig function
(asin 0.5)                            ; => ~0.524

;; Rounding
(round 3.5)                           ; => 4 (round to nearest, ties to even)
(truncate 3.7)                        ; => 3 (toward zero)
(truncate -3.7)                       ; => -3

;; Random numbers
(random 100)                          ; => integer 0-99
(random 1.0)                          ; => float 0.0-1.0
(random-seed 42)                      ; set seed for reproducibility

;; Constants
pi                                    ; => 3.14159...
e                                     ; => 2.71828...

;; Additional functions
(sign -5)                             ; => -1
(sign 0)                              ; => 0
(sign 5)                              ; => 1
```

### Checklist

- [x] `asin` - Arc sine (was documented but missing from math.jlll)
- [x] `round` - Round to nearest integer (ties to even)
- [x] `truncate` - Truncate toward zero
- [x] `random` - Random number generation
- [x] `random-seed` - Set random seed
- [x] `pi` - Mathematical constant 3.14159...
- [x] `e` - Mathematical constant 2.71828...
- [x] `sign` / `signum` - Sign of number (-1, 0, 1)

---

## 11. Parallel Execution (Nice to Have)

For CPU-bound operations that benefit from parallelism.

### Proposed Implementation

```lisp
;; Parallel map - process elements concurrently
(pmap expensive-fn '(1 2 3 4 5 6 7 8))

;; Parallel for-each
(pfor-each expensive-side-effect '(1 2 3 4))

;; Future - async computation
(define f (future (expensive-computation)))
(deref f)                             ; => wait and get result
(deref f 1000 'timeout)               ; => with timeout and default

;; Realized check
(realized? f)                         ; => true if computation complete

;; Execute multiple computations in parallel
(pcalls
  (lambda () (fetch-from-api-1))
  (lambda () (fetch-from-api-2))
  (lambda () (fetch-from-api-3)))     ; => list of results

;; Thread-safe mutable state (Clojure-style atoms)
(define counter (atom 0))
(swap! counter (lambda (x) (+ x 1)))  ; => atomic update, returns new value
(reset! counter 0)                    ; => set to value
(deref counter)                       ; => current value
```

**Implementation notes:**
- Use Java's `ForkJoinPool.commonPool()` for `pmap`
- `future` wraps `CompletableFuture`
- Atoms use `AtomicReference` with compare-and-swap loop
- Keep API simple - don't need full STM

### Checklist

- [x] `pmap` - Parallel map
- [x] `pfor-each` - Parallel for-each
- [x] `future` - Create async computation
- [x] `deref` - Get value (with optional timeout)
- [x] `realized?` - Check if future is complete
- [x] `pcalls` - Execute multiple thunks in parallel
- [x] `atom` - Create thread-safe mutable reference
- [x] `swap!` - Atomically update atom with function
- [x] `reset!` - Atomically set atom value
- [x] `compare-and-set!` - CAS operation on atom

---

## 12. Hash Maps / Dictionaries (Nice to Have)

Association lists work but are O(n). Need efficient key-value storage.

### Proposed Implementation

```lisp
;; Create hash map
(hash-map :a 1 :b 2 :c 3)             ; => hash map
(make-hash-map)                       ; => empty mutable hash map

;; Access
(hash-ref hm :a)                      ; => 1 (error if missing)
(hash-ref hm :missing 'default)       ; => default
(hash-has-key? hm :a)                 ; => true

;; Update (mutable)
(hash-set! hm :d 4)                   ; add/update entry
(hash-remove! hm :a)                  ; remove entry
(hash-update! hm :a                   ; update with function
  (lambda (v) (+ v 1)))

;; Iteration
(hash-keys hm)                        ; => (:a :b :c)
(hash-values hm)                      ; => (1 2 3)
(hash->alist hm)                      ; => ((:a . 1) (:b . 2) (:c . 3))
(alist->hash '((:a . 1) (:b . 2)))    ; => hash map

;; Info
(hash-count hm)                       ; => number of entries

;; Merge
(hash-merge hm1 hm2)                  ; => new map, hm2 overrides
```

**Implementation notes:**
- Wrap Java's `HashMap` or `LinkedHashMap`
- Start with mutable API (simpler)
- Consider immutable persistent maps later (like Clojure)

### Checklist

- [x] `hash-map` / `make-hash` - Create hash map
- [x] `hash-ref` - Lookup value (with optional default)
- [x] `hash-set!` - Add/update entry
- [x] `hash-remove!` - Remove entry
- [x] `hash-has-key?` - Check key existence
- [x] `hash-update!` - Update with function
- [x] `hash-keys` / `hash-values` - Get keys/values as lists
- [x] `hash->alist` / `alist->hash` - Conversion
- [x] `hash-count` - Number of entries
- [x] `hash-merge` - Merge maps
- [x] `hash-clear!` - Remove all entries
- [x] `hash?` - Test if value is hash map

---

## 13. Modules / Namespaces (Nice to Have)

Currently all definitions are global. For larger projects, need organization.

### Proposed Implementation (Simple Approach)

```lisp
;; Define module with explicit exports
(module mymath
  (export square cube)
  
  (define (helper x) (* x x x x))     ; private - not exported
  
  (define (square x)                  ; exported
    :doc "Returns x squared"
    (* x x))
  
  (define (cube x)                    ; exported
    :doc "Returns x cubed"
    (* x x x)))

;; Import all exports
(import mymath)
(square 5)                            ; => 25

;; Import specific symbols
(import mymath :only (square))

;; Import with prefix
(import mymath :prefix math/)
(math/square 5)                       ; => 25

;; Require - load file and import
(require "mymath.jlll")               ; load and import
(require "mymath.jlll" :as math)      ; with prefix
```

**Implementation notes:**
- Start simple: symbol prefixing without true isolation
- `module` creates bindings with prefix
- `import` copies/aliases symbols to current environment
- More complex: separate environments per module (later)

### Checklist

- [x] `module` - Define module with exports
- [x] `export` - Mark symbols for export (explicit or `*` for all)
- [x] `load` - Load file from path (foundation for `require`)
- [x] `import` - Import module symbols (with `:only`, `:except`, `:prefix` options)
- [x] `require` - Load file and import (with `:as` option for prefix)
- [x] Qualified symbol resolution (`module/symbol`)
- [x] Private definitions (not exported) - symbols not in `export` are private

---

## 14. Regular Expressions (Important)

Pattern matching and text manipulation using Java's regex engine.

### Proposed Implementation

```lisp
;; Match first occurrence
(regex-match "\\d+" "abc123def")          ; => "123"
(regex-match "xyz" "abc123def")           ; => false

;; Match all occurrences
(regex-match-all "\\d+" "a1b2c3")         ; => ("1" "2" "3")

;; Match with groups (returns list: full match + groups)
(regex-match "(\\w+)@(\\w+)" "user@host")
;; => ("user@host" "user" "host")

;; Replace all matches
(regex-replace "\\d" "a1b2c3" "X")        ; => "aXbXcX"

;; Replace first match only
(regex-replace-first "\\d" "a1b2c3" "X")  ; => "aXb2c3"

;; Replace with function (receives match, returns replacement)
(regex-replace "\\d+" "a12b34c"
  (lambda (m) (number->string (* 2 (string->number m)))))
;; => "a24b68c"

;; Split by pattern
(regex-split "\\s+" "a  b   c")           ; => ("a" "b" "c")
(regex-split "," "a,b,,c")                ; => ("a" "b" "" "c")

;; Test if matches
(regex-matches? "^\\d+$" "123")           ; => true
(regex-matches? "^\\d+$" "12a")           ; => false

;; Find position of first match
(regex-find "\\d+" "abc123def")           ; => 3 (index)
(regex-find "xyz" "abc123def")            ; => false
```

**Implementation notes:**
- Wrap Java's `java.util.regex.Pattern` and `Matcher`
- Cache compiled patterns for performance
- Use Java regex syntax (documented in `java.util.regex.Pattern`)
- `regex-replace` with function is more powerful but requires evaluating JLLL lambda

### Checklist

- [ ] `regex-match` - First match or false
- [ ] `regex-match-all` - All matches as list
- [ ] `regex-replace` - Replace all occurrences
- [ ] `regex-replace-first` - Replace first occurrence
- [ ] `regex-split` - Split string by pattern
- [ ] `regex-matches?` - Test if entire string matches
- [ ] `regex-find` - Index of first match

---

## 15. JSON Support (Important)

Parse and generate JSON for data interchange. Essential for web APIs and configuration files.

### Proposed Implementation

```lisp
;; Parse JSON string to JLLL data
(json-parse "{\"name\": \"Alice\", \"age\": 30}")
;; => hash-map with :name "Alice", :age 30

(json-parse "[1, 2, 3]")                  ; => (1 2 3)
(json-parse "\"hello\"")                  ; => "hello"
(json-parse "42")                         ; => 42
(json-parse "true")                       ; => true
(json-parse "null")                       ; => null

;; Parse with options
(json-parse "{\"name\": \"Alice\"}" :keys-as-symbols true)
;; => hash-map with symbol keys: name -> "Alice"

;; Convert JLLL data to JSON string
(json-stringify (hash-map :name "Alice" :age 30))
;; => "{\"name\":\"Alice\",\"age\":30}"

(json-stringify '(1 2 3))                 ; => "[1,2,3]"
(json-stringify "hello")                  ; => "\"hello\""

;; Pretty print
(json-stringify data :pretty true)
;; => formatted with indentation

;; File operations
(json-read-file "config.json")            ; => parsed data
(json-write-file "output.json" data)      ; write to file
(json-write-file "output.json" data :pretty true)
```

**Implementation notes:**
- Use a lightweight JSON library (e.g., minimal-json, or built-in Nashorn if available)
- Or implement simple recursive descent parser in Java
- JSON objects become hash-maps (keys as keywords by default)
- JSON arrays become lists
- JSON null becomes JLLL null

### Checklist

- [ ] `json-parse` / `json-read` - Parse JSON string to JLLL data
- [ ] `json-stringify` / `json-write` - Convert JLLL data to JSON string
- [ ] `json-read-file` - Parse JSON from file
- [ ] `json-write-file` - Write JLLL data as JSON to file
- [ ] `:pretty` option for formatted output
- [ ] `:keys-as-symbols` option for symbol keys instead of keywords

---

## 16. Date/Time Operations (Important)

Working with timestamps, formatting, and date arithmetic.

### Proposed Implementation

```lisp
;; Current time
(now)                                     ; => milliseconds since epoch
(current-time)                            ; => hash-map with components
;; {:year 2024 :month 3 :day 15 :hour 14 :minute 30 :second 45 :millis 123}

;; Format timestamp
(date-format (now) "yyyy-MM-dd")          ; => "2024-03-15"
(date-format (now) "HH:mm:ss")            ; => "14:30:45"
(date-format (now) "yyyy-MM-dd'T'HH:mm:ss.SSSZ")
;; => "2024-03-15T14:30:45.123-0500"

;; Parse string to timestamp
(date-parse "2024-03-15" "yyyy-MM-dd")    ; => timestamp (millis)
(date-parse "14:30:00" "HH:mm:ss")        ; => timestamp

;; Date arithmetic
(date-add (now) :days 7)                  ; => 7 days from now
(date-add (now) :hours -2)                ; => 2 hours ago
(date-add (now) :months 1)                ; => 1 month from now
;; Supported units: :years :months :weeks :days :hours :minutes :seconds :millis

;; Difference between timestamps
(date-diff t1 t2 :days)                   ; => number of days between
(date-diff t1 t2 :hours)                  ; => number of hours

;; Extract components
(date-year (now))                         ; => 2024
(date-month (now))                        ; => 3
(date-day (now))                          ; => 15
(date-hour (now))                         ; => 14
(date-minute (now))                       ; => 30
(date-second (now))                       ; => 45
(date-day-of-week (now))                  ; => 5 (1=Sunday, 7=Saturday)

;; Decompose to list
(date->list (now))                        ; => (2024 3 15 14 30 45 123)

;; Create timestamp from components
(make-date 2024 3 15)                     ; => timestamp for midnight
(make-date 2024 3 15 14 30 0)             ; => timestamp for 14:30:00

;; Comparisons
(date<? t1 t2)                            ; => true if t1 before t2
(date>? t1 t2)
(date=? t1 t2)
```

**Implementation notes:**
- Use Java 8+ `java.time` API (Instant, LocalDateTime, DateTimeFormatter)
- Timestamps are `Long` milliseconds since Unix epoch
- Format patterns follow Java's DateTimeFormatter
- Time zone handling: use system default, or add optional `:timezone` parameter

### Checklist

- [ ] `now` - Current timestamp in milliseconds
- [ ] `current-time` - Current time as hash-map
- [ ] `date-format` - Format timestamp to string
- [ ] `date-parse` - Parse string to timestamp
- [ ] `date-add` - Add time units to timestamp
- [ ] `date-diff` - Difference between timestamps
- [ ] `date-year` / `date-month` / `date-day` etc. - Extract components
- [ ] `date->list` - Decompose to list
- [ ] `make-date` - Create timestamp from components
- [ ] `date<?` / `date>?` / `date=?` - Comparisons

---

## 17. Debugging and Development Tools (Nice to Have)

Utilities for debugging, testing, and development workflows.

### Proposed Implementation

```lisp
;; Trace function calls
(trace square)                            ; enable tracing
(square 5)
;; prints: TRACE: (square 5)
;; prints: TRACE: square => 25
;; => 25
(untrace square)                          ; disable tracing

;; Trace all functions in module
(trace-module mymath)
(untrace-module mymath)

;; Assertions
(assert (> x 0))                          ; raises if false
(assert (> x 0) "x must be positive")     ; with message
(assert (integer? x) "expected integer, got: " x)

;; Type inspection
(type-of 42)                              ; => "Integer" or "Long"
(type-of "hello")                         ; => "String"
(type-of '(1 2 3))                        ; => "Cons"
(type-of (hash-map))                      ; => "LinkedHashMap"
(type-of square)                          ; => "Procedure" or "Lambda"

;; Identity function (useful for debugging/testing)
(identity x)                              ; => x

;; Constantly - returns function that always returns given value
(define always-42 (constantly 42))
(always-42)                               ; => 42
(always-42 'ignored 'args)                ; => 42

;; Complement - negate predicate
(define not-empty? (complement empty?))
(not-empty? '(1 2 3))                     ; => true

;; Tap - debug inspection without breaking flow
(tap x)                                   ; prints x, returns x
(tap x "label")                           ; prints "label: x", returns x
(->> data
     (filter valid?)
     (tap "after filter")
     (map transform)
     (tap "after map"))

;; Time with label
(time-it "fib(30)" (fib 30))
;; prints: fib(30): 245ms
;; => result

;; Inspect object in detail
(inspect obj)                             ; prints type, fields, methods
```

**Implementation notes:**
- `trace` modifies the binding to wrap the function
- `assert` raises `JlllException` on failure
- `type-of` returns Java class simple name
- `tap` is invaluable for debugging pipelines

### Checklist

- [ ] `trace` / `untrace` - Trace function calls
- [ ] `assert` - Assertion with optional message
- [ ] `type-of` - Get type name as string
- [ ] `identity` - Return argument unchanged
- [ ] `constantly` - Function that always returns given value
- [ ] `complement` - Negate a predicate
- [ ] `tap` - Debug inspection in pipelines
- [ ] `inspect` - Detailed object inspection

---

## 18. Environment and System (Nice to Have)

Access to environment variables, system properties, and system information.

### Proposed Implementation

```lisp
;; Environment variables
(getenv "HOME")                           ; => "/home/user"
(getenv "MISSING")                        ; => null
(getenv "MISSING" "default")              ; => "default"
(getenv-all)                              ; => hash-map of all env vars

;; Java system properties
(get-property "java.version")             ; => "17.0.1"
(get-property "os.name")                  ; => "Mac OS X"
(get-property "user.dir")                 ; => current directory
(set-property! "my.prop" "value")         ; set property

;; System information
(hostname)                                ; => "mycomputer.local"
(user-name)                               ; => "alice"
(user-home)                               ; => "/home/alice"
(os-name)                                 ; => "Mac OS X"
(os-arch)                                 ; => "aarch64"
(java-version)                            ; => "17.0.1"

;; Command execution (careful - security implications)
(shell "ls -la")                          ; => output as string
(shell "ls" "-la" "/tmp")                 ; => with args
(shell-lines "ls")                        ; => output as list of lines
(shell-status "test -f file.txt")         ; => exit code (0 = success)

;; Process memory
(gc)                                      ; trigger garbage collection
(memory-used)                             ; => bytes used
(memory-free)                             ; => bytes free
(memory-total)                            ; => total heap size
```

**Implementation notes:**
- Environment access via `System.getenv()`
- Properties via `System.getProperty()` / `setProperty()`
- Shell execution needs careful security consideration (maybe opt-in)
- Memory via `Runtime.getRuntime()`

### Checklist

- [ ] `getenv` - Get environment variable
- [ ] `getenv-all` - All environment variables as hash-map
- [ ] `get-property` - Get Java system property
- [ ] `set-property!` - Set Java system property
- [ ] `hostname` - Machine hostname
- [ ] `user-name` / `user-home` - User info
- [ ] `os-name` / `os-arch` - OS info
- [ ] `java-version` - Java version string
- [ ] `gc` - Trigger garbage collection
- [ ] `memory-used` / `memory-free` / `memory-total` - Memory info

---

## 19. Formatted Output (Nice to Have)

Printf-style formatting and Scheme-standard output procedures.

### Proposed Implementation

```lisp
;; Printf-style format (Common Lisp / Scheme SRFI-28 style)
(format "Hello, ~a!" "World")             ; => "Hello, World!"
(format "~a + ~a = ~a" 1 2 3)             ; => "1 + 2 = 3"

;; Format directives:
;; ~a - aesthetic (human readable, no quotes on strings)
;; ~s - standard (machine readable, quotes on strings)
;; ~d - decimal integer
;; ~f - floating point
;; ~% - newline
;; ~~ - literal tilde

(format "Name: ~a, Age: ~d" "Alice" 30)   ; => "Name: Alice, Age: 30"
(format "Pi is ~f" 3.14159)               ; => "Pi is 3.14159"
(format "~s" "hello")                     ; => "\"hello\""
(format "Line1~%Line2")                   ; => "Line1\nLine2"

;; Width and padding
(format "~10a" "hi")                      ; => "hi        " (right-pad)
(format "~10@a" "hi")                     ; => "        hi" (left-pad)
(format "~5d" 42)                         ; => "   42"
(format "~5,'0d" 42)                      ; => "00042" (zero-pad)

;; Scheme R5RS style output
(display "hello")                         ; prints: hello (no quotes)
(display '(1 2 3))                        ; prints: (1 2 3)
(write "hello")                           ; prints: "hello" (with quotes)
(write '(1 2 3))                          ; prints: (1 2 3)

;; With port
(display "hello" out-port)
(write data out-port)

;; Printf to port
(fprintf port "~a: ~d~%" name value)

;; Printf to stdout
(printf "Result: ~a~%" result)
```

**Implementation notes:**
- Format directive syntax follows Common Lisp / SRFI-28
- Can implement subset first, expand later
- `display` vs `write`: display is for humans, write is for `read` to parse back
- Consider using Java's `String.format` for numeric formatting

### Checklist

- [ ] `format` - Format string with directives
- [ ] `printf` - Format and print to stdout
- [ ] `fprintf` - Format and print to port
- [ ] `display` - Write object for humans (no string quotes)
- [ ] `write` - Write object for machine (readable representation)
- [ ] Basic directives: `~a`, `~s`, `~d`, `~f`, `~%`, `~~`
- [ ] Width/padding support

---

## 20. Sets (Nice to Have)

Hash-based set data structure for efficient membership testing and set operations.

### Proposed Implementation

```lisp
;; Create sets
(make-set)                                ; => empty mutable set
(set 1 2 3)                               ; => set with elements
(list->set '(1 2 2 3 3 3))                ; => set with 1, 2, 3

;; Predicates
(set? s)                                  ; => true
(set-empty? s)                            ; => true/false
(set-contains? s 2)                       ; => true

;; Size
(set-count s)                             ; => number of elements

;; Mutation
(set-add! s 4)                            ; add element, returns set
(set-remove! s 2)                         ; remove element, returns set
(set-clear! s)                            ; remove all elements

;; Conversion
(set->list s)                             ; => (1 2 3) (order may vary)

;; Set operations (return new sets)
(set-union s1 s2)                         ; elements in either
(set-intersection s1 s2)                  ; elements in both
(set-difference s1 s2)                    ; elements in s1 but not s2
(set-symmetric-difference s1 s2)          ; elements in exactly one

;; Set predicates
(set-subset? s1 s2)                       ; is s1 subset of s2?
(set-superset? s1 s2)                     ; is s1 superset of s2?
(set-disjoint? s1 s2)                     ; no common elements?
(set-equal? s1 s2)                        ; same elements?

;; Iteration
(set-for-each println s)                  ; iterate over elements
(set-map (lambda (x) (* x 2)) s)          ; => new set with transformed elements
(set-filter positive? s)                  ; => new set with matching elements
```

**Implementation notes:**
- Wrap Java's `LinkedHashSet` (preserves insertion order)
- Or `HashSet` for pure performance
- Mutable by default (like hash-maps)
- Set operations return new sets (non-destructive)

### Checklist

- [ ] `make-set` / `set` - Create sets
- [ ] `list->set` / `set->list` - Conversions
- [ ] `set?` - Test if value is set
- [ ] `set-empty?` - Test for empty set
- [ ] `set-contains?` - Membership test
- [ ] `set-count` - Number of elements
- [ ] `set-add!` / `set-remove!` / `set-clear!` - Mutation
- [ ] `set-union` / `set-intersection` / `set-difference` - Set operations
- [ ] `set-subset?` / `set-superset?` / `set-equal?` - Set predicates
- [ ] `set-for-each` / `set-map` / `set-filter` - Iteration

---

## 21. Lazy Sequences (Nice to Have)

Lazy evaluation for working with potentially infinite sequences and deferred computation.

### Proposed Implementation

```lisp
;; Delay evaluation (create a "promise")
(define p (delay (begin (println "computing...") 42)))
(force p)                                 ; prints "computing...", => 42
(force p)                                 ; => 42 (cached, no recomputation)

;; Test if realized
(realized? p)                             ; => true after first force

;; Lazy cons - cdr is delayed
(define ones (lazy-cons 1 ones))          ; infinite list of 1s
(car ones)                                ; => 1
(car (cdr ones))                          ; => 1

;; Lazy range (possibly infinite)
(define naturals (lazy-range 0))          ; 0, 1, 2, 3, ...
(define evens (lazy-range 0 :step 2))     ; 0, 2, 4, 6, ...
(define finite (lazy-range 0 10))         ; 0, 1, ..., 9

;; Take from lazy sequence (forces evaluation)
(take 5 naturals)                         ; => (0 1 2 3 4)
(take 10 (lazy-range 0 :step 2))          ; => (0 2 4 6 8 10 12 14 16 18)

;; Lazy transformations (return lazy sequences)
(define squares (lazy-map (lambda (x) (* x x)) naturals))
(take 5 squares)                          ; => (0 1 4 9 16)

(define evens (lazy-filter even? naturals))
(take 5 evens)                            ; => (0 2 4 6 8)

;; Lazy drop
(take 5 (lazy-drop 10 naturals))          ; => (10 11 12 13 14)

;; Take while
(lazy-take-while (lambda (x) (< x 100)) naturals)

;; Realize entire lazy sequence (careful with infinite!)
(realize (lazy-range 0 10))               ; => (0 1 2 3 4 5 6 7 8 9)

;; Check if lazy
(lazy-seq? naturals)                      ; => true
(lazy-seq? '(1 2 3))                      ; => false

;; Iterate - generate from function
(define powers-of-2 (iterate (lambda (x) (* x 2)) 1))
(take 10 powers-of-2)                     ; => (1 2 4 8 16 32 64 128 256 512)

;; Cycle - infinite repetition
(define abc (cycle '(a b c)))
(take 7 abc)                              ; => (a b c a b c a)

;; Repeat - infinite copies
(take 5 (repeat 'x))                      ; => (x x x x x)
```

**Implementation notes:**
- `delay`/`force` use a simple wrapper class with memoization
- Lazy sequences can be implemented as thunks returning `(value . thunk)`
- Be careful with stack depth on long sequences (use trampolining if needed)
- `take` on infinite sequence must force elements one at a time
- Consider implementing as proper stream type for better integration

### Checklist

- [ ] `delay` / `force` - Basic lazy evaluation primitives
- [ ] `lazy-cons` - Cons with lazy cdr
- [ ] `lazy-seq?` - Test for lazy sequence
- [ ] `lazy-range` - Lazy numeric range (finite or infinite)
- [ ] `lazy-map` / `lazy-filter` - Lazy transformations
- [ ] `lazy-take` / `lazy-drop` - Lazy slicing
- [ ] `lazy-take-while` / `lazy-drop-while` - Predicate-based slicing
- [ ] `realize` - Force entire lazy sequence to list
- [ ] `iterate` - Generate from repeated function application
- [ ] `cycle` - Infinite repetition of sequence
- [ ] `repeat` - Infinite copies of value

---

## 22. AI Integration with LangChain4j (Important)

Integrate LLM capabilities into JLLL using [langchain4j](https://github.com/langchain4j/langchain4j).
Provides a session-centric model where all AI interactions go through sessions with conversation
memory, dynamic tool management, and lazy streaming responses.

**Dependencies on other sections:**
- Section 21 (Lazy Sequences) - All AI responses are lazy sequences
- Section 15 (JSON Support) - Tool argument parsing, structured responses
- Section 12 (Hash Maps) - Configuration and structured data
- Section 11 (Parallel Execution) - Async AI calls via `future`/`pmap`
- Section 7 (File I/O) - Document loading for RAG

### Configuration

API keys are read from environment variables (auto-detected):

| Environment Variable | Provider |
|---------------------|----------|
| `OPENAI_API_KEY` | OpenAI (GPT-4, GPT-4o, etc.) |
| `ANTHROPIC_API_KEY` | Anthropic (Claude) |
| `GOOGLE_AI_API_KEY` | Google AI (Gemini) |
| `OLLAMA_BASE_URL` | Ollama (local models) |

Keys can also be set programmatically to override environment variables.

### Proposed Implementation

```lisp
;; ============================================================
;; SESSION MANAGEMENT
;; ============================================================

;; Create session explicitly (returns session object)
(define coder (ai-session-create 
  :name "coding-helper"              ; optional, otherwise auto-generated ID
  :system "You are a coding tutor"   ; system message
  :model "gpt-4o"                    ; optional model override
  :tools (list custom-tool)))        ; additional tools (eval is default)

;; Make session active for current environment
(ai-session-activate coder)

;; Get currently active session (nil if none)
(ai-session-current)

;; List all sessions
(ai-sessions)                        ; => (session1 session2 ...)

;; Deactivate current session
(ai-session-deactivate)

;; Get session identity
(ai-session-name coder)              ; => "coding-helper" or auto-generated
(ai-session-id coder)                ; => unique ID like "sess-12345"

;; ============================================================
;; CORE AI - Returns lazy sequence
;; ============================================================

;; (ai ...) uses active session, or creates implicit default session
;; Always returns a lazy sequence of text chunks

(define response (ai "Explain closures"))

;; Stream to output (prints as chunks arrive)
(for-each print response)

;; Collect full response as string
(string-join (realize response) "")

;; With options
(ai "Be creative" :temperature 0.9)
(ai "Use this model" :model "claude-3-sonnet")

;; Override session for single call (doesn't change active session)
(ai "Question" :session other-session)

;; Conversation continues in active session
(ai-session-activate coder)
(ai "Hello, I'm learning Lisp")
(ai "What's a cons cell?")           ; has context from previous
(ai "Show me an example")            ; continues same conversation

;; ============================================================
;; HISTORY MANAGEMENT
;; ============================================================

;; Get conversation history
(ai-history)                         ; active session
(ai-history coder)                   ; specific session

;; Clear history (keeps session config)
(ai-clear)                           ; clear active session
(ai-clear coder)                     ; clear specific session

;; ============================================================
;; TOOL MANAGEMENT (Dynamic)
;; ============================================================

;; Define a custom tool
(define weather-tool
  (ai-tool "get-weather"
    :description "Gets current weather for a city"
    :parameters ((:city :type "string" :description "City name"))
    :fn (lambda (city) (fetch-weather city))))

;; Add tool to session dynamically
(ai-tool-add coder weather-tool)

;; Remove tool from session
(ai-tool-remove coder "get-weather")

;; List tools in session
(ai-tools coder)                     ; => list of tool specs

;; ============================================================
;; DEFAULT EVAL TOOL
;; ============================================================

;; The "eval" tool is enabled by default in all sessions
;; Allows LLM to execute arbitrary JLLL code
;; Security is user's responsibility!

(ai "Calculate the 20th fibonacci number")
;; LLM writes JLLL code, calls eval tool, returns result

(ai "Write code to list files in current directory and count them")
;; LLM uses eval tool to run (length (directory-list "."))

;; Errors are returned as messages, allowing LLM to retry
(ai "Divide 10 by 0")
;; eval returns: "Error: Division by zero"
;; LLM can acknowledge or try different approach

;; Disable eval tool if needed (for security)
(ai-tool-remove coder "eval")

;; ============================================================
;; LAZY RESPONSE PATTERNS
;; ============================================================

;; Since (ai ...) returns lazy sequence, many patterns are possible:

;; Take only first few chunks (cancel early)
(take 5 (ai "Tell me a very long story..."))

;; Transform stream lazily
(lazy-map string-upcase (ai "Hello"))

;; Process with immediate side effects
(for-each 
  (lambda (chunk) 
    (print chunk))
  (ai "Streaming response"))

;; Tool calls happen transparently during realization
;; User only sees final text output

;; ============================================================
;; ASYNC AI (using JLLL concurrency - Section 11)
;; ============================================================

;; Run AI call in background
(define f (future (string-join (realize (ai "Summarize...")) "")))
(do-other-work)
(deref f)                            ; get result when ready

;; Parallel AI calls
(pmap (lambda (q) (string-join (realize (ai q)) ""))
      '("Question 1" "Question 2" "Question 3"))

;; ============================================================
;; CONFIGURATION
;; ============================================================

;; Override environment variables programmatically
(ai-configure :openai-api-key "sk-...")
(ai-configure :anthropic-api-key "sk-ant-...")
(ai-configure :default-model "gpt-4o")

;; Get current config as hash-map
(ai-config)                          ; => {:default-model "gpt-4o" ...}
```

**Implementation notes:**
- Uses [langchain4j](https://github.com/langchain4j/langchain4j) as the underlying library
- Provider-agnostic: automatically detects available providers from environment variables
- All AI responses are **lazy sequences** - no separate streaming API needed
- Tool execution is synchronous (LLM calls tool → execute → result sent back → continue)
- For async operations, use JLLL's `future`, `pmap`, `pcalls` from Section 11
- The `eval` tool is **enabled by default** - allows LLM to run JLLL code
- Eval tool errors return error messages to LLM (allows retry), not exceptions
- **Security note:** eval tool allows arbitrary code execution - security is user's responsibility

### Future Enhancements

These features may be added in future versions:

**Session Persistence:**
```lisp
;; Save session to file (history, config, tools)
(ai-session-save coder "coder-session.jlll")

;; Load session from file
(define restored (ai-session-load "coder-session.jlll"))
```

**Embeddings:**
```lisp
(ai-embed "Hello world")             ; => list of floats (vector)
(ai-similarity "cat" "dog")          ; => 0.85 (cosine similarity)
```

**RAG (Retrieval-Augmented Generation):**
```lisp
;; Load and split documents
(define docs (ai-load-documents "docs/*.md"))
(define chunks (ai-split-text docs :chunk-size 1000))

;; Create vector store and add embeddings
(define store (ai-vector-store :type "chroma"))
(ai-store-add store chunks)

;; Query with context
(ai-rag "How does the module system work?" :store store)
```

**Structured Outputs:**
```lisp
;; Force LLM to return specific structure
(ai-structured "Extract person info from: John is 30 years old"
  :schema {:name :string :age :integer})
;; => {:name "John" :age 30}
```

**Image Generation:**
```lisp
(ai-image "A sunset over mountains" :model "dall-e-3")
;; => URL or binary data
```

**Vision / Multimodal:**
```lisp
(ai-vision "What's in this image?" :image "photo.jpg")
(ai-vision "Describe this" :image-url "https://...")
```

### Checklist

**Session Management:**
- [ ] `ai-session-create` - Create session (`:name`, `:system`, `:model`, `:tools`)
- [ ] `ai-session-activate` - Make session active for current environment
- [ ] `ai-session-deactivate` - Deactivate current session
- [ ] `ai-session-current` - Get active session or nil
- [ ] `ai-sessions` - List all sessions
- [ ] `ai-session-name` / `ai-session-id` - Get session identity

**Core Operations:**
- [ ] `ai` - Chat using active session, returns lazy sequence (depends: Section 21)
- [ ] `ai-history` - Get conversation history
- [ ] `ai-clear` - Clear session history

**Tool Management:**
- [ ] `ai-tool` - Define custom tool
- [ ] `ai-tool-add` - Add tool to session dynamically
- [ ] `ai-tool-remove` - Remove tool from session
- [ ] `ai-tools` - List tools in session
- [ ] Built-in `eval` tool (default, errors returned as messages)

**Configuration:**
- [ ] `ai-configure` - Set API keys and defaults
- [ ] `ai-config` - Get configuration as hash-map

---

## Implementation Notes

### Priority Order for Implementation

**Phase 1 - Core Language (Completed):**
1. ~~Exception handling~~ - Everything else depends on being able to handle errors
2. ~~Input operations~~ - Needed for any interactive program
3. ~~String operations~~ - Fundamental for text processing
4. ~~Comparison operators & predicates~~ - Very frequently needed
5. ~~List utilities~~ - Core functional programming tools
6. ~~File I/O~~ - Essential for real applications
7. ~~Control flow macros~~ - Developer convenience
8. ~~Symbol/macro utilities~~ - For metaprogramming
9. ~~Math completion~~ - Fill gaps in numeric operations
10. ~~Parallel execution~~ - Modern concurrency support
11. ~~Hash maps~~ - Efficient key-value storage
12. ~~Modules/namespaces~~ - Code organization

**Phase 2 - Enhanced Functionality (New):**
13. **Regular expressions** - Essential for text processing
14. **JSON support** - Critical for modern applications and APIs
15. **Date/time operations** - Common requirement for most applications
16. **Debugging tools** - Developer productivity
17. **Environment/system access** - Integration with host system
18. **Formatted output** - Better output control
19. **Sets** - Complete collection types
20. **Lazy sequences** - Advanced functional programming
21. **AI integration** - LLM capabilities via langchain4j (depends on 11, 12, 15, 21)

### Implementation Strategies

**Java Primitives vs JLLL Implementations:**
- Simple operations (`when`, `unless`, `<=`, `zero?`) - JLLL macros/functions in `.jlll` files
- Performance-critical or Java-dependent features - Java primitives in `*Lib.java`
- String/file/regex operations - wrap Java methods directly
- JSON - use lightweight library or implement recursive descent parser
- Date/time - wrap `java.time` API

**New Library Classes to Create:**
- `RegexLib.java` - Regular expression operations
- `JsonLib.java` - JSON parsing and generation
- `DateLib.java` - Date/time operations
- `SystemLib.java` - Environment and system access
- `SetLib.java` - Set data structure
- `LazyLib.java` - Lazy sequences and streams
- `AILib.java` - AI/LLM integration (requires langchain4j dependency)

**Testing:**
- Add tests in `JLLLTestCase.java` for Java primitives
- Add `:doc` metadata to JLLL-implemented functions
- Test edge cases: empty lists, null, boundary values, malformed input

**Documentation:**
- Update `docs/primitives.md` with new functions
- Include examples in `:doc` strings
- Add new sections for each library

### Section Dependencies

Some sections depend on features from other sections. Implement dependencies first,
or implement sections in the order listed.

| Section | Depends On | Used For |
|---------|------------|----------|
| 14. Regular Expressions | 3. String Operations | Text pattern matching |
| 15. JSON Support | 6. List Utilities | JSON arrays → lists |
| 15. JSON Support | 12. Hash Maps | JSON objects → hash-maps |
| 16. Date/Time | 12. Hash Maps | `current-time` structured data |
| 17. Debugging Tools | 9. Symbol/Macro Utilities | `trace` function wrapping |
| 19. Formatted Output | 3. String Operations | Text formatting |
| 21. Lazy Sequences | 8. Control Flow | `delay`/`force` evaluation |
| 22. AI Integration | 11. Parallel Execution | Async AI calls (`future`, `pmap`) |
| 22. AI Integration | 12. Hash Maps | Configuration, structured responses |
| 22. AI Integration | 15. JSON Support | Tool arguments, LLM responses |
| 22. AI Integration | 21. Lazy Sequences | Streaming responses |
| 22. AI Integration | 7. File I/O | Document loading (RAG) |

**Dependency-free sections:** 14, 17, 18, 19, 20 can be implemented independently.

**Recommended implementation order for Phase 2:**
1. Regular expressions (14) - no dependencies
2. JSON support (15) - needs hash maps (done), lists (done)
3. Date/time (16) - needs hash maps (done)
4. Sets (20) - no dependencies
5. Lazy sequences (21) - needs control flow (done)
6. AI integration (22) - needs JSON, lazy sequences, concurrency (all done or above)
7. Debugging tools (17), Environment (18), Formatted output (19) - anytime

### Scheme Compatibility Notes

JLLL follows Scheme naming conventions but is not strictly R5RS/R7RS compatible:
- No character type (use single-char strings)
- Continuations (`call/cc`) are available with replay capability for saved continuations
- Mutable by default (Scheme prefers immutability)
- Keywords (`:foo`) are JLLL-specific
- Java interop is JLLL-specific
- `format` uses Common Lisp / SRFI-28 directives (`~a`, `~s`, etc.)

---

## Contributing

When implementing features from this roadmap:

1. **Format check**: `mvn spotless:check` (or `mvn spotless:apply` to fix)
2. **Run tests**: `mvn test`
3. **Add documentation**: `:doc` metadata for JLLL, JavaDoc for Java
4. **Update docs**: `docs/primitives.md` for new functions
5. **Commit cleanly**: Working tree should pass all checks
