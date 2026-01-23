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

- [ ] `raise` - Signal an exception
- [ ] `guard` - Scheme-style structured exception handling (Option A)
- [ ] `with-exception-handler` - Install exception handler for dynamic extent
- [ ] `try`/`catch`/`finally` - Java-style exception handling (Option B)
- [ ] `error` - Convenience for `(raise (make-error message))`

---

## 2. Input Operations (Critical)

Documented in primitives.md but **not implemented**. Essential for interactive programs.

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
- `read` requires exposing Tokenizer/Marshaller as primitive
- Use Java's `BufferedReader.readLine()` for `read-line`
- Already have `*stdin*` bound to `InputStreamReader`

### Checklist

- [ ] `read` - Read and parse JLLL expression
- [ ] `read-line` - Read line as string
- [ ] `read-char` - Read single character
- [ ] `peek-char` - Peek next character
- [ ] `newline` - Output newline (trivial: print empty line)
- [ ] `char-ready?` - Check if input available
- [ ] `eof-object?` - Test for end-of-file

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

- [ ] `string-length` - Get string length
- [ ] `substring` - Extract substring
- [ ] `string-ref` - Character at index (returns single-char string)
- [ ] `string-index` - Find substring position
- [ ] `string-contains?` - Check if contains substring
- [ ] `string-upcase` / `string-downcase` - Case conversion
- [ ] `string-trim` / `string-trim-left` / `string-trim-right`
- [ ] `string-replace` - Replace occurrences
- [ ] `string-split` - Split by delimiter
- [ ] `string-join` - Join with delimiter
- [ ] `string->number` / `number->string` - Conversion with optional radix
- [ ] `string->list` / `list->string` - Conversion
- [ ] `string=?` / `string<?` / `string>?` - Comparison
- [ ] `string<=?` / `string>=?` - Comparison
- [ ] `string-ci=?` / `string-ci<?` - Case-insensitive comparison
- [ ] `string-append` - Variadic concatenation (alias for `concat`)
- [ ] `make-string` - Create string of repeated chars
- [ ] `string-empty?` - Test for empty string

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
- [ ] `real?` - Test for real number (non-complex)
- [ ] `pair?` - Test for cons pair (note: `'()` is NOT a pair)
- [ ] `atom?` - Test for non-pair (convenience)
- [ ] `char?` - Test for character (if char type added)
- [ ] `vector?` - Test for Java array/vector
- [ ] `port?` - Test for input/output port
- [ ] `input-port?` / `output-port?` - Specific port types

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

- [ ] `list-ref` - Element at index
- [ ] `list-tail` - Sublist from index
- [ ] `member` / `memq` / `memv` - Search for element
- [ ] `assoc` / `assq` / `assv` - Association list lookup
- [ ] `for-each` - Map for side effects
- [ ] `fold-left` / `fold-right` - Fold operations
- [ ] `reduce` - Fold with first element as initial value
- [ ] `any` / `every` - Test predicates across list
- [ ] `find` - Find first matching element
- [ ] `range` / `iota` - Generate numeric sequence
- [ ] `make-list` - Create list of repeated elements
- [ ] `take` / `drop` - First/remaining n elements
- [ ] `take-while` / `drop-while` - Predicate-based take/drop
- [ ] `flatten` - Flatten nested lists
- [ ] `zip` - Combine lists pairwise
- [ ] `unzip` - Split list of pairs
- [ ] `sort` / `sort-by` - Sort list
- [ ] `remove` / `delete` - Remove elements
- [ ] `remove-duplicates` - Remove duplicates
- [ ] Variadic `append` - Extend current 2-arg version

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

- [ ] `slurp` - Read entire resource to string (file, URL, classpath)
- [ ] `spit` - Write string to file (with `:append` option)
- [ ] `open-input-file` / `open-output-file` - Create ports
- [ ] `close-input-port` / `close-output-port` - Close ports
- [ ] `call-with-input-file` / `call-with-output-file` - With cleanup
- [ ] `file-exists?` - Test file existence
- [ ] `directory?` - Test if directory
- [ ] `file-readable?` / `file-writable?` - Test permissions
- [ ] `delete-file` - Delete file
- [ ] `rename-file` - Rename/move file
- [ ] `copy-file` - Copy file
- [ ] `make-directory` - Create directory
- [ ] `file-size` - Get file size in bytes
- [ ] `directory-list` - List directory contents
- [ ] `path-join` - Join path components
- [ ] `path-directory` / `path-filename` / `path-extension` - Path parts
- [ ] `current-directory` - Get current working directory

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

- [ ] `when` - Conditional with implicit begin
- [ ] `unless` - Negated conditional with implicit begin
- [ ] `let*` - Sequential bindings
- [ ] `do` - General iteration (R5RS style)
- [ ] Named `let` - Recursive loop binding
- [ ] `dotimes` - Counted iteration
- [ ] `dolist` - List iteration

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

- [ ] `gensym` - Generate unique symbol
- [ ] `symbol=?` - Symbol equality (currently use `equal?`)

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
- [ ] `round` - Round to nearest integer (ties to even)
- [ ] `truncate` - Truncate toward zero
- [ ] `random` - Random number generation
- [ ] `random-seed` - Set random seed
- [ ] `pi` - Mathematical constant 3.14159...
- [ ] `e` - Mathematical constant 2.71828...
- [ ] `sign` / `signum` - Sign of number (-1, 0, 1)

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

- [ ] `pmap` - Parallel map
- [ ] `pfor-each` - Parallel for-each
- [ ] `future` - Create async computation
- [ ] `deref` - Get value (with optional timeout)
- [ ] `realized?` - Check if future is complete
- [ ] `pcalls` - Execute multiple thunks in parallel
- [ ] `atom` - Create thread-safe mutable reference
- [ ] `swap!` - Atomically update atom with function
- [ ] `reset!` - Atomically set atom value
- [ ] `compare-and-set!` - CAS operation on atom

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

- [ ] `hash-map` / `make-hash-map` - Create hash map
- [ ] `hash-ref` - Lookup value (with optional default)
- [ ] `hash-set!` - Add/update entry
- [ ] `hash-remove!` - Remove entry
- [ ] `hash-has-key?` - Check key existence
- [ ] `hash-update!` - Update with function
- [ ] `hash-keys` / `hash-values` - Get keys/values as lists
- [ ] `hash->alist` / `alist->hash` - Conversion
- [ ] `hash-count` - Number of entries
- [ ] `hash-merge` - Merge maps
- [ ] `hash-clear!` - Remove all entries

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

- [ ] `module` - Define module with exports
- [ ] `import` - Import module symbols
- [ ] `require` - Load file and import
- [ ] Qualified symbol resolution (`module/symbol`)
- [ ] Private definitions (not exported)

---

## Implementation Notes

### Priority Order for Implementation

1. **Exception handling** - Everything else depends on being able to handle errors
2. **Input operations** - Needed for any interactive program
3. **String operations** - Fundamental for text processing
4. **Comparison operators & predicates** - Very frequently needed
5. **List utilities** - Core functional programming tools
6. **File I/O** - Essential for real applications
7. **Control flow macros** - Developer convenience
8. The rest can follow based on actual usage needs

### Implementation Strategies

**Java Primitives vs JLLL Implementations:**
- Simple operations (`when`, `unless`, `<=`, `zero?`) - JLLL macros/functions in `.jlll` files
- Performance-critical or Java-dependent features - Java primitives in `*Lib.java`
- String/file operations - wrap Java methods directly

**Testing:**
- Add tests in `JLLLTestCase.java` for Java primitives
- Add `:doc` metadata to JLLL-implemented functions
- Test edge cases: empty lists, null, boundary values

**Documentation:**
- Update `docs/primitives.md` with new functions
- Include examples in `:doc` strings

### Scheme Compatibility Notes

JLLL follows Scheme naming conventions but is not strictly R5RS/R7RS compatible:
- No character type (use single-char strings)
- No continuations (`call/cc`) - limits some patterns
- Mutable by default (Scheme prefers immutability)
- Keywords (`:foo`) are JLLL-specific
- Java interop is JLLL-specific

---

## Contributing

When implementing features from this roadmap:

1. **Format check**: `mvn spotless:check` (or `mvn spotless:apply` to fix)
2. **Run tests**: `mvn test`
3. **Add documentation**: `:doc` metadata for JLLL, JavaDoc for Java
4. **Update docs**: `docs/primitives.md` for new functions
5. **Commit cleanly**: Working tree should pass all checks
