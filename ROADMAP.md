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

## 8. Dynamic Classpath and Dependencies (Important)

Add external Maven dependencies at runtime with environment-scoped classloaders. Dependencies are 
associated with child environments, providing isolation without polluting the parent environment.

### Core Concepts

- **Environment-scoped:** Dependencies are tied to child environments, not global
- **Classloader isolation:** Each environment with dependencies gets its own classloader
- **Full inheritance:** Child environments inherit all parent bindings plus new classpath
- **Maven resolution:** Full transitive dependency resolution using Maven coordinates

### Proposed Implementation

```lisp
;; ============================================================
;; EXECUTE WITH DEPENDENCIES
;; ============================================================

;; Execute code in child environment with additional dependencies
(env :depends '("com.google.code.gson:gson:2.10.1"
                "org.apache.commons:commons-lang3:3.14.0")
  (define Gson (class "com.google.gson.Gson"))
  (define gson (new Gson))
  (invoke gson "toJson" '(1 2 3)))
;; Returns: result of body
;; Child environment is discarded after execution

;; With custom repositories
(env :depends '("com.example:internal-lib:1.0.0")
     :repos '("https://maven.example.com/releases")
  (use-internal-lib))

;; ============================================================
;; CREATE REUSABLE ENVIRONMENT
;; ============================================================

;; Without body: returns environment object for later use
(define json-env (env :depends '("com.google.code.gson:gson:2.10.1")))

;; Execute in existing environment using eval :env
(eval :env json-env '(define Gson (class "com.google.gson.Gson")))
(eval :env json-env '(new Gson))

;; ============================================================
;; MODULES WITH DEPENDENCIES
;; ============================================================

;; Module with own classpath - clean abstraction over external libraries
(module json-utils
  :depends '("com.google.code.gson:gson:2.10.1")
  
  (export parse stringify)
  
  (define Gson (class "com.google.gson.Gson"))
  (define gson (new Gson))
  
  (define (parse json-string)
    :doc "Parse JSON string to JLLL data structure"
    (invoke gson "fromJson" json-string (class "java.util.Map")))
  
  (define (stringify obj)
    :doc "Convert JLLL data to JSON string"
    (invoke gson "toJson" obj)))

;; Import and use - module's classloader handles Gson classes
(import json-utils)
(json-utils/parse "{\"name\": \"Alice\"}")

;; ============================================================
;; ENVIRONMENT SWITCHING
;; ============================================================

;; Switch current environment to specified child env
(env-switch! json-env)

;; Return to parent environment (no argument)
(env-switch!)

;; REPL workflow example:
(define dev-env (env :depends '("gson:2.10.1" "guava:32.1.2-jre")))
(env-switch! dev-env)          ; now in child env with deps
;; ... work interactively ...
(env-switch!)                  ; back to parent

;; ============================================================
;; INTROSPECTION
;; ============================================================

;; List JARs in current environment's classpath
(env-classpath)                       ; => ("/path/to/gson.jar" ...)

;; List JARs in specific environment
(env-classpath json-env)

;; Get parent environment
(env-parent)                          ; current env's parent
(env-parent json-env)                 ; specific env's parent
```

**Dependency notation:**
- Full Maven coordinates: `"groupId:artifactId:version"`
- Short form for well-known groups: `"gson:2.10.1"` (if unambiguous)
- Keyword arguments also supported: `(:group "com.google.code.gson" :artifact "gson" :version "2.10.1")`

**Repository sources (in resolution order):**
1. Local Maven cache (`~/.m2/repository`)
2. Maven Central
3. Custom repositories specified via `:repos`

**Implementation notes:**
- Use Maven Resolver (Aether) for dependency resolution
- Each `env :depends` creates a new `URLClassLoader` with parent delegation
- Module classloaders are preserved when functions are exported/imported
- No dependency unloading - exit child environment instead
- Modules with `:depends` cannot be redefined (immutable once created)
- Different modules can depend on different versions of same library (isolated classloaders)

### Checklist

- [x] `env :depends` - Execute code in child environment with Maven dependencies
- [x] `env :depends :repos` - Support custom Maven repositories
- [x] `env :depends` (no body) - Return environment object
- [x] `eval :env` integration - Execute in specific environment (extend existing)
- [x] `module :depends` - Module with own classpath and exports
- [x] `env-switch!` - Switch current environment to child
- [x] `env-switch!` (no arg) - Return to parent environment
- [x] `env-classpath` - List JARs in environment's classpath
- [x] `env-parent` - Get parent environment
- [x] `env?` - Test if value is an Environment
- [x] Maven coordinate parsing - Full notation (groupId:artifactId:version)
- [x] Transitive dependency resolution
- [x] Local ~/.m2 cache integration
- [x] Isolated classloaders per environment/module

---

## 9. Control Flow Macros (Important)

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

## 10. Symbol and Macro Utilities (Important)

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

## 11. Math Completion (Nice to Have)

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

## 12. Parallel Execution (Nice to Have)

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

## 13. Hash Maps / Dictionaries (Nice to Have)

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

## 14. Modules / Namespaces (Nice to Have)

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

## 15. Regular Expressions (Important)

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

- [x] `regex-match` - First match or false
- [x] `regex-match-all` - All matches as list
- [x] `regex-replace` - Replace all occurrences (with string or function)
- [x] `regex-replace-first` - Replace first occurrence
- [x] `regex-split` - Split string by pattern
- [x] `regex-matches?` - Test if entire string matches
- [x] `regex-find` - Index of first match

---

## 16. JSON Support (Important)

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

- [x] `json-parse` / `json-read` - Parse JSON string to JLLL data
- [x] `json-stringify` / `json-write` - Convert JLLL data to JSON string
- [x] `json-read-file` - Parse JSON from file
- [x] `json-write-file` - Write JLLL data as JSON to file
- [x] `:pretty` option for formatted output
- [x] `:keywords` option for keyword keys instead of strings

---

## 17. Date/Time Operations (Important)

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

- [x] `now` - Current timestamp in milliseconds
- [x] `current-time` - Current time as hash-map
- [x] `date-format` - Format timestamp to string
- [x] `date-parse` - Parse string to timestamp
- [x] `date-add` - Add time units to timestamp
- [x] `date-diff` - Difference between timestamps
- [x] `date-year` / `date-month` / `date-day` / `date-hour` / `date-minute` / `date-second` / `date-day-of-week` - Extract components
- [x] `date->list` - Decompose to list
- [x] `make-date` - Create timestamp from components
- [x] `date<?` / `date>?` / `date=?` - Comparisons

---

## 18. Debugging and Development Tools (Nice to Have)

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

- [x] `trace` / `untrace` - Deep tracing (all procedure calls)
- [x] `traced?` - Check if tracing is enabled
- [x] `assert` - Assertion with optional message
- [x] `type-of` - Get type name as string
- [x] `identity` - Return argument unchanged
- [x] `constantly` - Function that always returns given value
- [x] `complement` - Negate a predicate
- [x] `tap` - Debug inspection in pipelines
- [x] `inspect` - Detailed object inspection

### AI Tool Traceability

When AI uses tools (via the `eval` tool), output is captured and returned to the AI for visibility.
For debugging AI interactions, a traceability option can echo this output to the user console.

**Current Implementation:**
- `CapturingConsole` captures all output during tool execution
- AI sees both captured output and return values
- User console is restored after tool execution

**Future Enhancement:**
```lisp
;; Enable AI tool tracing (echo tool output to user)
(ai-configure :trace-tools true)

;; When enabled:
;; 1. User sees what AI's eval tool prints
;; 2. Helps debug AI-generated code
;; 3. Provides transparency into AI actions
```

**Use Cases:**
- Debugging when AI-generated code doesn't work as expected
- Understanding what the AI "sees" during tool execution
- Auditing AI interactions for security/compliance

---

## 19. Environment and System (Implemented)

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

- [x] `getenv` - Get environment variable
- [x] `getenv-all` - All environment variables as hash-map
- [x] `get-property` - Get Java system property
- [x] `set-property!` - Set Java system property
- [x] `hostname` - Machine hostname
- [x] `user-name` / `user-home` - User info
- [x] `os-name` / `os-arch` - OS info
- [x] `java-version` - Java version string
- [x] `gc` - Trigger garbage collection
- [x] `memory-used` / `memory-free` / `memory-total` - Memory info
- [x] `memory-max` - Maximum heap size (bonus)
- [x] `available-processors` - Number of CPUs (bonus)

---

## 20. Formatted Output (Implemented)

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

- [x] `format` - Format string with directives
- [x] `printf` - Format and print to stdout
- [x] `fprintf` - Format and print to port
- [x] `display` - Write object for humans (no string quotes)
- [x] `write` - Write object for machine (readable representation)
- [x] Basic directives: `~a`, `~s`, `~d`, `~f`, `~%`, `~~`
- [ ] Width/padding support (future enhancement)

---

## 21. Sets (Implemented)

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

- [x] `make-set` / `set` - Create sets
- [x] `list->set` / `set->list` - Conversions
- [x] `set?` - Test if value is set
- [x] `set-empty?` - Test for empty set
- [x] `set-contains?` - Membership test
- [x] `set-count` - Number of elements
- [x] `set-add!` / `set-remove!` / `set-clear!` - Mutation
- [x] `set-union` / `set-intersection` / `set-difference` - Set operations
- [x] `set-symmetric-difference` - Elements in exactly one set
- [x] `set-subset?` / `set-superset?` / `set-disjoint?` / `set-equal?` - Set predicates
- [x] `set-for-each` / `set-map` / `set-filter` - Iteration

---

## 22. Lazy Sequences (Implemented)

Lazy evaluation for working with potentially infinite sequences and deferred computation.

See [docs/lazy-sequences.md](docs/lazy-sequences.md) for comprehensive documentation.

### Usage Examples

```lisp
;; Delay evaluation (create a "promise")
(define p (delay (begin (println "computing...") 42)))
(force p)                                 ; prints "computing...", => 42
(force p)                                 ; => 42 (cached, no recomputation)

;; Test if realized
(realized? p)                             ; => true after first force

;; Lazy cons - cdr is delayed
(lazy-cons 1 (lazy-cons 2 '()))           ; lazy list (1 ...)

;; Lazy range (possibly infinite)
(define naturals (lazy-range 0))          ; 0, 1, 2, 3, ...
(define evens (lazy-range 0 100 2))       ; 0, 2, 4, ..., 98
(define finite (lazy-range 0 10))         ; 0, 1, ..., 9

;; Take from lazy sequence (forces evaluation)
;; Note: JLLL take signature is (take list n)
(take naturals 5)                         ; => (0 1 2 3 4)
(take (lazy-range 0 100 2) 5)             ; => (0 2 4 6 8)

;; Lazy transformations (return lazy sequences)
(define squares (lazy-map (lambda (x) (* x x)) naturals))
(take squares 5)                          ; => (0 1 4 9 16)

(define evens (lazy-filter even? naturals))
(take evens 5)                            ; => (0 2 4 6 8)

;; Lazy drop
(take (lazy-drop 10 naturals) 5)          ; => (10 11 12 13 14)

;; Take while
(realize (lazy-take-while (lambda (x) (< x 5)) naturals))  ; => (0 1 2 3 4)

;; Realize entire lazy sequence (careful with infinite!)
(realize (lazy-range 0 10))               ; => (0 1 2 3 4 5 6 7 8 9)

;; Check if lazy (has unrealized cdr)
(lazy-seq? (lazy-range 0))                ; => true
(lazy-seq? '(1 2 3))                      ; => false

;; Iterate - generate from function
(define powers-of-2 (iterate (lambda (x) (* x 2)) 1))
(take powers-of-2 8)                      ; => (1 2 4 8 16 32 64 128)

;; Cycle - infinite repetition
(define abc (cycle '(a b c)))
(take abc 7)                              ; => (a b c a b c a)

;; Repeat - infinite copies
(take (repeat 'x) 5)                      ; => (x x x x x)
```

**Implementation notes:**
- Lazy sequences are implemented by allowing `Cons.cdr()` to contain a `LazyThunk`
- When `cdr()` is called, the thunk is automatically forced and cached
- `JlllDelay` provides the `delay`/`force` mechanism with memoization
- `LazyThunk` is used internally by lazy generators and transformations
- Unrealized lazy tails display as `...` in the REPL (e.g., `(0 ...)`)
- Thread-safe forcing with double-checked locking
- Existing `take`, `car`, `cdr` work transparently with lazy sequences

### Checklist

- [x] `delay` / `force` - Basic lazy evaluation primitives
- [x] `lazy-cons` - Cons with lazy cdr
- [x] `lazy-seq?` - Test for lazy sequence (unrealized cdr)
- [x] `lazy-range` - Lazy numeric range (finite or infinite)
- [x] `lazy-map` / `lazy-filter` - Lazy transformations (return lazy output)
- [x] `realize` - Force entire lazy sequence to list
- [x] `iterate` - Generate from repeated function application
- [x] `cycle` - Infinite repetition of sequence
- [x] `repeat` - Infinite copies of value
- [x] Additional predicates: `delay?`, `thunk?`, `has-lazy?`
- [x] Extended `realized?` to support delays and lazy sequences
- [x] Helper functions in `lazy.jlll`: `lazy-naturals`, `lazy-concat`, `lazy-zip`, etc.
- [x] Transparent integration: existing `take`, `drop`, `take-while`, `drop-while`, `any`, `every`, `find`, `reduce`, `list-ref` work with lazy sequences

---

## 23. AI Integration with LangChain4j (Important)

Integrate LLM capabilities into JLLL using [langchain4j](https://github.com/langchain4j/langchain4j).
Provides a session-centric model where all AI interactions go through sessions with conversation
memory, dynamic tool management, and lazy streaming responses.

**Dependencies on other sections:**
- Section 22 (Lazy Sequences) - All AI responses are lazy sequences
- Section 16 (JSON Support) - Tool argument parsing, structured responses
- Section 13 (Hash Maps) - Configuration and structured data
- Section 12 (Parallel Execution) - Async AI calls via `future`/`pmap`
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
;; ASYNC AI (using JLLL concurrency - Section 12)
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
- For async operations, use JLLL's `future`, `pmap`, `pcalls` from Section 12
- The `eval` tool is **enabled by default** - allows LLM to run JLLL code
- Eval tool errors return error messages to LLM (allows retry), not exceptions
- **Security note:** eval tool allows arbitrary code execution - security is user's responsibility

### Session Persistence (Implemented)

Save and restore AI sessions to/from JSON files:

```lisp
;; Save session to file (history, config, custom tools)
(ai-session-save coder "coder-session.json")
(ai-session-save coder "coder.json" :pretty true)  ; formatted output
(ai-session-save "current.json")                    ; save current session

;; Load session from file
(define restored (ai-session-load "coder-session.json"))
(ai-session-load "coder.json" :name "new-name")     ; override name
(ai-session-load "coder.json" :activate true)       ; load and activate
(ai-session-load "coder.json" :eval false)          ; don't add eval tool

;; Auto-save: automatically save session after each AI interaction
(ai-session-create :name "coder" :auto-save ".jlll/coder.json")
(ai-session-auto-save ".jlll/session.json")         ; enable on current session
(ai-session-auto-save false)                         ; disable auto-save
(ai-session-auto-save)                               ; query auto-save path or false
```

**Session file format (JSON):**
- Session identity (id, name)
- Configuration (provider, model, system prompt, temperature, maxTokens)
- Conversation history (user/AI messages)
- Custom tools (with procedure source code)
- Auto-save path (if enabled)

**Notes:**
- Built-in eval tool is not saved (always recreated fresh on load)
- Custom tools require `CompoundProcedure` (user-defined lambdas)
- ID collision on load results in new ID with `-restored-N` suffix
- `ai-session-save` auto-creates parent directories if needed
- Auto-save is triggered after each AI response completes
- Auto-save errors print warnings but don't interrupt the conversation

### Future Enhancements

These features may be added in future versions:

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
- [x] `ai-session-create` - Create session (`:name`, `:system`, `:model`, `:tools`, `:eval`, `:auto-save`)
- [x] `ai-session-activate` - Make session active for current environment
- [x] `ai-session-deactivate` - Deactivate current session
- [x] `ai-session-current` - Get active session or nil
- [x] `ai-sessions` - List all sessions
- [x] `ai-session-name` / `ai-session-id` - Get session identity
- [x] `ai-session?` - Test if value is a session
- [x] `ai-session-save` - Save session to JSON file (`:pretty` option, auto-creates directories)
- [x] `ai-session-load` - Load session from JSON file (`:name`, `:activate`, `:eval` options)
- [x] `ai-session-restore` - Load and activate session (convenience wrapper)
- [x] `ai-session-auto-save` - Enable/disable/query auto-save for a session

**Core Operations:**
- [x] `ai` - Chat using active session, returns lazy sequence (depends: Section 22)
- [x] `ai-history` - Get conversation history
- [x] `ai-clear` - Clear session history

**Tool Management:**
- [x] `ai-tool` - Define custom tool from JLLL lambda
- [x] `ai-tool-add` - Add tool to session dynamically
- [x] `ai-tool-remove` - Remove tool from session
- [x] `ai-tools` - List tools in session
- [x] `ai-tool?` - Test if value is a tool
- [x] Built-in `eval` tool (enabled by default, errors returned as messages)

**Configuration:**
- [x] `ai-configure` - Set API keys and defaults
- [x] `ai-config` - Get configuration as hash-map
- [x] Provider auto-detection from environment variables
- [x] Multi-provider support: OpenAI, Anthropic, Google AI Gemini, Ollama

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
- `ShellLib.java` - Shell command execution (bash function)
- `HttpLib.java` - HTTP server primitives (requires Javalin dependency)
- `WebConsoleLib.java` - Web console REPL server

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
| 26. Init File | - | None (CLI enhancement) |
| 27. Shell Execution | 12. Hash Maps | Return structured results |
| 28. Web Server | 12. Hash Maps | Request context, JSON responses |
| 28. Web Server | 15. JSON Support | JSON body parsing/rendering |
| 29. Web Console | 28. Web Server | Reuses HTTP infrastructure |

**Dependency-free sections:** 14, 17, 18, 19, 20, 26 can be implemented independently.

**Recommended implementation order for Phase 2:**
1. Regular expressions (14) - no dependencies
2. JSON support (15) - needs hash maps (done), lists (done)
3. Date/time (16) - needs hash maps (done)
4. Sets (20) - no dependencies
5. Lazy sequences (21) - needs control flow (done)
6. AI integration (22) - needs JSON, lazy sequences, concurrency (all done or above)
7. Debugging tools (17), Environment (18), Formatted output (19) - anytime

**Recommended implementation order for Phase 3 (new sections):**
1. Init file (26) - CLI only, no library dependencies
2. Shell execution (27) - needs hash maps (done)
3. Web server (28) - needs hash maps (done), JSON (done), adds Javalin dependency
4. Web console (29) - depends on web server (28)

### Scheme Compatibility Notes

JLLL follows Scheme naming conventions but is not strictly R5RS/R7RS compatible:
- No character type (use single-char strings)
- Continuations (`call/cc`) are available with replay capability for saved continuations
- Mutable by default (Scheme prefers immutability)
- Keywords (`:foo`) are JLLL-specific
- Java interop is JLLL-specific
- `format` uses Common Lisp / SRFI-28 directives (`~a`, `~s`, etc.)

---

## 24. Lexical Closures (Critical - Core Language Fix)

JLLL currently uses **dynamic scoping** instead of **lexical scoping**. When a `lambda` is
created, it does NOT capture its definition-time environment. This means inner functions
cannot close over variables from their enclosing scope - a fundamental expectation in
modern Lisp dialects.

### The Problem

```lisp
;; This SHOULD work with lexical closures but FAILS in JLLL:
(define (make-adder n)
  (lambda (x) (+ x n)))  ; lambda does NOT capture 'n'

(define add5 (make-adder 5))
(add5 10)  ; ERROR: Symbol is unbound: n

;; Counter example - also fails:
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c (make-counter))
(c)  ; ERROR: Symbol is unbound: count
```

### Root Cause

In `CompoundProcedure.java`:
- The class stores `variables` (parameter names) and `body` (code)
- It does **NOT** store the definition-time environment
- When called, `applyEvaluated()` creates `ProcEnvironment` with the **call-time** environment
- This is **dynamic scoping**, not lexical scoping

### Solution

Modify `CompoundProcedure` to:
1. Add a `lexicalEnv` field to store the definition-time environment
2. Pass this environment from the `lambda` primitive when creating the procedure
3. Use `lexicalEnv` (not call-time `env`) as parent for `ProcEnvironment` in `applyEvaluated()`

### Test Cases

After implementation, these should work:

```lisp
;; Basic closure
(define (make-adder n)
  (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(add5 10)                              ; => 15

;; Stateful closure
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
(define c (make-counter))
(c)                                    ; => 1
(c)                                    ; => 2
(c)                                    ; => 3

;; Currying
(define (curry f x)
  (lambda (y) (f x y)))
(define add10 (curry + 10))
(add10 5)                              ; => 15
```

### Post-Implementation: Simplify Existing Code

Once closures work, `constantly` and `complement` in `KernelLib.java` can be rewritten
as simple JLLL functions in `debug.jlll`:

```lisp
(define (constantly value)
  :doc "Returns a function that always returns the given value."
  (lambda args value))

(define (complement pred)
  :doc "Returns a function that negates the predicate."
  (lambda args (not (apply pred args))))
```

### Checklist

- [x] Add `lexicalEnv` field to `CompoundProcedure.java`
- [x] Modify constructors to capture definition-time environment
- [x] Update `applyEvaluated()` to use `lexicalEnv` as parent
- [x] Update `lambda` primitive in `KernelLib.java` to pass environment
- [x] Update `define` function form to pass environment
- [x] Create `ClosureTestCase.java` with comprehensive tests
- [x] Rewrite `constantly` as JLLL function
- [x] Rewrite `complement` as JLLL function
- [x] Update documentation in `docs/`

---

## 25. Java Interop Enhancements (Nice to Have)

Enhancements to Java interoperability beyond basic reflection.

### Implemented: SAM/Functional Interface Conversion

JLLL now automatically converts procedures/lambdas to Java functional interfaces (SAM - Single Abstract Method) when passed to `invoke`, `invoke-static`, or `new`. This works transparently with `Runnable`, `Comparator`, `ActionListener`, `Consumer`, `Function`, `Predicate`, etc.

See [docs/java-interop.md](docs/java-interop.md#functional-interface-support-sam-conversion) for documentation.

### Proposed: Explicit `proxy` for Multi-Method Interfaces

For interfaces with multiple abstract methods (like `MouseListener`, `DocumentListener`), an explicit `proxy` function would allow implementing all methods:

```lisp
;; Implement MouseListener with multiple methods
(define my-listener (proxy 'java.awt.event.MouseListener
  (hash-map
    "mouseClicked" (lambda (e) (println "clicked"))
    "mousePressed" (lambda (e) (println "pressed"))
    "mouseReleased" (lambda (e) null)
    "mouseEntered" (lambda (e) null)
    "mouseExited" (lambda (e) null))))

(invoke component "addMouseListener" my-listener)

;; Support multiple interfaces
(define handler (proxy '(Interface1 Interface2)
  (hash-map
    "method1" (lambda (args) ...)
    "method2" (lambda (args) ...))))
```

### Checklist

- [x] SAM/Functional interface auto-conversion (ByteBuddy)
- [ ] `proxy` - Explicit multi-method interface implementation
- [ ] Support for implementing multiple interfaces in single proxy

---

## 26. Init File (~/.jlllrc) (Implemented)

Auto-execute JLLL code at startup, similar to `.bashrc` for bash or `.zshrc` for zsh.
Allows users to customize their JLLL environment with aliases, utility functions,
library imports, and preferences.

### Behavior

- **Default location:** `~/.jlllrc` (single file in user's home directory)
- **Error handling:** Fail hard - exit with error code if init file fails to parse/execute
- **When loaded:** REPL mode only (not when running scripts via `jlll script.jlll`)
- **Interactive mode:** `-i` flag also loads init file after script execution

### CLI Options

```bash
# Normal REPL - loads ~/.jlllrc if it exists
jlll

# Specify alternate init file
jlll --rc /path/to/custom-init.jlll

# Run script (does NOT load init file)
jlll script.jlll

# Run script then enter REPL (loads init file)
jlll -i script.jlll
```

### Example Init File

```lisp
;; ~/.jlllrc - JLLL initialization file

;; Load frequently used libraries
(require "~/jlll-libs/utils.jlll")

;; Custom aliases
(define ll (lambda () (bash "ls -la")))
(define pwd (lambda () (hash-ref (bash "pwd") :stdout)))

;; Configure AI session defaults
(ai-configure :default-model "gpt-4o")

;; Set up commonly used AI session
(define coder (ai-session-create 
  :name "coder"
  :system "You are a helpful coding assistant"))

;; Print welcome message
(println "JLLL initialized. Type (help) for assistance.")
```

### Implementation Notes

- Check for `~/.jlllrc` existence before attempting to load
- Use `System.getProperty("user.home")` for home directory
- Evaluate in the same environment that will be used for REPL
- On error, print clear message with file path and line number, then exit with code 1
- `--rc` flag should accept absolute or relative paths

### Checklist

- [x] Add `--rc` option to `JlllCli.java`
- [x] Add `--no-rc` option to skip init file loading
- [x] Load `~/.jlllrc` automatically in REPL mode
- [x] Skip init file when running scripts (unless `-i` flag)
- [x] Clear error messages on init file failure
- [x] Support `~` expansion in paths
- [x] Document in `docs/init-file.md`

---

## 27. Shell Execution (Important)

Execute shell commands from JLLL and capture output. Essential for system integration,
build automation, and scripting tasks.

### Proposed Implementation

```lisp
;; Basic usage - returns structured result
(bash "ls -la")
;; => {:stdout "total 48\ndrwxr-xr-x..." :stderr "" :exit-code 0}

;; With timeout (milliseconds, default 120000 = 2 minutes)
(bash "long-running-command" :timeout 60000)

;; With working directory
(bash "npm test" :cwd "/path/to/project")

;; With stdin input (string)
(bash "wc -l" :input "line1\nline2\nline3")
;; => {:stdout "       3" :stderr "" :exit-code 0}

;; With stdin from port
(define in (open-input-file "data.txt"))
(bash "sort" :input in)

;; Combined options
(bash "grep -c pattern" :cwd "/tmp" :timeout 5000 :input search-text)

;; Check exit code
(if (zero? (hash-ref (bash "test -f file.txt") :exit-code))
    (println "File exists")
    (println "File not found"))

;; Pipe pattern (multiple commands)
(bash "cat file.txt | grep pattern | wc -l")

;; Environment variables
(bash "echo $MY_VAR" :env (hash-map "MY_VAR" "hello"))
```

### Return Value

The `bash` function returns a hash-map with three keys:

| Key | Type | Description |
|-----|------|-------------|
| `:stdout` | String | Standard output (may be empty) |
| `:stderr` | String | Standard error (may be empty) |
| `:exit-code` | Integer | Exit code (0 = success) |

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| command | String | (required) | Shell command to execute |
| `:timeout` | Integer | 120000 | Timeout in milliseconds |
| `:cwd` | String | current dir | Working directory |
| `:input` | String/Port | null | Data to pipe to stdin |
| `:env` | Hash-map | null | Additional environment variables |

### AI Tool Integration

The `bash` function is exposed as an AI tool for LLM use:

```lisp
;; Tool is automatically available in AI sessions
(ai-session-create :name "system-admin")

;; AI can now execute shell commands
(ai "List all running Java processes")
;; AI uses bash tool: (bash "ps aux | grep java")
```

**Security note:** Like the `eval` tool, the `bash` tool allows arbitrary command
execution. Security is the user's responsibility. Document risks clearly.

### Implementation Notes

- Use `ProcessBuilder` for command execution
- Shell is determined by OS: `/bin/sh -c` on Unix, `cmd /c` on Windows
- Capture both stdout and stderr in separate streams
- Handle timeout with `Process.waitFor(timeout, TimeUnit.MILLISECONDS)`
- On timeout, destroy process and return exit code -1 with stderr message
- For `:input`, support both String and InputStream (from ports)

### Checklist

- [x] `bash` - Execute shell command, return structured result
- [x] `:timeout` - Configurable timeout with process termination
- [x] `:cwd` - Working directory support
- [x] `:input` - Stdin from string or port
- [x] `:env` - Additional environment variables
- [x] AI tool registration in `ai.jlll`
- [x] Create `ShellLib.java`
- [x] Document in `docs/shell.md`

---

## 28. Web Server (Important)

Embedded HTTP server for exposing web endpoints from JLLL. Uses Javalin for a
lightweight, modern implementation with an imperative routing API.

### Core API

```lisp
;; Create server instance
(define server (http-server))

;; Route handlers - Javalin style
(http-get server "/hello" 
  (lambda (ctx) (http-result ctx "Hello, World!")))

(http-post server "/api/users" 
  (lambda (ctx)
    (define body (http-body-json ctx))
    (define user (create-user body))
    (http-json ctx user)))

(http-put server "/api/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (define body (http-body-json ctx))
    (update-user id body)))

(http-delete server "/api/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (delete-user id)
    (http-status ctx 204)))

;; Start with port (and optional host)
(http-start server :port 8080)
;; (http-start server :port 8080 :host "127.0.0.1")

;; Stop server
(http-stop server)
```

### Route Methods

```lisp
(http-get server path handler)
(http-post server path handler)
(http-put server path handler)
(http-delete server path handler)
(http-patch server path handler)
(http-head server path handler)
(http-options server path handler)

;; Before/after filters
(http-before server handler)        ; runs before all route handlers
(http-before server path handler)   ; runs before matching routes
(http-after server handler)         ; runs after all route handlers
(http-after server path handler)    ; runs after matching routes
```

### Path Parameters

```lisp
;; Named parameters with {name} syntax
(http-get server "/user/{id}" 
  (lambda (ctx)
    (http-path-param ctx "id")))

;; Wildcard paths
(http-get server "/files/*" 
  (lambda (ctx)
    (http-path ctx)))  ; returns full path like "/files/docs/readme.txt"
```

### Context Accessors

```lisp
;; Request information
(http-method ctx)              ; => :get, :post, etc.
(http-path ctx)                ; => "/api/users/123"
(http-path-param ctx "id")     ; => "123"
(http-query-param ctx "q")     ; => query string value or null
(http-query-params ctx)        ; => hash-map of all query params
(http-header ctx "Accept")     ; => header value or null
(http-headers ctx)             ; => hash-map of all headers
(http-body ctx)                ; => request body as string
(http-body-json ctx)           ; => parsed JSON body as hash-map
(http-remote-addr ctx)         ; => client IP address

;; Response helpers
(http-result ctx data)         ; set response body (string)
(http-json ctx data)           ; set JSON response with Content-Type
(http-html ctx html)           ; set HTML response with Content-Type
(http-status ctx 404)          ; set HTTP status code
(http-header! ctx "X-Custom" "value")  ; set response header
(http-redirect ctx "/login")   ; redirect (302)
(http-redirect ctx "/new" 301) ; redirect with custom status
```

### Static Files

```lisp
;; Serve static files from directory
(http-static server "/assets" "./public/")

;; Example: /assets/style.css serves ./public/style.css
```

### Error Handling

```lisp
;; Custom error handlers
(http-error server 404 
  (lambda (ctx)
    (http-json ctx (hash-map :error "Not found"))))

(http-error server 500
  (lambda (ctx)
    (http-json ctx (hash-map :error "Internal server error"))))

;; Exception handler
(http-exception server
  (lambda (ctx exception)
    (println "Error: " (invoke exception "getMessage"))
    (http-status ctx 500)
    (http-json ctx (hash-map :error "Something went wrong"))))
```

### Server Configuration

```lisp
;; Create server (optional context-path for all routes)
(http-server)                         ; basic server
(http-server :context-path "/api")    ; with base path prefix

;; Start with port and host options
(http-start server :port 8080)                          ; default host 0.0.0.0
(http-start server :port 8080 :host "127.0.0.1")       ; localhost only
```

### Example: REST API

```lisp
;; Create server
(define server (http-server))

;; In-memory data store
(define users (atom (hash-map)))
(define next-id (atom 1))

;; List all users
(http-get server "/users"
  (lambda (ctx)
    (http-json ctx (hash-values (deref users)))))

;; Get user by ID
(http-get server "/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (define user (hash-ref (deref users) id))
    (if user
        (http-json ctx user)
        (begin
          (http-status ctx 404)
          (http-json ctx (hash-map :error "User not found"))))))

;; Create user
(http-post server "/users"
  (lambda (ctx)
    (define body (http-body-json ctx))
    (define id (to-string (swap! next-id (lambda (n) (+ n 1)))))
    (define user (hash-merge body (hash-map :id id)))
    (swap! users (lambda (u) (hash-set! u id user)))
    (http-status ctx 201)
    (http-json ctx user)))

;; Start server
(http-start server :port 3000)
(println "Server running on http://localhost:3000")
```

### Future Enhancements

These features may be added later:

- **WebSocket support:** Bidirectional communication
- **Sessions:** Cookie-based session management
- **CORS middleware:** Cross-origin resource sharing
- **SSL/TLS:** HTTPS support
- **Request validation:** Schema-based body validation

### Implementation Notes

- Uses Javalin 6.x embedded in the JAR
- Each `http-server` creates a Javalin instance
- Handlers are JLLL lambdas wrapped in Java handlers
- Context is a thin wrapper around Javalin's `Context`
- Thread-safe: Javalin handles concurrency

### Checklist

- [x] Add Javalin dependency to `pom.xml`
- [x] Create `HttpLib.java` with server primitives
- [x] `http-server` - Create server instance
- [x] `http-start` / `http-stop` - Lifecycle management
- [x] `http-get/post/put/delete/patch` - Route registration
- [x] `http-before` / `http-after` - Filters
- [x] Context accessors for request data
- [x] Response helpers (json, html, status, redirect)
- [x] `http-static` - Static file serving
- [x] `http-error` - Custom error handlers
- [x] `http-exception` - Exception handler
- [x] `http-sse` - Server-Sent Events endpoint
- [x] `sse-send` / `sse-close` / `sse-keep-alive` - SSE operations
- [x] Document in `docs/web-server.md`

---

## 29. Web Console (Nice to Have)

Browser-based REPL for JLLL. Provides a rich interactive environment accessible
from any web browser, useful for remote development, demonstrations, and
environments where terminal access is limited.

### Starting the Web Console

**CLI:**
```bash
# Start web console on default port (8080)
java -jar jlll-cli.jar web

# Custom port
java -jar jlll-cli.jar web --port 3000

# Allow external connections (dangerous - use with caution!)
java -jar jlll-cli.jar web --bind 0.0.0.0
```

**From JLLL:**
```lisp
;; Start web console
(web-console)                    ; default port 8080

;; Custom port
(web-console :port 3000)

;; Get web console status
(web-console :status)            ; => {:running true :port 8080 :url "http://localhost:8080"}

;; Stop web console
(web-console :stop)
```

### Security

- **Localhost only by default:** Binds to `127.0.0.1`
- **Explicit opt-in for external access:** Requires `--bind 0.0.0.0` flag
- **Warning on startup:** Display security warning when external binding enabled
- **No authentication in v1:** Future enhancement

```
⚠️  WARNING: Web console bound to 0.0.0.0
    Anyone with network access can execute code!
    Use only in trusted networks or behind a firewall.
```

### Web UI Features

The browser interface provides a rich REPL experience:

- **Syntax highlighting:** CodeMirror-based editor with JLLL mode
- **Auto-complete:** Symbol completion from current environment
- **Command history:** Up/down arrows to navigate previous commands
- **Multi-line input:** Shift+Enter for continuation
- **Output formatting:**
  - Results in green
  - Errors in red
  - Printed output in default color
- **Mobile-friendly:** Responsive design for tablets/phones

### Communication Protocol

Uses **Server-Sent Events (SSE)** for server-to-client streaming:

```
Client                          Server
  |                               |
  |-- POST /eval {"code":"..."}-->|
  |                               |
  |<-- SSE: {type:"output"} ------|  (printed output)
  |<-- SSE: {type:"output"} ------|  (more output)
  |<-- SSE: {type:"result"} ------|  (final result)
  |<-- SSE: {type:"done"} --------|  (stream complete)
```

**Event types:**
- `output` - Printed output (from `println`, `print`, etc.)
- `result` - Evaluation result
- `error` - Error message
- `done` - Stream complete

### API Endpoints

```
GET  /                  - Web UI (HTML page)
POST /eval              - Evaluate code, returns SSE stream
GET  /complete?prefix=  - Auto-complete suggestions
GET  /history           - Get command history
POST /interrupt         - Interrupt running evaluation
GET  /status            - Server status
```

### Example Session

```
JLLL Web Console - http://localhost:8080

> (+ 1 2 3)
6

> (define (fib n)
    (if (<= n 1)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
fib

> (map fib (range 10))
(0 1 1 2 3 5 8 13 21 34)

> (println "Hello")
Hello
null
```

### Implementation Notes

- Reuses web server infrastructure (Section 28)
- Evaluation runs in isolated environment per session
- Output captured via `CapturingConsole` and streamed via SSE
- History stored in browser localStorage
- CodeMirror for syntax highlighting (bundled, no CDN dependency)

### Future Enhancements

- **Authentication:** Token or password protection
- **Multiple sessions:** Support concurrent users with isolated environments
- **Notebook mode:** Save/load evaluation history as notebooks
- **File browser:** Navigate and edit files in the project
- **WebSocket upgrade:** For bidirectional communication

### Checklist

- [ ] Add `web` subcommand to CLI
- [ ] `--port` option for custom port
- [ ] `--bind` option for external access
- [ ] Create `WebConsoleLib.java`
- [ ] `web-console` function for starting from JLLL
- [ ] SSE streaming for evaluation output
- [ ] HTML/CSS/JS for web UI
- [ ] CodeMirror integration for syntax highlighting
- [ ] Auto-complete endpoint
- [ ] Document in `docs/web-console.md`

---

## Future Enhancements

### Error Message Improvements

JLLL should provide user-friendly, verbose error messages that help both humans and AI:

- [ ] Detect common mistakes and provide suggestions
- [ ] "Did you forget to quote the class name?" for unbound Java class symbols (e.g., `java.util.Date` → suggest `'java.util.Date`)
- [ ] Include examples of correct syntax in error messages
- [ ] Suggest similar function names for typos (e.g., "Did you mean 'string-length'?")
- [ ] Better error context showing the expression that caused the error
- [ ] Stack traces with source locations when available

These improvements benefit both human users and AI using the eval tool.

---

## Contributing

When implementing features from this roadmap:

1. **Format check**: `mvn spotless:check` (or `mvn spotless:apply` to fix)
2. **Run tests**: `mvn test`
3. **Add documentation**: `:doc` metadata for JLLL, JavaDoc for Java
4. **Update docs**: `docs/primitives.md` for new functions
5. **Commit cleanly**: Working tree should pass all checks
