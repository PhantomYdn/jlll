# JLLL Primitives

Built-in functions organized by library. All primitives are loaded automatically when JLLL starts.

## Kernel Library

Core language primitives.

### Control Flow

| Primitive | Description | Example |
|-----------|-------------|---------|
| `if` | Conditional | `(if (> x 0) "pos" "neg")` |
| `cond` | Multi-branch conditional | `(cond ((< x 0) "neg") (else "pos"))` |
| `case` | Value dispatch | `(case x ((1 2) "small") (else "big"))` |

### Definitions and Assignment

| Primitive | Description | Example |
|-----------|-------------|---------|
| `define` | Create binding (with optional metadata) | `(define x :doc "desc" 10)` |
| `set!` / `set` | Modify binding (preserves metadata) | `(set! x 20)` |
| `lambda` | Create procedure | `(lambda (x) (* x x))` |
| `defmacro` | Define macro | `(defmacro (when test . body) ...)` |
| `define-from` | Copy binding with metadata | `(define-from y 'x)` |

### List Construction

| Primitive | Description | Example |
|-----------|-------------|---------|
| `cons` | Create pair | `(cons 1 2)` => `(1 . 2)` |
| `car` | First element | `(car '(1 2 3))` => `1` |
| `cdr` | Rest of list | `(cdr '(1 2 3))` => `(2 3)` |
| `list` | Create list | `(list 1 2 3)` => `(1 2 3)` |

### Higher-Order Functions

| Primitive | Description | Example |
|-----------|-------------|---------|
| `apply` | Apply procedure to list | `(apply + '(1 2 3))` => `6` |
| `map` | Transform list | `(map (lambda (x) (* x 2)) '(1 2 3))` => `(2 4 6)` |
| `filter` | Select elements | `(filter (lambda (x) (> x 2)) '(1 2 3 4))` => `(3 4)` |
| `mapall` | Recursive map over nested lists | `(mapall inc '((1 2) (3 4)))` |

### Evaluation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `eval` | Evaluate expression | `(eval '(+ 1 2))` => `3` |
| `quote` | Return unevaluated | `(quote x)` => `x` |
| `quasiquote` | Template with unquote | `` `(a ,x b) `` |

### Sequencing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `begin` | Sequence expressions | `(begin (print "hi") 42)` => `42` |

### String Operations

| Primitive | Description | Example |
|-----------|-------------|---------|
| `concat` | Concatenate strings | `(concat "a" "b" "c")` => `"abc"` |

### Environment

| Primitive | Description | Example |
|-----------|-------------|---------|
| `current-environment` | List bound symbols | `(current-environment)` |
| `top-environment` | List top-level symbols | `(top-environment)` |

### Loading

| Primitive | Description | Example |
|-----------|-------------|---------|
| `load` | Load JLLL from file path | `(load "path/to/script.jlll")` |
| `load-url` | Load JLLL from URL | `(load-url "http://example.com/lib.jlll")` |
| `load-system-script` | Load internal script | `(load-system-script "math.jlll")` |
| `load-lib` | Load Java library class | `(load-lib "com.example.MyLib")` |

### Modules

| Primitive | Description | Example |
|-----------|-------------|---------|
| `module` | Define a module with body | `(module mymath (export square) (define (square x) (* x x)))` |
| `export` | Mark symbols for export | `(export fn1 fn2)` or `(export *)` for all |
| `import` | Import module exports | `(import mymath)` or `(import mymath :prefix m/)` |
| `require` | Load file and import | `(require "file.jlll")` or `(require "file.jlll" :as m)` |

**Import Options:**
- `:only (sym1 sym2)` - Import only specified symbols
- `:except (sym1)` - Import all except specified symbols
- `:prefix foo/` - Add prefix to imported names

**Qualified Access:**
- Use `module/symbol` syntax to access exported symbols without importing: `(mymath/square 5)`

### Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `time` | Measure execution time (ms) | `(time (fib 30))` => `245` |
| `sleep` | Pause execution (ms) | `(sleep 1000)` |
| `quit` / `exit` | Exit JLLL | `(quit)` |
| `describe` | Describe object (includes metadata) | `(describe 'my-var)` |

### Metadata

See [Metadata](metadata.md) for detailed documentation.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `doc` | Get `:doc` metadata | `(doc 'my-func)` => `"description"` |
| `meta` | Get metadata by key | `(meta 'x :version)` => `"1.0"` |
| `meta` | Get all metadata | `(meta 'x)` => `((:doc . "desc"))` |
| `set-meta!` | Set metadata on binding | `(set-meta! 'x :author "Jane")` |

### Macro Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `jlll-macro-expand` | Expand macro once | `(jlll-macro-expand let ...)` |
| `jlll-extract-body` | Get procedure body | `(jlll-extract-body square)` |

### Exception Handling

See [Special Forms](special-forms.md) for `try`, `guard`, and `with-exception-handler` syntax.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `raise` | Raise an exception | `(raise "error message")` |
| `error` | Raise with concatenated message | `(error "Not found: " item)` |
| `exception?` | Test if value is exception | `(exception? e)` => `true` |
| `exception-message` | Get exception message | `(exception-message e)` => `"error"` |
| `exception-cause` | Get underlying cause | `(exception-cause e)` => `null` or cause |
| `with-exception-handler` | Install handler for thunk | `(with-exception-handler handler thunk)` |

## Math Library

Arithmetic and numeric operations.

### Basic Arithmetic

| Primitive | Description | Example |
|-----------|-------------|---------|
| `+` | Addition | `(+ 1 2 3)` => `6` |
| `-` | Subtraction | `(- 10 3)` => `7` |
| `*` | Multiplication | `(* 2 3 4)` => `24` |
| `/` | Division | `(/ 10 2)` => `5` |

### Comparison

| Primitive | Description | Example |
|-----------|-------------|---------|
| `<` | Less than | `(< 1 2)` => `true` |
| `>` | Greater than | `(> 2 1)` => `true` |
| `<=` | Less than or equal | `(<= 1 2)` => `true` |
| `>=` | Greater than or equal | `(>= 2 1)` => `true` |
| `=` | Numeric equality | `(= 1 1)` => `true` |
| `between` | Range check | `(between 1 10 5)` => `true` |

### Logic

| Primitive | Description | Example |
|-----------|-------------|---------|
| `and` | Logical and | `(and true true)` => `true` |
| `or` | Logical or | `(or false true)` => `true` |
| `not` | Logical not | `(not false)` => `true` |

### Math Functions

| Primitive | Description | Example |
|-----------|-------------|---------|
| `sqrt` | Square root | `(sqrt 16)` => `4.0` |
| `abs` | Absolute value | `(abs -5)` => `5` |
| `floor` | Round down | `(floor 3.7)` => `3.0` |
| `ceil` | Round up | `(ceil 3.2)` => `4.0` |
| `sin` | Sine | `(sin 0)` => `0.0` |
| `cos` | Cosine | `(cos 0)` => `1.0` |
| `tan` | Tangent | `(tan 0)` => `0.0` |
| `asin` | Arc sine | `(asin 0)` => `0.0` |
| `acos` | Arc cosine | `(acos 1)` => `0.0` |
| `atan` | Arc tangent | `(atan 0)` => `0.0` |
| `exp` | Exponential | `(exp 1)` => `2.718...` |
| `log` | Natural logarithm | `(log 2.718)` => `~1.0` |

### Aggregation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `max` | Maximum value | `(max 1 5 3)` => `5` |
| `min` | Minimum value | `(min 1 5 3)` => `1` |

### Integer Division

| Primitive | Description | Example |
|-----------|-------------|---------|
| `quotient` | Integer division (truncate toward zero) | `(quotient 13 4)` => `3` |
| `remainder` | Remainder (sign follows dividend) | `(remainder -13 4)` => `-1` |
| `modulo` | Modulo (sign follows divisor) | `(modulo -13 4)` => `3` |

### Exponentiation and Number Theory

| Primitive | Description | Example |
|-----------|-------------|---------|
| `expt` | Exponentiation | `(expt 2 10)` => `1024.0` |
| `gcd` | Greatest common divisor | `(gcd 12 18)` => `6` |
| `lcm` | Least common multiple | `(lcm 4 6)` => `12` |

### Rounding

| Primitive | Description | Example |
|-----------|-------------|---------|
| `round` | Round to nearest integer | `(round 3.5)` => `4` |
| `truncate` | Truncate toward zero | `(truncate -3.7)` => `-3` |
| `sign` | Sign of number (-1, 0, or 1) | `(sign -5)` => `-1` |

### Constants

| Variable | Description | Value |
|----------|-------------|-------|
| `pi` | Mathematical constant π | `3.14159...` |
| `e` | Mathematical constant e (Euler's number) | `2.71828...` |

## List Library

List manipulation functions.

### Accessors

| Primitive | Description | Example |
|-----------|-------------|---------|
| `car` | First element | `(car '(1 2))` => `1` |
| `cdr` | Rest of list | `(cdr '(1 2))` => `(2)` |
| `caar`, `cadr`, etc. | Nested access | `(cadr '(1 2 3))` => `2` |
| `last` | Last element | `(last '(1 2 3))` => `3` |

### Construction

| Primitive | Description | Example |
|-----------|-------------|---------|
| `cons` | Prepend element | `(cons 0 '(1 2))` => `(0 1 2)` |
| `list` | Create list | `(list 1 2 3)` => `(1 2 3)` |
| `append` | Concatenate lists | `(append '(1 2) '(3 4))` => `(1 2 3 4)` |

### Transformation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `reverse` | Reverse list | `(reverse '(1 2 3))` => `(3 2 1)` |
| `length` | Count elements | `(length '(1 2 3))` => `3` |

### Conversion

| Primitive | Description | Example |
|-----------|-------------|---------|
| `list->vector` | List to Java array | `(list->vector '(1 2 3))` |
| `collection->list` | Java collection to list | `(collection->list java-list)` |
| `vector->list` | Alias for collection->list | `(vector->list arr)` |

## Predicates Library

Type checking and testing functions.

### Type Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `null?` | Test for null | `(null? '())` => `true` |
| `nil?` | Alias for null? | `(nil? null)` => `true` |
| `list?` | Test for list | `(list? '(1 2))` => `true` |
| `pair?` | Test for non-empty cons cell | `(pair? '(a . b))` => `true` |
| `atom?` | Test for non-pair (includes empty list) | `(atom? 'x)` => `true` |
| `number?` | Test for any number | `(number? 42)` => `true` |
| `integer?` | Test for integer | `(integer? 42)` => `true` |
| `keyword?` | Test for keyword | `(keyword? :foo)` => `true` |
| `jlll-bound?` | Test if symbol is bound | `(jlll-bound? 'x)` |

### Numeric Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `zero?` | Test for zero | `(zero? 0)` => `true` |
| `positive?` | Test for positive number | `(positive? 5)` => `true` |
| `negative?` | Test for negative number | `(negative? -3)` => `true` |
| `even?` | Test for even integer | `(even? 4)` => `true` |
| `odd?` | Test for odd integer | `(odd? 3)` => `true` |

### Keyword Conversions

| Primitive | Description | Example |
|-----------|-------------|---------|
| `keyword->symbol` | Convert keyword to symbol | `(keyword->symbol :foo)` => `foo` |
| `symbol->keyword` | Convert symbol to keyword | `(symbol->keyword 'foo)` => `:foo` |
| `keyword-name` | Get keyword name as string | `(keyword-name :foo)` => `"foo"` |

### Symbol Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `gensym` | Generate unique symbol | `(gensym)` => `G__1` |
| `gensym` | Generate with prefix | `(gensym "temp")` => `temp__2` |
| `symbol=?` | Test symbol equality | `(symbol=? 'foo 'foo)` => `true` |

## String Library

Comprehensive string manipulation operations. JLLL does not have a character type, so functions like `string-ref` return single-character strings.

### Length and Access

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string-length` | Get string length | `(string-length "hello")` => `5` |
| `substring` | Extract substring | `(substring "hello" 1 3)` => `"el"` |
| `string-ref` | Character at index | `(string-ref "hello" 0)` => `"h"` |

### Search

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string-index` | Find substring position | `(string-index "hello" "l")` => `2` |
| `string-contains?` | Check if contains substring | `(string-contains? "hello" "ell")` => `true` |

### Transformation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string-upcase` | Convert to uppercase | `(string-upcase "hello")` => `"HELLO"` |
| `string-downcase` | Convert to lowercase | `(string-downcase "HELLO")` => `"hello"` |
| `string-trim` | Remove leading/trailing whitespace | `(string-trim "  hello  ")` => `"hello"` |
| `string-trim-left` | Remove leading whitespace | `(string-trim-left "  hello")` => `"hello"` |
| `string-trim-right` | Remove trailing whitespace | `(string-trim-right "hello  ")` => `"hello"` |

### Manipulation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string-replace` | Replace all occurrences | `(string-replace "hello" "l" "L")` => `"heLLo"` |
| `string-split` | Split by delimiter | `(string-split "a,b,c" ",")` => `("a" "b" "c")` |
| `string-join` | Join with delimiter | `(string-join '("a" "b" "c") ",")` => `"a,b,c"` |

### Conversion

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string->number` | Parse string to number | `(string->number "42")` => `42` |
| `string->number` | Parse with radix | `(string->number "ff" 16)` => `255` |
| `number->string` | Number to string | `(number->string 42)` => `"42"` |
| `number->string` | Number with radix | `(number->string 255 16)` => `"ff"` |
| `string->list` | String to char list | `(string->list "abc")` => `("a" "b" "c")` |
| `list->string` | Char list to string | `(list->string '("a" "b" "c"))` => `"abc"` |

### Comparison

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string=?` | Equality | `(string=? "a" "a")` => `true` |
| `string<?` | Less than (lexicographic) | `(string<? "a" "b")` => `true` |
| `string>?` | Greater than | `(string>? "b" "a")` => `true` |
| `string<=?` | Less than or equal | `(string<=? "a" "a")` => `true` |
| `string>=?` | Greater than or equal | `(string>=? "b" "a")` => `true` |

### Case-Insensitive Comparison

| Primitive | Description | Example |
|-----------|-------------|---------|
| `string-ci=?` | Case-insensitive equality | `(string-ci=? "Hello" "hello")` => `true` |
| `string-ci<?` | Case-insensitive less than | `(string-ci<? "a" "B")` => `true` |
| `string-ci>?` | Case-insensitive greater than | `(string-ci>? "B" "a")` => `true` |
| `string-ci<=?` | Case-insensitive less or equal | `(string-ci<=? "a" "A")` => `true` |
| `string-ci>=?` | Case-insensitive greater or equal | `(string-ci>=? "a" "A")` => `true` |

### Construction

| Primitive | Description | Example |
|-----------|-------------|---------|
| `make-string` | Create repeated string | `(make-string 5 "x")` => `"xxxxx"` |
| `string-append` | Concatenate strings (alias for concat) | `(string-append "a" "b")` => `"ab"` |
| `string-empty?` | Test for empty string | `(string-empty? "")` => `true` |

## IO Library

Input/output operations.

### Output

| Primitive | Description | Example |
|-----------|-------------|---------|
| `print` | Print without newline | `(print "hello")` |
| `println` | Print with newline | `(println "hello")` |
| `newline` | Print newline | `(newline)` |

### Input

| Primitive | Description | Example |
|-----------|-------------|---------|
| `read` | Read and parse JLLL expression | `(read)` or `(read port)` |
| `read-line` | Read line as string | `(read-line)` or `(read-line port)` |
| `read-char` | Read single character as string | `(read-char)` or `(read-char port)` |
| `peek-char` | Peek next character without consuming | `(peek-char)` or `(peek-char port)` |
| `char-ready?` | Check if input available | `(char-ready?)` or `(char-ready? port)` |
| `eof-object?` | Test for end-of-file object | `(eof-object? x)` => `true` if x is EOF |

All input functions return the EOF object (printed as `#<eof>`) when end of input is reached.
Use `eof-object?` to test for it:

```lisp
(let loop ()
  (define line (read-line))
  (unless (eof-object? line)
    (println "Got: " line)
    (loop)))
```

### Streams

| Primitive | Description | Example |
|-----------|-------------|---------|
| `stdin` | Standard input stream (BufferedReader) | `stdin` |
| `stdout` | Standard output stream (PrintWriter) | `stdout` |

## File Library

File system operations for reading, writing, and manipulating files and paths.

### File Reading and Writing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `slurp` | Read entire resource to string | `(slurp "file.txt")` |
| `slurp` | Read from URL | `(slurp "https://example.com/data")` |
| `slurp` | Read from classpath | `(slurp "classpath:config.jlll")` |
| `spit` | Write string to file | `(spit "file.txt" "content")` |
| `spit` | Append to file | `(spit "file.txt" "more" :append true)` |

### Port-Based I/O

| Primitive | Description | Example |
|-----------|-------------|---------|
| `open-input-file` | Open file for reading | `(open-input-file "file.txt")` |
| `open-output-file` | Open file for writing | `(open-output-file "out.txt")` |
| `close-input-port` | Close input port | `(close-input-port port)` |
| `close-output-port` | Close output port | `(close-output-port port)` |
| `call-with-input-file` | Read with auto-close | `(call-with-input-file "f.txt" (lambda (p) (read-line p)))` |
| `call-with-output-file` | Write with auto-close | `(call-with-output-file "f.txt" (lambda (p) (display "hi" p)))` |

### File System Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `file-exists?` | Test if file exists | `(file-exists? "file.txt")` => `true` |
| `directory?` | Test if path is directory | `(directory? "mydir")` => `true` |
| `file-readable?` | Test if file is readable | `(file-readable? "file.txt")` => `true` |
| `file-writable?` | Test if file is writable | `(file-writable? "file.txt")` => `true` |

### File Operations

| Primitive | Description | Example |
|-----------|-------------|---------|
| `delete-file` | Delete a file | `(delete-file "file.txt")` |
| `rename-file` | Rename/move a file | `(rename-file "old.txt" "new.txt")` |
| `copy-file` | Copy a file | `(copy-file "src.txt" "dst.txt")` |
| `make-directory` | Create directory (with parents) | `(make-directory "a/b/c")` |
| `file-size` | Get file size in bytes | `(file-size "file.txt")` => `1024` |
| `directory-list` | List directory contents | `(directory-list "mydir")` => `("a.txt" "b.txt")` |
| `current-directory` | Get current working directory | `(current-directory)` => `"/home/user"` |

### Path Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `path-join` | Join path components | `(path-join "a" "b" "c.txt")` => `"a/b/c.txt"` |
| `path-directory` | Get parent directory | `(path-directory "/a/b/c.txt")` => `"/a/b"` |
| `path-filename` | Get filename | `(path-filename "/a/b/c.txt")` => `"c.txt"` |
| `path-extension` | Get file extension | `(path-extension "/a/b/c.txt")` => `"txt"` |

### Examples

```lisp
;; Read a file
(define content (slurp "config.txt"))

;; Write to file
(spit "output.txt" "Hello, World!")

;; Append to file
(spit "log.txt" "New entry\n" :append true)

;; Safe file reading with cleanup
(call-with-input-file "data.txt"
  (lambda (port)
    (read-line port)))

;; List files and filter
(filter (lambda (f) (string-contains? f ".txt"))
        (directory-list "."))
```

## Hash Library

Mutable hash tables (associative arrays) with O(1) access. Backed by `LinkedHashMap` to preserve insertion order.

### Creation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `make-hash` | Create empty hash map | `(make-hash)` |
| `hash-map` | Create with key-value pairs | `(hash-map :a 1 :b 2)` |
| `alist->hash` | Create from association list | `(alist->hash '((:a . 1) (:b . 2)))` |

### Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `hash?` | Test if value is hash map | `(hash? h)` => `true` |
| `hash-has-key?` | Test if key exists | `(hash-has-key? h :a)` => `true` |

### Access

| Primitive | Description | Example |
|-----------|-------------|---------|
| `hash-ref` | Get value for key | `(hash-ref h :a)` => `1` |
| `hash-ref` | Get with default | `(hash-ref h :missing 0)` => `0` |
| `hash-keys` | Get all keys as list | `(hash-keys h)` => `(:a :b)` |
| `hash-values` | Get all values as list | `(hash-values h)` => `(1 2)` |
| `hash-count` | Number of entries | `(hash-count h)` => `2` |

### Mutation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `hash-set!` | Add/update entry | `(hash-set! h :c 3)` => `3` |
| `hash-remove!` | Remove entry | `(hash-remove! h :a)` => `1` (removed value) |
| `hash-update!` | Update with function | `(hash-update! h :count (lambda (x) (+ x 1)))` |
| `hash-clear!` | Remove all entries | `(hash-clear! h)` => empty map |

### Conversion

| Primitive | Description | Example |
|-----------|-------------|---------|
| `hash->alist` | Convert to association list | `(hash->alist h)` => `((:a . 1) (:b . 2))` |
| `alist->hash` | Create from association list | `(alist->hash '((:a . 1)))` |
| `hash-merge` | Merge two maps | `(hash-merge h1 h2)` => new merged map |

### Examples

```lisp
;; Create and populate a hash map
(define config (make-hash))
(hash-set! config :host "localhost")
(hash-set! config :port 8080)

;; Or create with initial values
(define config (hash-map :host "localhost" :port 8080))

;; Access values
(hash-ref config :host)           ; => "localhost"
(hash-ref config :timeout 30)     ; => 30 (default)

;; Check for keys
(hash-has-key? config :host)      ; => true

;; Update value with function
(hash-update! config :port (lambda (p) (+ p 1)))

;; Convert to list for iteration
(for-each
  (lambda (pair)
    (println (car pair) ": " (cdr pair)))
  (hash->alist config))

;; Merge configurations
(define defaults (hash-map :timeout 30 :retries 3))
(define merged (hash-merge defaults config))
```

## Concurrency Library

Parallel execution primitives for async computations and thread-safe state.

### Futures

Futures represent asynchronous computations that execute in parallel.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `future` | Create async computation | `(future (expensive-fn x))` |
| `deref` | Get value (blocks if not ready) | `(deref f)` |
| `deref` | Get with timeout and default | `(deref f 1000 'timeout)` |
| `realized?` | Check if completed | `(realized? f)` => `true` |
| `future?` | Test if value is future | `(future? f)` => `true` |

### Atoms

Atoms provide thread-safe mutable references with atomic updates.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `atom` | Create thread-safe reference | `(atom 0)` |
| `deref` | Get current value | `(deref a)` => `0` |
| `reset!` | Set to new value | `(reset! a 42)` => `42` |
| `swap!` | Update with function | `(swap! a (lambda (x) (+ x 1)))` |
| `compare-and-set!` | CAS operation | `(compare-and-set! a 0 1)` => `true` |
| `jlll-atom?` | Test if value is atom | `(jlll-atom? a)` => `true` |

**Note:** `jlll-atom?` tests for JLLL atoms (thread-safe references). The existing `atom?` predicate tests if a value is not a pair (i.e., any non-cons value).

### Parallel Operations

| Primitive | Description | Example |
|-----------|-------------|---------|
| `pmap` | Parallel map over list | `(pmap expensive-fn '(1 2 3 4))` |
| `pfor-each` | Parallel for-each (side effects) | `(pfor-each process items)` |
| `pcalls` | Execute thunks in parallel | `(pcalls fn1 fn2 fn3)` |

### Examples

```lisp
;; Future - async computation
(define f (future (do
  (sleep 1000)
  (* 42 42))))
(do-other-work)
(println "Result: " (deref f))  ; blocks until ready

;; Future with timeout
(define result (deref f 500 'not-ready))

;; Atom - thread-safe counter
(define counter (atom 0))
(swap! counter (lambda (x) (+ x 1)))  ; => 1
(swap! counter (lambda (x) (+ x 1)))  ; => 2
(deref counter)                        ; => 2
(reset! counter 0)                     ; => 0

;; CAS for lock-free algorithms
(define state (atom 'idle))
(when (compare-and-set! state 'idle 'running)
  (do-work)
  (reset! state 'idle))

;; Parallel map - process list in parallel
(define squares (pmap (lambda (x) (* x x)) '(1 2 3 4 5 6 7 8)))
;; => (1 4 9 16 25 36 49 64)

;; Parallel for-each - side effects in parallel
(define results (atom '()))
(pfor-each 
  (lambda (url) 
    (swap! results (lambda (r) (cons (fetch url) r))))
  urls)

;; Execute multiple independent computations
(define (fetch-all)
  (pcalls
    (lambda () (slurp "http://api1.example.com"))
    (lambda () (slurp "http://api2.example.com"))
    (lambda () (slurp "http://api3.example.com"))))
```

### Thread Safety Notes

- **Futures** execute in Java's `ForkJoinPool.commonPool()`. Each future receives a snapshot of the environment at creation time.
- **Atoms** use compare-and-swap for lock-free atomic updates. The update function in `swap!` may be called multiple times if there is contention.
- **`call/cc`** (continuations) cannot cross thread boundaries - continuations captured in one thread cannot be invoked from another.

## Reflect Library (Java Interop)

See [Java Interop](java-interop.md) for detailed documentation.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `new` | Create Java object | `(new "java.util.ArrayList")` |
| `invoke` | Call instance method | `(invoke list "add" item)` |
| `invoke-static` | Call static method | `(invoke-static "Math" "sqrt" 2)` |
| `peek` | Get instance field | `(peek point "x")` |
| `poke` | Set instance field | `(poke point "x" 10)` |
| `peek-static` | Get static field | `(peek-static "Integer" "MAX_VALUE")` |
| `poke-static` | Set static field | `(poke-static obj "field" value)` |
| `instanceof?` | Type check | `(instanceof? obj "java.util.List")` |
| `class` | Get Class object | `(class "java.lang.String")` |

## SQL Library

Database operations (when enabled with `-Ddburl=...`).

| Primitive | Description | Example |
|-----------|-------------|---------|
| `sql-query` | Execute SELECT | `(sql-query "SELECT * FROM users")` |
| `sql-update` | Execute INSERT/UPDATE/DELETE | `(sql-update "INSERT INTO users VALUES (?)" name)` |
