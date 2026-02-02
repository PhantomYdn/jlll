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

### Metadata and Documentation

See [Metadata](metadata.md) for detailed documentation on metadata system.

| Primitive | Description | Example |
|-----------|-------------|---------|
| `doc` | Get `:doc` metadata | `(doc 'my-func)` => `"description"` |
| `meta` | Get metadata by key | `(meta 'x :version)` => `"1.0"` |
| `meta` | Get all metadata | `(meta 'x)` => `((:doc . "desc"))` |
| `set-meta!` | Set metadata on binding | `(set-meta! 'x :author "Jane")` |
| `jlll-docs` | Access JLLL documentation | `(jlll-docs)` or `(jlll-docs "topic")` |
| `apropos` | Search for functions by name | `(apropos "string")` |

**Using `jlll-docs`:**

```lisp
;; List all documentation topics
(jlll-docs)

;; Read specific topic
(jlll-docs "java-interop")
(jlll-docs "primitives")
(jlll-docs "syntax")

;; Topic aliases are supported
(jlll-docs "interop")    ; same as "java-interop"
(jlll-docs "functions")  ; same as "primitives"
(jlll-docs "lazy")       ; same as "lazy-sequences"
```

Available topics: README, syntax, special-forms, procedures, primitives, macros, java-interop, lazy-sequences, metadata, system-prompt

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

### Random Numbers

| Primitive | Description | Example |
|-----------|-------------|---------|
| `random` | Random integer in [0, max) | `(random 100)` => `42` |
| `random-seed` | Set random seed for reproducibility | `(random-seed 12345)` |

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

### List Access

| Primitive | Description | Example |
|-----------|-------------|---------|
| `list-ref` | Element at index (0-based) | `(list-ref '(a b c) 1)` => `b` |
| `list-tail` | Sublist starting at index | `(list-tail '(a b c d) 2)` => `(c d)` |

### List Search

| Primitive | Description | Example |
|-----------|-------------|---------|
| `member` | Find element (equal?), returns sublist or false | `(member 'b '(a b c))` => `(b c)` |
| `memq` | Find element (eq?), returns sublist or false | `(memq 'b '(a b c))` => `(b c)` |
| `memv` | Find element (eqv?), returns sublist or false | `(memv 2 '(1 2 3))` => `(2 3)` |

### Association Lists

| Primitive | Description | Example |
|-----------|-------------|---------|
| `assoc` | Find pair by key (equal?) | `(assoc 'b '((a 1) (b 2)))` => `(b 2)` |
| `assq` | Find pair by key (eq?) | `(assq 'b '((a 1) (b 2)))` => `(b 2)` |
| `assv` | Find pair by key (eqv?) | `(assv 2 '((1 a) (2 b)))` => `(2 b)` |

### Iteration

| Primitive | Description | Example |
|-----------|-------------|---------|
| `for-each` | Apply proc for side effects | `(for-each println '(1 2 3))` |
| `fold-left` | Left fold (accumulate left to right) | `(fold-left + 0 '(1 2 3))` => `6` |
| `fold-right` | Right fold (accumulate right to left) | `(fold-right cons '() '(1 2 3))` => `(1 2 3)` |
| `reduce` | Fold with first element as init | `(reduce + '(1 2 3 4))` => `10` |

### List Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `any` | True if pred true for any element | `(any even? '(1 2 3))` => `true` |
| `every` | True if pred true for all elements | `(every positive? '(1 2 3))` => `true` |
| `find` | First element matching pred, or false | `(find even? '(1 2 3))` => `2` |

### List Generation

| Primitive | Description | Example |
|-----------|-------------|---------|
| `range` | Generate number sequence | `(range 5)` => `(0 1 2 3 4)` |
| `range` | With start and end | `(range 1 5)` => `(1 2 3 4)` |
| `range` | With step | `(range 0 10 2)` => `(0 2 4 6 8)` |
| `iota` | Alias for range | `(iota 5)` => `(0 1 2 3 4)` |
| `make-list` | Create list of n elements | `(make-list 3 'x)` => `(x x x)` |

### List Slicing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `take` | First n elements | `(take '(a b c d) 2)` => `(a b)` |
| `drop` | All but first n elements | `(drop '(a b c d) 2)` => `(c d)` |
| `take-while` | Leading elements while pred true | `(take-while even? '(2 4 5 6))` => `(2 4)` |
| `drop-while` | Drop leading while pred true | `(drop-while even? '(2 4 5 6))` => `(5 6)` |

### List Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `flatten` | Flatten nested lists | `(flatten '((1 2) (3 (4))))` => `(1 2 3 4)` |
| `zip` | Combine two lists pairwise | `(zip '(1 2) '(a b))` => `((1 a) (2 b))` |
| `unzip` | Split list of pairs | `(unzip '((1 a) (2 b)))` => `((1 2) (a b))` |
| `remove` | Remove all occurrences | `(remove 2 '(1 2 3 2))` => `(1 3)` |
| `delete` | Remove first occurrence | `(delete 2 '(1 2 3 2))` => `(1 3 2)` |
| `remove-duplicates` | Remove duplicates | `(remove-duplicates '(1 2 1 3))` => `(1 2 3)` |
| `append*` | Variadic append | `(append* '(1) '(2) '(3))` => `(1 2 3)` |

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
| `real?` | Test for real number | `(real? 3.14)` => `true` |
| `string?` | Test for string | `(string? "hello")` => `true` |
| `symbol?` | Test for symbol | `(symbol? 'foo)` => `true` |
| `boolean?` | Test for boolean | `(boolean? true)` => `true` |
| `keyword?` | Test for keyword | `(keyword? :foo)` => `true` |
| `procedure?` | Test for procedure | `(procedure? +)` => `true` |
| `jlll-bound?` | Test if symbol is bound | `(jlll-bound? 'x)` |

### Equality Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `eq?` | Identity comparison (same object) | `(eq? 'a 'a)` => `true` |
| `eqv?` | Value comparison | `(eqv? 1 1)` => `true` |
| `equal?` | Deep equality comparison | `(equal? '(1 2) '(1 2))` => `true` |

### Port Predicates

| Primitive | Description | Example |
|-----------|-------------|---------|
| `port?` | Test for I/O port | `(port? p)` |
| `input-port?` | Test for input port | `(input-port? p)` |
| `output-port?` | Test for output port | `(output-port? p)` |
| `eof-object?` | Test for end-of-file object | `(eof-object? x)` => `true` if EOF |

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

## Regex Library

Regular expression primitives for pattern matching and text manipulation. Patterns are cached for performance.

### Matching

| Primitive | Description | Example |
|-----------|-------------|---------|
| `regex-match` | First match (with groups) or false | `(regex-match "\\d+" "abc123")` => `"123"` |
| `regex-match` | With capture groups | `(regex-match "(\\d+)-(\\d+)" "a12-34b")` => `("12-34" "12" "34")` |
| `regex-match-all` | All matches as list | `(regex-match-all "\\d+" "a1b23c456")` => `("1" "23" "456")` |
| `regex-matches?` | Test if entire string matches | `(regex-matches? "\\d+" "123")` => `true` |

### Searching

| Primitive | Description | Example |
|-----------|-------------|---------|
| `regex-find` | Index of first match or false | `(regex-find "\\d+" "abc123")` => `3` |

### Replacing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `regex-replace` | Replace all matches | `(regex-replace "\\d+" "a1b2" "X")` => `"aXbX"` |
| `regex-replace` | With function | `(regex-replace "\\d+" "a1b2" (lambda (m) ...))` |
| `regex-replace-first` | Replace first match only | `(regex-replace-first "\\d+" "a1b2" "X")` => `"aXb2"` |

### Splitting

| Primitive | Description | Example |
|-----------|-------------|---------|
| `regex-split` | Split by pattern | `(regex-split "\\s+" "a b  c")` => `("a" "b" "c")` |

### Examples

```lisp
;; Extract all numbers from text
(regex-match-all "\\d+" "Order #123, Item #456")  ; => ("123" "456")

;; Validate email format
(regex-matches? "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" "test@example.com")
; => true

;; Replace with function - double all numbers
(regex-replace "\\d+" "a1b23c456" 
  (lambda (m) (number->string (* 2 (string->number m)))))
; => "a2b46c912"

;; Split CSV line
(regex-split "," "a,b,c")  ; => ("a" "b" "c")

;; Find position of pattern
(regex-find "world" "hello world")  ; => 6

;; Match with capture groups
(regex-match "(\\w+)@(\\w+)" "user@host")
; => ("user@host" "user" "host")
```

**Note:** In JLLL strings, backslashes must be escaped: `"\\d+"` for the regex `\d+`.

## IO Library

Input/output operations.

### Output

| Primitive | Description | Example |
|-----------|-------------|---------|
| `print` | Print without newline (streams lazy sequences) | `(print "hello")` |
| `println` | Print with newline (streams lazy sequences) | `(println "hello")` |
| `newline` | Print newline | `(newline)` |

**Lazy Sequence Streaming:**

When `print` or `println` receive a lazy sequence, they stream each element as it becomes available:

```lisp
(print (lazy-range 0 5))   ; prints: 01234
(println (ai-prompt "Hi")) ; streams AI response chunk by chunk
```

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

## JSON Library

Parse and generate JSON for data interchange with web APIs and configuration files.

### Type Mappings

| JSON Type | JLLL Type | Java Type |
|-----------|-----------|-----------|
| object | hash-map | `LinkedHashMap` |
| array | list | `Cons` |
| string | string | `String` |
| number (int) | integer | `Long` |
| number (float) | float | `Double` |
| boolean | boolean | `Boolean` |
| null | null | `Null` |

### Parsing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `json-parse` | Parse JSON string to JLLL data | `(json-parse "{\"name\": \"Alice\"}")` |
| `json-parse` | Parse with keyword keys | `(json-parse "{\"name\": \"Alice\"}" :keywords true)` |
| `json-read-file` | Parse JSON from file | `(json-read-file "config.json")` |
| `json-read-file` | Parse with keyword keys | `(json-read-file "data.json" :keywords true)` |

### Generating

| Primitive | Description | Example |
|-----------|-------------|---------|
| `json-stringify` | Convert JLLL data to JSON string | `(json-stringify data)` |
| `json-stringify` | Pretty-print with indentation | `(json-stringify data :pretty true)` |
| `json-write-file` | Write JSON to file | `(json-write-file "out.json" data)` |
| `json-write-file` | Write pretty JSON | `(json-write-file "out.json" data :pretty true)` |

### Options

- `:keywords true` - Convert JSON object keys to JLLL keywords (`:name` instead of `"name"`)
- `:pretty true` - Format output with indentation and newlines

### Examples

```lisp
;; Parse JSON string
(define data (json-parse "{\"name\": \"Alice\", \"age\": 30}"))
(hash-ref data "name")                    ; => "Alice"
(hash-ref data "age")                     ; => 30

;; Parse with keywords for cleaner access
(define data (json-parse "{\"name\": \"Bob\"}" :keywords true))
(hash-ref data :name)                     ; => "Bob"

;; Parse JSON array
(define items (json-parse "[1, 2, 3]"))   ; => (1 2 3)

;; Convert JLLL data to JSON
(json-stringify (hash-map :x 1 :y 2))     ; => "{\"x\":1,\"y\":2}"
(json-stringify '(1 2 3))                 ; => "[1,2,3]"

;; Pretty print
(json-stringify (hash-map :a 1 :b 2) :pretty true)
;; => "{\n  \"a\": 1,\n  \"b\": 2\n}"

;; File operations
(define config (json-read-file "config.json" :keywords true))
(json-write-file "output.json" data :pretty true)

;; Roundtrip: parse, modify, save
(define data (json-read-file "data.json"))
(hash-set! data "updated" true)
(json-write-file "data.json" data :pretty true)
```

## Date/Time Library

Date and time operations using timestamps (milliseconds since Unix epoch).

### Current Time

| Primitive | Description | Example |
|-----------|-------------|---------|
| `now` | Current timestamp in milliseconds | `(now)` => `1710512345678` |
| `current-time` | Current time as hash-map | `(current-time)` => `{:year 2024 :month 3 ...}` |

### Formatting and Parsing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `date-format` | Format timestamp to string | `(date-format ts "yyyy-MM-dd")` => `"2024-03-15"` |
| `date-parse` | Parse string to timestamp | `(date-parse "2024-03-15" "yyyy-MM-dd")` |

Common format patterns:
- `"yyyy-MM-dd"` - Date only (2024-03-15)
- `"HH:mm:ss"` - Time only (14:30:45)
- `"yyyy-MM-dd'T'HH:mm:ss"` - ISO 8601 datetime

### Date Arithmetic

| Primitive | Description | Example |
|-----------|-------------|---------|
| `date-add` | Add time units | `(date-add ts :days 1)` |
| `date-add` | Multiple units | `(date-add ts :hours 2 :minutes 30)` |
| `date-diff` | Difference in units | `(date-diff t1 t2 :days)` => `10` |

Supported units: `:years`, `:months`, `:weeks`, `:days`, `:hours`, `:minutes`, `:seconds`, `:millis`

### Component Extraction

| Primitive | Description | Example |
|-----------|-------------|---------|
| `date-year` | Extract year | `(date-year ts)` => `2024` |
| `date-month` | Extract month (1-12) | `(date-month ts)` => `3` |
| `date-day` | Extract day of month | `(date-day ts)` => `15` |
| `date-hour` | Extract hour (0-23) | `(date-hour ts)` => `14` |
| `date-minute` | Extract minute | `(date-minute ts)` => `30` |
| `date-second` | Extract second | `(date-second ts)` => `45` |
| `date-day-of-week` | Day of week (ISO: 1=Mon, 7=Sun) | `(date-day-of-week ts)` => `5` |

### Conversion

| Primitive | Description | Example |
|-----------|-------------|---------|
| `date->list` | Decompose to list | `(date->list ts)` => `(2024 3 15 14 30 45 123 5)` |
| `make-date` | Create from components | `(make-date 2024 3 15)` |
| `make-date` | With time | `(make-date 2024 3 15 14 30 45)` |

### Comparison

| Primitive | Description | Example |
|-----------|-------------|---------|
| `date<?` | Before | `(date<? t1 t2)` => `true` |
| `date>?` | After | `(date>? t1 t2)` => `false` |
| `date=?` | Equal | `(date=? t1 t2)` => `false` |

### Examples

```lisp
;; Get current time
(define now-ts (now))
(date-format now-ts "yyyy-MM-dd HH:mm:ss")  ; => "2024-03-15 14:30:45"

;; Current time as structured data
(define ct (current-time))
(hash-ref ct :year)   ; => 2024
(hash-ref ct :month)  ; => 3

;; Parse a date string
(define birthday (date-parse "1990-06-15" "yyyy-MM-dd"))

;; Calculate age in years
(date-diff birthday (now) :years)  ; => 33

;; Add 30 days to today
(define future (date-add (now) :days 30))
(date-format future "yyyy-MM-dd")

;; Check if a date is in the past
(date<? birthday (now))  ; => true

;; Create a specific date/time
(define meeting (make-date 2024 12 25 10 0 0))
(date-format meeting "yyyy-MM-dd HH:mm")  ; => "2024-12-25 10:00"

;; Get day of week (1=Monday, 7=Sunday)
(date-day-of-week (make-date 2024 3 15))  ; => 5 (Friday)

;; Decompose to list: (year month day hour minute second millis day-of-week)
(date->list (now))  ; => (2024 3 15 14 30 45 123 5)
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

## AI Library

LLM integration using LangChain4j. Provides session-based AI interactions with conversation memory, streaming responses, and tool calling.

**Configuration:** API keys are read from environment variables:
- `ANTHROPIC_API_KEY` - Anthropic (Claude) - **default provider**
- `OPENAI_API_KEY` - OpenAI (GPT-4, GPT-4o, etc.)
- `GOOGLE_AI_API_KEY` - Google AI (Gemini)
- `OLLAMA_BASE_URL` - Ollama (local models)

### Model Tiers

Models are organized into three capability tiers (best → fast):

| Tier | Description | Anthropic | OpenAI | Google | Ollama |
|------|-------------|-----------|--------|--------|--------|
| `best` | Most capable, highest cost | claude-opus-4-5 | o1 | gemini-1.5-pro | llama3.2 |
| `balanced` | Good balance (default) | claude-sonnet-4-5 | gpt-4o | gemini-1.5-pro | llama3.2 |
| `fast` | Fastest, lowest cost | claude-haiku-4-5 | gpt-4o-mini | gemini-1.5-flash | llama3.2 |

**Default:** `balanced` tier is used when no model is specified.

**Model Configuration:**
- Configuration is stored in `models.json` (bundled resource)
- Can be overridden by user config files (see below)
- Provider priority: Anthropic → OpenAI → Google → Ollama

### Custom Model Configuration

You can override the default model configuration by creating a `models.json` file:

**Override Priority (highest → lowest):**
1. `JLLL_AI_MODELS_PATH` environment variable (explicit path)
2. `./models.json` (local project directory)
3. `~/.jlll/models.json` (user home directory)
4. Bundled resource (default)

**Example `~/.jlll/models.json`:**
```json
{
  "defaultTier": "fast",
  "providers": {
    "anthropic": {
      "tiers": {
        "balanced": "claude-sonnet-4-0"
      }
    }
  }
}
```

Configs are merged, so you only need to specify what you want to change.

### Session Management

| Primitive | Description | Example |
|-----------|-------------|---------|
| `ai-session-create` | Create a new session | `(ai-session-create :name "coder")` |
| `ai-session-activate` | Set session as current | `(ai-session-activate sess)` |
| `ai-session-deactivate` | Clear current session | `(ai-session-deactivate)` |
| `ai-session-current` | Get current session | `(ai-session-current)` |
| `ai-sessions` | List all sessions | `(ai-sessions)` |
| `ai-session-name` | Get session name | `(ai-session-name sess)` => `"coder"` |
| `ai-session-id` | Get session ID | `(ai-session-id sess)` => `"sess-12345"` |
| `ai-session?` | Test if value is session | `(ai-session? x)` => `true` |

**Session Options:**
- `:name "name"` - Session name (auto-generated if omitted)
- `:system "prompt"` - Custom system message (default: loaded from `docs/system-prompt.md`)
- `:tier "best"` - Model tier ("best", "balanced", "fast")
- `:model "gpt-4o"` - Explicit model override (takes precedence over tier)
- `:tools (list tool1 tool2)` - Additional tools
- `:eval true/false` - Enable/disable eval tool (default: true)

**Custom System Prompt:**

```lisp
;; Use default system prompt (loaded from docs/system-prompt.md)
(ai-session-create)

;; Custom system prompt for specific use case
(ai-session-create :system "You are a helpful math tutor. Use (eval ...) to verify calculations.")

;; Combine with other options
(ai-session-create :name "math-tutor" 
                   :system "Focus on explaining step by step."
                   :model "gpt-4o")
```

### Core AI Operations

| Primitive | Description | Example |
|-----------|-------------|---------|
| `ai` | Chat with LLM, prints streaming response, returns full text | `(ai "Explain closures")` |
| `ai-prompt` | Chat with LLM, returns lazy sequence (for programmatic use) | `(ai-prompt "Hello")` |
| `ai-history` | Get conversation history | `(ai-history)` => list of hash-maps |
| `ai-clear` | Clear conversation history | `(ai-clear)` |

**Console-friendly `ai`:**

The `ai` function prints the response to the console as it streams, then returns the full response as a string:

```lisp
(ai "Tell me a joke")
;; Output appears chunk by chunk as it streams...
;; => "Why did the programmer quit? Because he didn't get arrays!"
```

**Programmatic `ai-prompt`:**

For programmatic use, `ai-prompt` returns a lazy sequence of text chunks:

```lisp
(define response (ai-prompt "Hello"))
(realize response)  ; => ("Hello" "!" " How" " can" " I" " help" "?")
(string-join (realize response) "")  ; => "Hello! How can I help?"

;; Stream with custom processing
(for-each (lambda (chunk) (print "[" chunk "]")) (ai-prompt "Hi"))
```

### Tool Management

| Primitive | Description | Example |
|-----------|-------------|---------|
| `ai-tool` | Create a custom tool | See below |
| `ai-tool-add` | Add tool to session | `(ai-tool-add my-tool)` |
| `ai-tool-remove` | Remove tool by name | `(ai-tool-remove "my-tool")` |
| `ai-tools` | List tools in session | `(ai-tools)` |
| `ai-tool?` | Test if value is tool | `(ai-tool? x)` => `true` |

**Creating Tools:**

```lisp
(define weather-tool
  (ai-tool "get-weather"
    :description "Get current weather for a city"
    :parameters '((city "string" "City name"))
    :fn (lambda (city) (concat "Weather in " city ": Sunny, 72F"))))
```

**Built-in Eval Tool:**

By default, sessions include an `eval` tool that allows the LLM to execute JLLL code:

```lisp
;; Session with eval tool (default)
(ai-session-create)

;; Session without eval tool
(ai-session-create :eval false)
```

### Configuration

| Primitive | Description | Example |
|-----------|-------------|---------|
| `ai-configure` | Set configuration options | `(ai-configure :default-tier "fast")` |
| `ai-config` | Get current configuration | `(ai-config)` => hash-map |
| `ai-models` | Get all model configurations | `(ai-models)` => nested hash-map |

**Configuration Options:**
- `:openai-api-key` - Set OpenAI API key
- `:anthropic-api-key` - Set Anthropic API key
- `:google-ai-api-key` - Set Google AI API key
- `:ollama-base-url` - Set Ollama base URL
- `:default-tier` - Set default model tier ("best", "balanced", "fast")
- `:default-model` - Override default model (explicit model name)

### Examples

```lisp
;; Simple conversation (uses default "balanced" tier)
(ai-session-create :name "assistant")
(ai-session-activate (car (ai-sessions)))
(println (string-join (realize (ai "What is 2+2?")) ""))

;; Use "best" tier for complex reasoning tasks
(ai-session-create :name "expert" :tier "best")

;; Use "fast" tier for quick, simple tasks
(ai-session-create :name "quick" :tier "fast")

;; Change default tier globally
(ai-configure :default-tier "fast")

;; View all available models
(ai-models)
;; => {:anthropic {:best "claude-opus-4-5" :balanced "claude-sonnet-4-5" :fast "claude-haiku-4-5"}
;;     :openai {:best "o1" :balanced "gpt-4o" :fast "gpt-4o-mini"} ...}

;; With system prompt
(define coder (ai-session-create 
  :name "coder"
  :system "You are a helpful coding assistant. Be concise."))
(ai-session-activate coder)
(println (string-join (realize (ai "Write a factorial function")) ""))

;; Custom tool
(define calc-tool
  (ai-tool "calculate"
    :description "Evaluate a math expression"
    :parameters '((expr "string" "Math expression like '2+2'"))
    :fn (lambda (expr) (to-string (eval (read-from-string expr))))))
(ai-tool-add calc-tool)
(ai "What is 123 * 456?")  ; LLM can use the calculate tool

;; Check history
(ai-history)  ; => list of messages with :role and :content

;; Clear and start fresh
(ai-clear)
```

### Best Practices

**Prefer JLLL built-ins over system commands:**
- Use `slurp`/`spit` for file I/O instead of shell redirection
- Use `open-browser` instead of system `open` command
- Build content as JLLL strings, not shell command output

**Code transparency:**
- Show the JLLL code being executed when practical
- Let users see patterns for reproducibility and learning

## Debugging and Development Tools

Utilities for debugging, testing, and functional programming.

### Functional Utilities

| Primitive | Description | Example |
|-----------|-------------|---------|
| `identity` | Return argument unchanged | `(identity x)` => `x` |
| `constantly` | Function that always returns given value | `((constantly 42) 'ignored)` => `42` |
| `complement` | Negate a predicate | `((complement null?) '(1 2))` => `true` |

**Examples:**

```lisp
;; Filter falsy values (in JLLL, only nil/() is falsy)
(filter identity '(1 null 2 () 3))  ; => (1 2 3)

;; Fill list with constant value
(map (constantly 0) '(a b c))  ; => (0 0 0)

;; Inverse predicate
(filter (complement null?) '(1 null 2))  ; => (1 2)
```

### Type Inspection

| Primitive | Description | Example |
|-----------|-------------|---------|
| `type-of` | Get type name as string | `(type-of 42)` => `"Number"` |
| `describe` | Human-readable description | `(describe 'my-func)` |
| `inspect` | Detailed object info | `(inspect obj)` |

**Type names returned by `type-of`:**
- `"Number"` - integers and floats
- `"String"` - strings
- `"Boolean"` - true/false
- `"Symbol"` - symbols
- `"Keyword"` - keywords like `:foo`
- `"List"` - lists (cons cells)
- `"Nil"` - null/empty list
- `"Procedure"` - user-defined functions
- `"Primitive"` - built-in functions
- `"Macro"` - macros
- `"Java: ClassName"` - Java objects

**`inspect` output includes:**
- For JLLL types: type name, value representation, structure details
- For Java objects: class name, accessible fields and their values, available methods
- Returns the inspected value unchanged (can be used in pipelines like `tap`)

### Assertions

| Primitive | Description | Example |
|-----------|-------------|---------|
| `assert` | Throw if test is false | `(assert (> x 0))` |
| `assert` | With message | `(assert (> x 0) "x must be positive")` |
| `assert` | With concatenated message | `(assert (> x 0) "x must be > 0, got: " x)` |

Returns `true` if assertion passes. Message parts are lazily evaluated (only computed if assertion fails).

### Debug Output

| Primitive | Description | Example |
|-----------|-------------|---------|
| `tap` | Print value and return it (for pipelines) | `(tap x)` or `(tap x "label")` |

**Examples:**

```lisp
;; Debug a value in a pipeline
(map square (tap (filter positive? data)))  
;; prints the filtered list, then maps square over it

;; With label
(tap (compute-result) "result")  ; prints: result: <value>
```

### Deep Tracing

| Primitive | Description | Example |
|-----------|-------------|---------|
| `trace` | Enable deep tracing (all procedure calls) | `(trace)` |
| `untrace` | Disable deep tracing | `(untrace)` |
| `traced?` | Check if tracing is enabled | `(traced?)` => `true` |

Deep tracing shows **all** procedure calls when enabled, with indentation reflecting call depth.

**Trace output format:**
```
TRACE: (func-name arg1 arg2 ...)
  TRACE: (nested-call ...)        ; indented for depth
  TRACE: nested-call => result
TRACE: func-name => result
```

**Examples:**

```lisp
;; Enable tracing and call a function
(define (factorial n) 
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(trace)           ; Enable deep tracing
(factorial 3)
;; Output shows ALL calls including internal ones:
;; TRACE: (factorial 3)
;;   TRACE: (<= n 1)
;;   TRACE: <= => false
;;   TRACE: (* n (factorial (- n 1)))
;;     TRACE: (- n 1)
;;     TRACE: - => 2
;;     TRACE: (factorial (- n 1))
;;       ...
;;     TRACE: factorial => 2
;;   TRACE: * => 6
;; TRACE: factorial => 6

(untrace)         ; Disable tracing
```

**Note:** Trace output goes to the console, so AI can see trace results when debugging.

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
