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
| `load-url` | Load JLLL from URL | `(load-url "http://example.com/lib.jlll")` |
| `load-system-script` | Load internal script | `(load-system-script "math.jlll")` |
| `load-lib` | Load Java library class | `(load-lib "com.example.MyLib")` |

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
| `read` | Read JLLL expression | `(read)` |
| `read-line` | Read line as string | `(read-line)` |

### Streams

| Primitive | Description | Example |
|-----------|-------------|---------|
| `stdin` | Standard input stream | `stdin` |
| `stdout` | Standard output stream | `stdout` |

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
