# JLLL Syntax

## JLLL-Specific Notes

- **Keywords** (`:foo`) are a distinct type from symbols, self-evaluating
- **Reader macro `!`** evaluates expressions immediately at read/parse time
- **Symbols** can contain special characters via escape sequences (`\s` for space, `\n` for newline)
- **Comments** use semicolon (`;`), not `#|...|#` or `#;`

## Atoms

### Numbers

```lisp
42          ; integer
-17         ; negative integer
3.14        ; floating point
-2.5        ; negative float
1000000000000000000000  ; BigInteger (arbitrary precision)
```

JLLL uses Java's `Integer`, `Double`, and `BigInteger` types.

### Strings

```lisp
"hello"           ; simple string
"hello\nworld"    ; with newline escape
"say \"hi\""      ; with escaped quote
""                ; empty string
```

Escape sequences in strings:
- `\n` - newline
- `\"` - double quote
- `\\` - backslash

### Symbols

Symbols are identifiers that can be looked up in the environment.

```lisp
foo           ; simple symbol
+             ; operator symbol
list->vector  ; symbol with arrow
null?         ; symbol with question mark
set!          ; symbol with exclamation (mutation convention)
*global*      ; earmuff convention
<<=           ; comparison-like symbol
```

Valid symbol characters: letters, digits, and `! * + - . / < = > ? $ % & ^ _ ~`

**Note on `!`**: The exclamation mark serves dual purposes:
- **In symbols**: `set!`, `swap!` - Lisp convention for mutation
- **As reader macro**: `!expr` - immediate evaluation at parse time

The tokenizer handles this by checking position: `!` at the start of a token followed by more characters triggers the reader macro; `!` elsewhere is part of the symbol.

Symbols with special characters use escape sequences:
```lisp
foo\sbar      ; symbol containing space: "foo bar"
foo\nbar      ; symbol containing newline
foo\\bar      ; symbol containing backslash: "foo\bar"
```

### Keywords

Keywords are self-evaluating identifiers, distinct from symbols. They start with a colon.

```lisp
:name         ; keyword
:foo-bar      ; keyword with hyphen
:123          ; keyword with numeric name (valid)
```

Keywords evaluate to themselves:
```lisp
:foo          ; => :foo
(eq? :foo :foo)  ; => true
```

Keywords are NOT equal to symbols:
```lisp
(eq? :foo 'foo)  ; => false
```

**Invalid keyword syntax:**
```lisp
::foo         ; ERROR: double colon not allowed
:             ; ERROR: empty keyword name not allowed
```

### Booleans

```lisp
true          ; boolean true
false         ; boolean false
#t            ; Scheme-style true
#f            ; Scheme-style false
```

Both styles are equivalent:
```lisp
(eq? true #t)   ; => true
(eq? false #f)  ; => true
```

### Null

```lisp
null          ; null value
'()           ; empty list (also represents null in list context)
```

## Lists (S-expressions)

Lists are the fundamental compound structure.

```lisp
(1 2 3)               ; list of numbers
(a b c)               ; list of symbols (evaluated as function call)
'(a b c)              ; quoted list (data, not evaluated)
(1 "two" three)       ; mixed types
()                    ; empty list
((1 2) (3 4))         ; nested lists
```

### Dotted Pairs (Cons Cells)

```lisp
'(a . b)              ; cons cell with car=a, cdr=b
'(1 2 . 3)            ; improper list: (1 . (2 . 3))
```

## Reader Macros

Reader macros transform syntax at read time, before evaluation.

### Quote (`'`)

Prevents evaluation, returns data as-is.

```lisp
'foo          ; => foo (the symbol, not its value)
'(1 2 3)      ; => (1 2 3) (the list, not a function call)

;; Equivalent to:
(quote foo)
(quote (1 2 3))
```

### Quasiquote (`` ` ``)

Template with selective evaluation via unquote.

```lisp
`(a b c)              ; => (a b c), like quote
`(a ,(+ 1 2) c)       ; => (a 3 c), evaluates (+ 1 2)
`(a ,@(list 1 2) b)   ; => (a 1 2 b), splices list
```

### Unquote (`,`)

Inside quasiquote, evaluates the following expression.

```lisp
(define x 10)
`(value is ,x)        ; => (value is 10)
```

### Unquote-Splicing (`,@`)

Inside quasiquote, evaluates and splices a list.

```lisp
(define nums '(1 2 3))
`(start ,@nums end)   ; => (start 1 2 3 end)
```

### Exclamation (`!`)

Evaluates expression immediately at read/parse time.

```lisp
!(+ 1 2)              ; => 3 (evaluated when parsed)

;; Useful in parameter defaults:
(define (foo (id !(random 1000)))  ; random called once at definition
  id)

;; Contrast with invocation-time evaluation:
(define (bar (id (random 1000)))   ; random called on each invocation
  id)
```

The `!` reader macro expands to `(exlamation ...)`:
```lisp
!expr   ; equivalent to (exlamation expr)
```

### Sharp (`#`)

Used for special literals. Currently supports booleans:

```lisp
#t                    ; true
#f                    ; false
```

## Comments

Single-line comments start with semicolon:

```lisp
; This is a comment
(+ 1 2)  ; inline comment

;; Common convention: double semicolon for section headers
;;; Triple for file-level documentation
```

There is no multi-line comment syntax in JLLL.

## Whitespace

Whitespace (spaces, tabs, newlines) separates tokens and is otherwise ignored.

```lisp
(+   1    2)          ; same as (+ 1 2)

(define (foo x)
  (+ x 1))            ; newlines for readability
```

## Expression Evaluation

1. **Self-evaluating:** Numbers, strings, booleans, keywords return themselves
2. **Symbols:** Looked up in the current environment
3. **Lists:** First element evaluated as procedure, rest as arguments
4. **Quoted:** Returned as data without evaluation

```lisp
42            ; => 42 (self-evaluating)
"hi"          ; => "hi" (self-evaluating)
:key          ; => :key (self-evaluating)
x             ; => (value of x in environment)
(+ 1 2)       ; => 3 (procedure call)
'(+ 1 2)      ; => (+ 1 2) (quoted data)
```
