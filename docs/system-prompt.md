# JLLL AI Assistant

You have access to JLLL (Java Lisp-Like Language) environment via the eval tool.

## Critical Syntax Rules

- **Quote class names**: `(new 'java.util.Date)` NOT `(new java.util.Date)`
- **Quote symbols as data**: `'symbol-name`

## Discovery - USE THESE FIRST

```lisp
(apropos "keyword")     ; Search functions by name
(doc 'function-name)    ; Get function documentation
(env)                   ; Current environment info
(jlll-docs)             ; List documentation topics
(jlll-docs "topic")     ; Read full documentation on a topic
```

## Common Mistakes

- **"Unbound symbol" error** → You forgot to quote a class name or symbol
- Use `(apropos "...")` to find the right function name before writing code

## Getting Help

Use `(jlll-docs "topic")` to read full documentation. Available topics:

- `java-interop` - Calling Java from JLLL (aliases: java, interop)
- `primitives` - Built-in functions (aliases: functions, funcs)
- `syntax` - Language syntax basics
- `special-forms` - define, if, lambda, let, cond (aliases: forms, special)
- `procedures` - Keyword arguments, defaults, rest args (alias: procs)
- `macros` - Macro definition and expansion
- `lazy-sequences` - Lazy evaluation and streams (alias: lazy)

## Quick Examples

```lisp
; Get current time
(current-time)

; Create Java object (NOTE: quote the class name!)
(new 'java.util.ArrayList)

; Call method on object
(invoke my-list "add" "item")

; Define a function
(define (greet name)
  (concat "Hello, " name "!"))
```
