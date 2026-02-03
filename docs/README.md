# JLLL Language Documentation

JLLL (Java Lisp Like Language) is a lightweight, embeddable Lisp interpreter for Java.

## What Makes JLLL Different

JLLL is **not** Scheme, Clojure, or Common Lisp. While it shares Lisp syntax fundamentals, it has unique features and semantics:

| Feature | JLLL | Scheme | Clojure | Common Lisp |
|---------|------|--------|---------|-------------|
| Keywords | `:foo` (self-evaluating, distinct from symbols) | No standard | `:foo` | `:foo` (in KEYWORD package) |
| Keyword arguments | `(func :arg value)` at call site | SRFI-89 only | Map destructuring | `&key` in lambda list |
| Optional args with defaults | `(define (f (x 10)) ...)` | No standard | `[x 10]` in vector | `&optional (x 10)` |
| Binding metadata | `(define x :doc "desc" 42)` | N/A | `(def ^{:doc "desc"} x 42)` | N/A |
| Immediate evaluation | `!expr` reader macro | N/A | N/A | N/A |
| Boolean values | `true`, `false`, `#t`, `#f` | `#t`, `#f` | `true`, `false` | `t`, `nil` |
| Null/nil | `null`, `'()` | `'()` | `nil` | `nil` |
| Java interop | Built-in (`invoke`, `invoke-static`) | N/A | Built-in (`.method`) | Via FFI |

## Documentation Index

### Core Language
- **[Syntax](syntax.md)** - Atoms, lists, reader macros, comments
- **[Special Forms](special-forms.md)** - `define`, `if`, `lambda`, `let`, `cond`, `case`, `begin`
- **[Procedures](procedures.md)** - Function definition, keyword arguments, defaults, rest arguments
- **[Metadata](metadata.md)** - Documentation and metadata on bindings
- **[Primitives](primitives.md)** - Built-in functions organized by library
- **[Macros](macros.md)** - Macro definition and expansion
- **[Lazy Sequences](lazy-sequences.md)** - Working with lazy evaluation and infinite sequences

### Java Integration
- **[Java Interop](java-interop.md)** - Calling Java from JLLL
- **[Dynamic Classpath](dynamic-classpath.md)** - Loading Maven dependencies at runtime

### System Integration
- **[Shell Execution](shell.md)** - Running shell commands from JLLL
- **[Web Server](web-server.md)** - Embedded HTTP server with Javalin
- **[Init File](init-file.md)** - Customizing JLLL startup with ~/.jlllrc

### AI Integration
- **[AI Library](ai.md)** - LLM integration with session management, tools, and streaming

### Reference
- **[Cookbook](cookbook.md)** - Practical examples and common patterns
- **[System Prompt](system-prompt.md)** - AI assistant integration guide

## Quick Start

```lisp
;; Basic arithmetic
(+ 1 2 3)           ; => 6

;; Define a function
(define (square x) (* x x))
(square 5)          ; => 25

;; Function with optional argument and default
(define (greet (name "World"))
  (concat "Hello, " name "!"))
(greet)             ; => "Hello, World!"
(greet "Alice")     ; => "Hello, Alice!"

;; Keyword arguments
(greet :name "Bob") ; => "Hello, Bob!"

;; Keywords are self-evaluating
:foo                ; => :foo

;; Immediate evaluation with !
(define (make-id (id !(random 1000)))  ; evaluated once at definition
  id)
```

## Running JLLL

```bash
# Start REPL
java -jar jlll-*-cli.jar

# Evaluate expression
java -jar jlll-*-cli.jar -e '(+ 1 2)'

# Run file
java -jar jlll-*-cli.jar script.jlll
```

## Embedding in Java

```java
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.Enviroment;

// Create environment and evaluate
Enviroment env = Jlll.prepare();
Object result = Jlll.eval("(+ 1 2)", env);  // => 3

// Call JLLL function from Java
Jlll.eval("(define (square x) (* x x))", env);
Object squared = Jlll.invokeProcedure("square", env, 5);  // => 25
```
