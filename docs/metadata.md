# JLLL Metadata

JLLL supports attaching metadata to bindings using keywords. This enables documentation, versioning, and other annotations on variables and functions.

## Basic Syntax

Metadata is specified as `:keyword value` pairs in `define`:

```lisp
;; Variable with documentation
(define x :doc "The answer to everything" 42)

;; Function with documentation
(define (add x y)
  :doc "Adds two numbers together"
  (+ x y))

;; Multiple metadata entries
(define config
  :doc "Application configuration"
  :version "1.0"
  :author "Jane Doe"
  (make-config))
```

## Keyword Position Flexibility

Keywords can appear anywhere in the `define` form - they are extracted regardless of position:

```lisp
;; All equivalent:
(define x :doc "test" 42)
(define :doc "test" x 42)
(define x 42 :doc "test")   ; Keywords after value work too
```

This follows the same pattern as keyword arguments in function calls.

## Retrieving Metadata

### doc

Returns the `:doc` metadata for a symbol:

```lisp
(define x :doc "my value" 42)
(doc 'x)          ; => "my value"

(define (add x y) :doc "Adds numbers" (+ x y))
(doc 'add)        ; => "Adds numbers"
```

For built-in primitives without metadata, `doc` falls back to any `@JlllDoc` annotation.

### meta

Retrieves metadata by key:

```lisp
(define x :doc "desc" :version "1.0" 42)

(meta 'x :doc)        ; => "desc"
(meta 'x :version)    ; => "1.0"
(meta 'x :missing)    ; => () (null)
```

With one argument, returns all metadata as an association list:

```lisp
(meta 'x)             ; => ((:doc . "desc") (:version . "1.0"))
```

## Modifying Metadata

### set-meta!

Adds or updates metadata on an existing binding:

```lisp
(define x 42)
(set-meta! 'x :doc "added later")
(doc 'x)              ; => "added later"

(set-meta! 'x :doc "updated")
(doc 'x)              ; => "updated"
```

## Copying Bindings with Metadata

### define-from

Creates a new binding with the same value and metadata as an existing one:

```lisp
(define x :doc "original" :version "1.0" 42)
(define-from y 'x)

y                     ; => 42
(doc 'y)              ; => "original"
(meta 'y :version)    ; => "1.0"
```

After copying, the bindings are independent:

```lisp
(set-meta! 'x :doc "changed")
(doc 'x)              ; => "changed"
(doc 'y)              ; => "original" (unchanged)
```

## Metadata and set!

The `set!` function preserves metadata when updating a value:

```lisp
(define x :doc "persistent" 42)
(set! x 100)
x                     ; => 100
(doc 'x)              ; => "persistent" (still there!)
```

## Integration with describe

The `describe` function includes metadata in its output:

```lisp
(define x :doc "test" :author "me" 42)
(describe 'x)
; => Symbol 'x'
;    Value: 42
;    Metadata:
;      :doc test
;      :author me
```

## Use Cases

### Self-Documenting Code

```lisp
(define (http-get url)
  :doc "Performs an HTTP GET request"
  :param-url "The URL to fetch"
  :returns "Response body as string"
  :since "2.1"
  (fetch-url url "GET"))
```

### Configuration with Metadata

```lisp
(define db-host
  :doc "Database hostname"
  :env "DB_HOST"
  :default "localhost"
  (get-env "DB_HOST" "localhost"))
```

### Deprecation Markers

```lisp
(define (old-function x)
  :doc "Use new-function instead"
  :deprecated "2.0"
  :see-also "new-function"
  (new-function x))
```

## Summary

| Primitive | Usage | Description |
|-----------|-------|-------------|
| `doc` | `(doc 'sym)` | Get `:doc` metadata |
| `meta` | `(meta 'sym :key)` | Get specific metadata |
| `meta` | `(meta 'sym)` | Get all metadata as alist |
| `set-meta!` | `(set-meta! 'sym :key val)` | Set metadata |
| `define-from` | `(define-from new 'src)` | Copy binding with metadata |
