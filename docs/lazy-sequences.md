# Lazy Sequences

Lazy sequences allow working with potentially infinite data structures by computing elements on demand. JLLL implements lazy sequences through a thunk-based approach where the `cdr` of a cons cell can be a lazy thunk that is automatically forced when accessed.

## Basic Concepts

### Delay and Force

The `delay` special form creates a delayed computation (promise) that is not evaluated until `force` is called:

```lisp
;; Create a delayed computation
(define p (delay (begin (println "computing...") 42)))

;; Force evaluation - prints "computing..." and returns 42
(force p)  ; => 42

;; Second force returns cached value without recomputation
(force p)  ; => 42

;; Check if a delay has been realized
(realized? p)  ; => true
```

### Lazy Cons

The `lazy-cons` special form creates a cons cell where the tail is lazily evaluated:

```lisp
;; Create a lazy cons - head is evaluated immediately, tail is delayed
(define lc (lazy-cons 1 (expensive-computation)))

;; Accessing car is immediate
(car lc)  ; => 1

;; Accessing cdr forces the delayed computation
(cdr lc)  ; forces and caches the tail

;; Check if a cons has an unrealized lazy tail
(lazy-seq? lc)  ; => true (before cdr access)
```

## Lazy Generators

### lazy-range

Creates a lazy numeric sequence, optionally infinite:

```lisp
;; Infinite sequence starting from 0
(define naturals (lazy-range 0))
(take naturals 5)  ; => (0 1 2 3 4)

;; Finite range [0, 10)
(take (lazy-range 0 10) 20)  ; => (0 1 2 3 4 5 6 7 8 9)

;; Range with step
(take (lazy-range 0 100 10) 5)  ; => (0 10 20 30 40)

;; Negative step
(realize (lazy-range 10 0 -2))  ; => (10 8 6 4 2)
```

### iterate

Creates an infinite sequence by repeatedly applying a function:

```lisp
;; Powers of 2
(define powers-of-2 (iterate (lambda (x) (* x 2)) 1))
(take powers-of-2 8)  ; => (1 2 4 8 16 32 64 128)

;; Collatz sequence starting from 7
(define (collatz n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))
(take (iterate collatz 7) 10)  ; => (7 22 11 34 17 52 26 13 40 20)
```

### cycle

Infinitely repeats a sequence:

```lisp
(define abc (cycle '(a b c)))
(take abc 7)  ; => (a b c a b c a)
```

### repeat

Creates an infinite sequence of the same value:

```lisp
(take (repeat 'x) 5)  ; => (x x x x x)
```

## Lazy Transformations

### lazy-map

Lazily maps a function over a sequence:

```lisp
(define squares (lazy-map (lambda (x) (* x x)) (lazy-range 0)))
(take squares 5)  ; => (0 1 4 9 16)
```

### lazy-filter

Lazily filters elements by a predicate:

```lisp
(define evens (lazy-filter even? (lazy-range 0)))
(take evens 5)  ; => (0 2 4 6 8)
```

## Using Standard Functions with Lazy Sequences

Thanks to transparent thunk forcing in `Cons.cdr()`, most standard list functions work seamlessly with lazy sequences. There's no need for separate `lazy-take`, `lazy-drop`, etc. variants.

### take / drop

```lisp
;; Standard take/drop work transparently with lazy sequences
(take (lazy-range 0 10) 3)  ; => (0 1 2)
(drop (lazy-range 0 10) 7)  ; => (7 8 9)

;; Works with infinite sequences too
(take (lazy-range 0) 5)  ; => (0 1 2 3 4)
```

### take-while / drop-while

```lisp
(take-while (lambda (x) (< x 5)) (lazy-range 0))
; => (0 1 2 3 4)

(take (drop-while (lambda (x) (< x 5)) (lazy-range 0)) 3)
; => (5 6 7)
```

### any / every / find

These short-circuit, making them safe for infinite sequences:

```lisp
;; Find first even number > 100 in infinite sequence
(find (lambda (x) (and (even? x) (> x 100))) (lazy-range 0))
; => 102

;; Check if any element satisfies predicate
(any (lambda (x) (> x 1000)) (lazy-range 0))  ; => true

;; Check first 10 elements are positive
(every positive? (take (lazy-range 1) 10))  ; => true
```

### reduce / fold-left

Work with finite lazy sequences:

```lisp
(reduce + (lazy-range 1 11))  ; => 55 (sum 1..10)
(fold-left * 1 (lazy-range 1 6))  ; => 120 (5!)
```

### list-ref / list-tail

```lisp
(list-ref (lazy-range 0) 100)  ; => 100
(take (list-tail (lazy-range 0) 10) 5)  ; => (10 11 12 13 14)
```

## Realization

### realize

Forces an entire lazy sequence into a regular list:

```lisp
(realize (lazy-range 0 5))  ; => (0 1 2 3 4)
```

**Warning:** Never call `realize` on an infinite sequence - it will not terminate! Use `take` first:

```lisp
;; DON'T DO THIS - will not terminate:
;; (realize (lazy-range 0))

;; Do this instead:
(take (lazy-range 0) 10)  ; => (0 1 2 3 4 5 6 7 8 9)
```

## Integration with Existing Functions

Lazy sequences integrate transparently with existing list functions because they are built on top of regular cons cells:

```lisp
;; Regular take works with lazy sequences
(take (lazy-range 0) 5)  ; => (0 1 2 3 4)

;; Regular length works (but forces the entire sequence!)
(length (realize (lazy-range 0 100)))  ; => 100

;; car/cdr work as expected
(car (lazy-range 10))  ; => 10
(car (cdr (lazy-range 10)))  ; => 11
```

## Helper Functions in lazy.jlll

Additional lazy utilities are provided:

| Function | Description |
|----------|-------------|
| `lazy-naturals` | Returns `(lazy-range 0)` |
| `lazy-integers` | Integers: 0, 1, -1, 2, -2, ... |
| `lazy-concat` | Lazily concatenates two sequences |
| `lazy-append` | Variadic lazy append |
| `lazy-flatten` | Lazily flattens nested sequences |
| `lazy-zip` | Lazily zips two sequences |
| `lazy-interleave` | Lazily interleaves elements |
| `lazy-indexed` | Pairs elements with indices |
| `lazy-partition` | Split by predicate into two lazy seqs |
| `lazy-partition-by` | Group consecutive elements by key function |
| `lazy-split-at` | Split at index into (taken . rest) |
| `list->lazy` | Convert eager list to lazy |
| `lazy->list` | Alias for `realize` |

**Note:** Standard functions `any`, `every`, `find`, `reduce`, `fold-left`, `list-ref`, `take`, `drop`, `take-while`, `drop-while` all work transparently with lazy sequences - no separate `lazy-` variants are needed.

## Type Predicates

| Predicate | Description |
|-----------|-------------|
| `delay?` | True if value is a JlllDelay |
| `thunk?` | True if value is a LazyThunk |
| `lazy-seq?` | True if cons has unrealized lazy cdr |
| `has-lazy?` | True if cons cdr is/was a LazyThunk |
| `realized?` | True if delay/thunk/cons cdr has been forced |

## Implementation Notes

JLLL's lazy sequences are implemented by allowing the `cdr` of a `Cons` cell to be a `LazyThunk`. When `cdr()` is called on a cons with a lazy tail:

1. The thunk is forced (evaluated)
2. The result replaces the thunk in the cons cell
3. The result is returned

This means:
- The head of a lazy sequence is always eager
- Only the tail is lazy
- Forcing is memoized - each element computed once
- Thread-safe double-checked locking for concurrent access

### Printing Lazy Sequences

Lazy sequences display unrealized tails as `...`:

```lisp
(lazy-range 0)  ; => (0 ...)
```

After accessing elements, they become visible:

```lisp
(define s (lazy-range 0))
(car (cdr s))  ; force first few elements
s  ; => (0 1 ...)
```

## Performance Considerations

1. **Memory**: Lazy sequences can represent infinite data with finite memory, but realized portions are retained.

2. **Computation**: Each element is computed at most once and cached.

3. **Stack depth**: Deep lazy chains may cause stack overflow. For very long sequences, consider iterative approaches.

4. **Eager operations**: Functions like `length`, `reverse`, and `last` must traverse the entire sequence - avoid on infinite sequences!
