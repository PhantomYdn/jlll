# JLLL Java Interop

JLLL provides direct access to Java classes, objects, and methods through the Reflect library.

## JLLL-Specific Notes

- Java interop uses explicit function calls, not special syntax like Clojure's `.method`
- Class names can be strings or Class objects
- Method/field names are strings
- Primitive types are automatically converted

## Creating Objects

### `new`

Create a Java object:

```lisp
(new class-name args ...)
```

Examples:

```lisp
;; No-arg constructor
(new "java.util.ArrayList")

;; With arguments
(new "java.util.ArrayList" 100)  ; initial capacity

;; Using Class object
(new (class "java.util.HashMap"))

;; Nested classes use $ notation
(new "java.util.AbstractMap$SimpleEntry" "key" "value")
```

## Calling Methods

### `invoke`

Call an instance method:

```lisp
(invoke object method-name args ...)
```

Examples:

```lisp
(define list (new "java.util.ArrayList"))

(invoke list "add" "item1")
(invoke list "add" "item2")
(invoke list "size")           ; => 2
(invoke list "get" 0)          ; => "item1"
(invoke list "contains" "item1")  ; => true

;; Method chaining
(invoke (invoke str "trim") "toLowerCase")
```

### `invoke-static`

Call a static method:

```lisp
(invoke-static class-name method-name args ...)
```

Examples:

```lisp
(invoke-static "java.lang.Math" "sqrt" 16)    ; => 4.0
(invoke-static "java.lang.Math" "max" 10 20)  ; => 20
(invoke-static "java.lang.Integer" "parseInt" "42")  ; => 42

;; System methods
(invoke-static "java.lang.System" "currentTimeMillis")
(invoke-static "java.lang.System" "getProperty" "user.home")
```

## Accessing Fields

### `peek`

Get an instance field value:

```lisp
(peek object field-name)
```

### `poke`

Set an instance field value:

```lisp
(poke object field-name value)
```

Example:

```lisp
(define point (new "java.awt.Point"))
(poke point "x" 10)
(poke point "y" 20)
(peek point "x")               ; => 10
```

### `peek-static`

Get a static field value:

```lisp
(peek-static class-name field-name)
```

### `poke-static`

Set a static field value:

```lisp
(poke-static class-name field-name value)
```

Examples:

```lisp
(peek-static "java.lang.Integer" "MAX_VALUE")  ; => 2147483647
(peek-static "java.lang.Math" "PI")            ; => 3.14159...
```

## Type Checking

### `instanceof?`

Check if object is instance of class:

```lisp
(instanceof? object class-name)
```

Examples:

```lisp
(instanceof? "hello" "java.lang.String")       ; => true
(instanceof? '(1 2 3) "java.util.List")        ; => false (JLLL Cons)
(instanceof? (new "java.util.ArrayList") "java.util.List")  ; => true
```

### `class`

Get Class object from name:

```lisp
(class class-name)
```

Examples:

```lisp
(class "java.lang.String")
(class "int")                  ; primitive type
(class "[Ljava.lang.String;")  ; String array
```

## Type Conversions

JLLL automatically converts between types:

| JLLL Type | Java Type |
|-----------|-----------|
| Integer | `java.lang.Integer` |
| Double | `java.lang.Double` |
| String | `java.lang.String` |
| Boolean (`true`/`false`) | `java.lang.Boolean` |
| Cons (list) | `ru.ydn.jlll.common.Cons` |
| Symbol | `ru.ydn.jlll.common.Symbol` |
| Keyword | `ru.ydn.jlll.common.Keyword` |
| BigInteger | `java.math.BigInteger` |

### Converting Collections

```lisp
;; JLLL list to Java List
(define jlist '(1 2 3))
(define alist (new "java.util.ArrayList"))
(map (lambda (x) (invoke alist "add" x)) jlist)

;; Java Collection to JLLL list
(collection->list alist)       ; => (1 2 3)
```

## Working with Arrays

```lisp
;; Create array
(define arr (invoke-static "java.lang.reflect.Array" "newInstance" 
                           (class "java.lang.String") 3))

;; Set elements
(invoke-static "java.lang.reflect.Array" "set" arr 0 "a")
(invoke-static "java.lang.reflect.Array" "set" arr 1 "b")
(invoke-static "java.lang.reflect.Array" "set" arr 2 "c")

;; Get element
(invoke-static "java.lang.reflect.Array" "get" arr 1)  ; => "b"

;; Array length
(invoke-static "java.lang.reflect.Array" "getLength" arr)  ; => 3
```

## Exception Handling

Java exceptions are wrapped in `JlllException` and can be caught using JLLL's exception handling:

```lisp
;; Unhandled exception
(invoke-static "java.lang.Integer" "parseInt" "not-a-number")
; => JlllException: java.lang.NumberFormatException: For input string: "not-a-number"

;; Caught with try/catch
(try
  (invoke-static "java.lang.Integer" "parseInt" "not-a-number")
  (catch "java.lang.NumberFormatException" e
    (println "Invalid number: " (exception-message e))
    0))  ; => 0
```

See [Special Forms](special-forms.md) for full documentation on `try`/`catch`/`finally` and `guard`.

## Practical Examples

### Reading a File

```lisp
(define (read-file path)
  (define reader (new "java.io.BufferedReader"
                      (new "java.io.FileReader" path)))
  (define lines (new "java.util.ArrayList"))
  (letrec ((read-loop (lambda ()
                        (define line (invoke reader "readLine"))
                        (if (null? line)
                            (invoke reader "close")
                            (begin
                              (invoke lines "add" line)
                              (read-loop))))))
    (read-loop))
  (collection->list lines))
```

### HTTP Request

```lisp
(define (http-get url-string)
  (define url (new "java.net.URL" url-string))
  (define conn (invoke url "openConnection"))
  (define reader (new "java.io.BufferedReader"
                      (new "java.io.InputStreamReader"
                           (invoke conn "getInputStream"))))
  (define sb (new "java.lang.StringBuilder"))
  (letrec ((read-loop (lambda ()
                        (define line (invoke reader "readLine"))
                        (if (null? line)
                            (begin
                              (invoke reader "close")
                              (invoke sb "toString"))
                            (begin
                              (invoke sb "append" line)
                              (invoke sb "append" "\n")
                              (read-loop))))))
    (read-loop)))
```

## Functional Interface Support (SAM Conversion)

JLLL automatically converts procedures/lambdas to Java functional interfaces when needed.
A **functional interface** (also called SAM - Single Abstract Method) is any Java interface
with exactly one abstract method, such as `Runnable`, `Comparator`, `ActionListener`, `Consumer`, etc.

### Automatic Conversion

When you pass a JLLL lambda to a Java method or constructor that expects a functional interface,
JLLL automatically wraps the lambda in an implementation of that interface:

```lisp
;; Thread constructor takes Runnable - lambda auto-converted
(define t (new 'java.lang.Thread 
  (lambda () (println "Hello from thread!"))))
(invoke t "start")
(invoke t "join")

;; Collections.sort takes Comparator - lambda auto-converted
(define nums (new 'java.util.ArrayList))
(invoke nums "add" 3)
(invoke nums "add" 1)
(invoke nums "add" 2)
(invoke-static 'java.util.Collections "sort" nums
  (lambda (a b) (- a b)))  ; ascending order

;; forEach takes Consumer - lambda auto-converted
(invoke nums "forEach"
  (lambda (x) (println "Value: " x)))
```

### Supported Interfaces

Any interface with exactly one abstract method is supported, including:

| Interface | Method | Common Use |
|-----------|--------|------------|
| `Runnable` | `run()` | Thread execution |
| `Callable<V>` | `call()` | Async computations with return value |
| `Comparator<T>` | `compare(a, b)` | Sorting, ordering |
| `Consumer<T>` | `accept(t)` | forEach operations |
| `Supplier<T>` | `get()` | Lazy value providers |
| `Function<T,R>` | `apply(t)` | Transformations |
| `Predicate<T>` | `test(t)` | Filtering, testing |
| `ActionListener` | `actionPerformed(e)` | Swing event handling |

### Event Handling Example

```lisp
;; Swing button with click handler
(define frame (new 'javax.swing.JFrame "My App"))
(define button (new 'javax.swing.JButton "Click Me"))

;; Lambda auto-converted to ActionListener
(invoke button "addActionListener"
  (lambda (event)
    (println "Button clicked!")
    (println "Command: " (invoke event "getActionCommand"))))

(invoke frame "add" button)
(invoke frame "setSize" 300 200)
(invoke frame "setVisible" true)
```

### How It Works

1. When `invoke` or `new` encounters a JLLL procedure where Java expects a functional interface,
   JLLL uses ByteBuddy to generate a wrapper class at runtime
2. The wrapper implements the interface and delegates to the JLLL procedure
3. Generated classes are cached by interface type for performance

### Limitations

- Only single-method interfaces are supported automatically
- Multi-method interfaces (like `MouseListener` with multiple methods) require explicit implementation in Java
- The lambda receives all Java method arguments as JLLL values

### Using Java 8 Streams

```lisp
(define list (new 'java.util.ArrayList))
(invoke list "add" 1)
(invoke list "add" 2)
(invoke list "add" 3)

;; forEach with lambda Consumer
(invoke list "forEach" (lambda (x) (println (* x x))))
;; prints: 1, 4, 9

;; Note: Stream intermediate operations (map, filter) require ToIntFunction etc.
;; which are also auto-converted:
(define stream (invoke list "stream"))
(define sum (invoke 
              (invoke stream "mapToInt" (lambda (x) x))
              "sum"))
;; => 6
```

## Embedding JLLL in Java

```java
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.JlllException;

public class Example {
    public static void main(String[] args) throws JlllException {
        // Create environment
        Enviroment env = Jlll.prepare();
        
        // Evaluate expressions
        Object result = Jlll.eval("(+ 1 2 3)", env);
        System.out.println(result);  // 6
        
        // Define functions
        Jlll.eval("(define (factorial n) " +
                  "  (if (<= n 1) 1 (* n (factorial (- n 1)))))", env);
        
        // Call JLLL function from Java
        Object fact10 = Jlll.invokeProcedure("factorial", env, 10);
        System.out.println(fact10);  // 3628800
        
        // Add Java objects to environment
        env.addBinding(Symbol.intern("my-list"), new ArrayList<>());
        Jlll.eval("(invoke my-list \"add\" \"from-jlll\")", env);
    }
}
```

## Extending JLLL with Java Libraries

Create a class implementing `Library`:

```java
package com.example;

import ru.ydn.jlll.common.*;
import ru.ydn.jlll.common.annotation.JlllName;

public class MyLib extends ReflectionLibrary {
    
    @JlllName("greet")
    public String greet(String name) {
        return "Hello, " + name + "!";
    }
    
    @JlllName("add-numbers")
    public int addNumbers(int a, int b) {
        return a + b;
    }
    
    // First param can be Environment for access to JLLL state
    @JlllName("lookup-var")
    public Object lookupVar(Enviroment env, String name) {
        return env.lookup(name);
    }
}
```

Load in JLLL:

```lisp
(load-lib "com.example.MyLib")
(greet "World")       ; => "Hello, World!"
(add-numbers 1 2)     ; => 3
```
