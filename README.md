```
     ___ _     _     _     
    |_  | |   | |   | |    
      | | |   | |   | |    
      | | |   | |   | |    
  /\__/ / |___| |___| |____
  \____/|_____|_____|______|
```

# JLLL - Java Lisp Like Language

[![Build](https://github.com/PhantomYdn/jlll/actions/workflows/ci.yml/badge.svg)](https://github.com/PhantomYdn/jlll/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/PhantomYdn/jlll/branch/master/graph/badge.svg)](https://codecov.io/gh/PhantomYdn/jlll)
[![Maven Central](https://img.shields.io/maven-central/v/ru.ydn/jlll)](https://central.sonatype.com/artifact/ru.ydn/jlll)
[![Java 17+](https://img.shields.io/badge/Java-17%2B-blue)](https://openjdk.org/)
[![License](https://img.shields.io/badge/License-Apache%202.0-green.svg)](LICENSE)

**Lightweight, embeddable Lisp interpreter for Java.** Add scripting, DSLs, or a REPL to your project in minutes.

---

## Features

- **Lightweight** - Zero runtime dependencies, small footprint
- **Full Java Interop** - Call any Java method, access fields, create objects
- **Embeddable** - Evaluate Lisp from Java with a single line of code
- **Extensible** - Add custom primitives with simple annotations
- **Interactive REPL** - Experiment and prototype interactively
- **Built-in Libraries** - Math, List, String, SQL, and more

---

## Quick Start

### Java API

```java
import ru.ydn.jlll.common.Jlll;

// Evaluate Lisp expressions
Object result = Jlll.eval("(+ 2 2)");
System.out.println(result); // 4

// Define and call functions
Jlll.eval("(define (square x) (* x x))");
Object squared = Jlll.eval("(square 5)");
System.out.println(squared); // 25
```

### Interactive REPL

Build and run the interactive interpreter:

```bash
mvn package
java -jar target/jlll-*.jar
```

Example session:

```
> (define (factorial n)
    (if (<= n 1) 
        1 
        (* n (factorial (- n 1)))))
> (factorial 10)
3628800
> (invoke "hello world" 'toUpperCase)
HELLO WORLD
```

---

## Installation

### Maven

```xml
<dependency>
    <groupId>ru.ydn</groupId>
    <artifactId>jlll</artifactId>
    <version>2.0.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'ru.ydn:jlll:2.0.0'
```

---

## Examples

### Basic Operations

```lisp
(+ 2 2)                        ; => 4
(cons 'a 'b)                   ; => (a . b)
(append '(a b) '(c d))         ; => (a b c d)
```

### Functions

```lisp
(define (inc x) (+ x 1))
(inc 2)                        ; => 3

(lambda (x y) (+ x y))         ; anonymous function
((lambda (x) (* x x)) 4)       ; => 16
```

### Java Interop

```lisp
; Call static methods
(invoke-static 'java.lang.Math 'cos 0.0)    ; => 1.0
(invoke-static 'java.lang.Math 'max 10 20)  ; => 20

; Call instance methods
(invoke "hello" 'length)                     ; => 5
(invoke "hello" 'substring 1 3)              ; => "el"

; Create objects
(new 'java.util.ArrayList)
```

---

## License

[Apache License 2.0](LICENSE)
