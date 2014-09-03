[![Build Status](https://travis-ci.org/PhantomYdn/jlll.svg?branch=master)](https://travis-ci.org/PhantomYdn/jlll)

jlll
====

JLLL - Java Lisp Like Language. Lightweight and embeddable java library to bring some lisp flavour into your projects.


Installation
------------
Add following snippet into pom.xml
```xml

<dependency>
	<groupId>ru.ydn</groupId>
	<artifactId>jlll</artifactId>
	<version>1.0</version>
</dependency>
```

Examples
--------
```lisp
(+ 2 2)
4

(cons 'a 'b)
(a . b)

(append '(a b) '(c d))
(a b c d)

(define (inc x) (+ x 1))
(inc 2)
3

(invoke-static 'java.lang.Math 'cos 0.0)
1.0

(invoke "hello" 'length)
5
```
