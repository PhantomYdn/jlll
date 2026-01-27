# Dynamic Classpath and Dependencies

JLLL supports loading external Maven dependencies at runtime with environment-scoped classloaders.
This allows you to use external Java libraries without recompiling your application.

## Core Concepts

- **Environment-scoped**: Dependencies are tied to child environments, not global
- **Classloader isolation**: Each environment with dependencies gets its own classloader
- **Full inheritance**: Child environments inherit all parent bindings plus new classpath
- **Maven resolution**: Full transitive dependency resolution using Maven coordinates

## Basic Usage

### Execute with Dependencies

Use `(env :depends ...)` to execute code in a child environment with additional Maven dependencies:

```lisp
;; Load Gson and use it
(env :depends '("com.google.code.gson:gson:2.10.1")
  (define Gson (class "com.google.gson.Gson"))
  (define gson (new Gson))
  (invoke gson "toJson" '(1 2 3)))
;; => "[1,2,3]"
```

The child environment is created, dependencies are resolved, code is executed, and the result is returned.
The child environment is discarded after execution.

### Create Reusable Environment

Without a body, `(env :depends ...)` returns an environment object for later use:

```lisp
;; Create environment with Gson dependency
(define json-env (env :depends '("com.google.code.gson:gson:2.10.1")))

;; Execute code in that environment using eval :env
(eval :env json-env '(define Gson (class "com.google.gson.Gson")))
(eval :env json-env '(new Gson))
```

### Custom Repositories

Add custom Maven repositories with `:repos`:

```lisp
(env :depends '("com.example:internal-lib:1.0.0")
     :repos '("https://maven.example.com/releases")
  (load-lib "com.example.MyLibrary"))
```

## Modules with Dependencies

Modules can have their own classpath using `:depends`:

```lisp
(module json-utils
  :depends '("com.google.code.gson:gson:2.10.1")
  
  (export parse stringify)
  
  (define Gson (class "com.google.gson.Gson"))
  (define gson (new Gson))
  
  (define (parse json-string)
    :doc "Parse JSON string"
    (invoke gson "fromJson" json-string (class "java.util.Map")))
  
  (define (stringify obj)
    :doc "Convert to JSON string"
    (invoke gson "toJson" obj)))
```

**Note**: Modules with `:depends` cannot be redefined (they are immutable once created).

## Environment Management

### env-switch!

Switch the current REPL/evaluation environment:

```lisp
;; Create child environment
(define dev-env (env :depends '("com.google.code.gson:gson:2.10.1")))

;; Switch to it
(env-switch! dev-env)
;; Now Gson classes are available

;; Return to parent
(env-switch!)
```

### env-classpath

List JARs in an environment's classpath:

```lisp
(define my-env (env :depends '("com.google.code.gson:gson:2.10.1")))
(env-classpath my-env)
;; => ("/Users/.../.m2/repository/.../gson-2.10.1.jar" ...)

;; Current environment
(env-classpath)
```

### env-parent

Get parent environment:

```lisp
(env-parent my-env)  ; => parent environment
(env-parent)         ; => current environment's parent
```

### env?

Test if a value is an Environment:

```lisp
(env? my-env)  ; => true
(env? 42)      ; => false
```

## Dependency Notation

Maven coordinates use the standard format:

```
groupId:artifactId:version
```

Examples:
- `"com.google.code.gson:gson:2.10.1"`
- `"org.apache.commons:commons-lang3:3.14.0"`
- `"org.slf4j:slf4j-api:2.0.9"`

## Repository Sources

Dependencies are resolved in this order:

1. Local Maven cache (`~/.m2/repository`)
2. Maven Central
3. Custom repositories specified via `:repos`

## Classloader Isolation

Each environment with dependencies gets its own `URLClassLoader`:

- Child environments inherit parent's classloader as their parent loader
- Classes loaded in a child environment are NOT visible in the parent
- Different modules can depend on different versions of the same library

```lisp
;; Module A uses Gson 2.8
(module a :depends '("com.google.code.gson:gson:2.8.9") ...)

;; Module B uses Gson 2.10 - no conflict!
(module b :depends '("com.google.code.gson:gson:2.10.1") ...)
```

## Java Interop

All reflection primitives (`class`, `new`, `invoke`, `invoke-static`, `peek-static`, `poke-static`, `instanceof?`)
use the current environment's classloader:

```lisp
(env :depends '("org.apache.commons:commons-lang3:3.14.0")
  ;; These use the child environment's classloader
  (invoke-static "org.apache.commons.lang3.StringUtils" "isBlank" ""))
;; => true
```

## Limitations

- **No dependency unloading**: Once loaded, dependencies stay until the JVM exits.
  To "unload", exit the child environment.
  
- **No hot reload**: Modules with dependencies cannot be redefined.

- **Network required**: First resolution of a dependency requires network access to Maven Central
  (unless already cached in `~/.m2/repository`).
