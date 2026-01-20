# JLLL - Java Lisp Like Language

Lightweight, embeddable Java library bringing Lisp-flavored programming to Java projects.

**Requirements:** Java 17+

## Build Commands

```bash
# Compile
mvn compile

# Run all tests
mvn test

# Run a single test class
mvn test -Dtest=JLLLTestCase

# Run a single test method
mvn test -Dtest=JLLLTestCase#testTokenizer

# Package JARs (library + CLI fat JAR)
mvn package
# Output:
#   target/jlll-*.jar       - Library JAR (for embedding)
#   target/jlll-*-cli.jar   - Executable fat JAR with all dependencies

# Run CLI
java -jar target/jlll-*-cli.jar --help
java -jar target/jlll-*-cli.jar -e '(+ 1 2)'
java -jar target/jlll-*-cli.jar  # Start REPL

# Full clean build and install
mvn clean install

# Run tests with database (enables SQLLibTestCase)
mvn test -Ddburl=<jdbc-url>

# Code coverage report (generates target/site/jacoco/index.html)
mvn verify

# Check code formatting (CI uses this)
mvn spotless:check

# Auto-format all source files
mvn spotless:apply
```

## Project Structure

```
src/
├── main/
│   ├── java/ru/ydn/jlll/
│   │   ├── common/           # Core: Jlll, Cons, Symbol, Enviroment, Evaluator
│   │   │   └── annotation/   # @JlllName, @JlllDoc
│   │   ├── cli/              # CLI: JlllCli, JlllRepl, JlllCompleter, JlllHighlighter
│   │   ├── io/               # Tokenizer, Marshaller, Renderer
│   │   ├── libs/             # Built-in libraries (Kernel, Math, List, etc.)
│   │   └── util/             # Utility classes
│   └── resources/ru/ydn/jlll/
│       ├── common/           # init.jlll bootstrap
│       ├── cli/              # version.properties (Maven-filtered)
│       └── libs/             # Lisp scripts (math.jlll, list.jlll)
└── test/
    └── java/ru/ydn/jlll/tests/   # Test cases
```

## Code Style Guidelines

### Formatting

- **Brace style**: Allman (opening brace on new line)
- **Indentation**: 4 spaces (no tabs)
- **Line length**: No strict limit, but keep readable

```java
public class Example
{
    public void method()
    {
        if (condition)
        {
            // code
        }
        else
        {
            // code
        }
    }
}
```

### Imports

- Use individual imports, not wildcards
- Static imports allowed for test assertions
- Order: java.*, then external libs, then project imports

```java
package ru.ydn.jlll.tests;

import java.lang.reflect.Method;
import java.math.BigInteger;

import org.junit.Test;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Jlll;
import static org.junit.Assert.*;
```

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Classes | PascalCase | `KernelLib`, `ConsRenderer` |
| Interfaces | PascalCase | `Library`, `ConsRenderHandler` |
| Methods | camelCase | `applayEvaluated`, `getFieldA` |
| Variables | camelCase | `fieldA`, `jlllCouse` |
| Constants | UPPER_SNAKE_CASE | `GENERIC_COMPARATOR`, `TRUE` |
| Packages | lowercase | `ru.ydn.jlll.common` |
| Test classes | `*TestCase` | `JLLLTestCase`, `RendererTestCase` |

### Type Conventions

- All core classes implement `Serializable` with explicit `serialVersionUID`
- Use generics where appropriate: `List<Object>`, `Map<String,Symbol>`
- Prefer primitives and autoboxing over wrapper constructors

```java
public class Symbol implements Serializable
{
    private static final long serialVersionUID = 3176952970569428659L;
    // ...
}

// Good: use autoboxing or Boolean constants
return Boolean.TRUE;
return 42;  // autoboxed to Integer

// Avoid: deprecated wrapper constructors
// return new Boolean(true);  // deprecated
// return new Integer(42);    // deprecated
```

### Error Handling

- Use `JlllException` for interpreter/language errors
- Wrap underlying exceptions with context

```java
public void method() throws JlllException
{
    try
    {
        // risky operation
    }
    catch (Exception e)
    {
        throw new JlllException("Context message", e);
    }
}
```

### Annotations

- `@JlllName("name")` - Bind Java method to Lisp function name
- `@JlllDoc("description")` - Document a primitive
- `@Override` - Always use when overriding methods

```java
@JlllName("testMethod")
public int testMethod(Enviroment env, String arg)
{
    return 0;
}
```

## Testing Patterns

### Test Structure

- Test classes extend nothing (JUnit 4 annotations)
- Use `@Test` annotation on test methods
- Test methods named `test*`

```java
public class ExampleTestCase
{
    private final Enviroment env;

    public ExampleTestCase()
    {
        env = new Enviroment(Enviroment.top);
    }

    @Test
    public void testFeature() throws Exception
    {
        // test code
    }
}
```

### Common Test Helpers

Use private helper methods for repeated assertions:

```java
private void eval(Object expected, String code) throws Exception
{
    Object ret = Jlll.eval(code, env);
    assertEquals(expected, ret);
}

private void assertException(String code, Class<JlllException> clss) throws Exception
{
    try
    {
        Jlll.eval(code, env);
    }
    catch (Exception e)
    {
        if (clss.isAssignableFrom(e.getClass())) return;
        else throw e;
    }
    throw new AssertionFailedError("Exception was not thrown");
}
```

## Key Classes

| Class | Purpose |
|-------|---------|
| `Jlll` | Main entry point - `prepare()`, `eval()`, `invoke()` |
| `Cons` | Lisp cons cell (car/cdr pair) |
| `Symbol` | Interned symbol with `Symbol.intern()` |
| `Enviroment` | Variable bindings and scope |
| `Evaluator` | Expression evaluation |
| `Primitive` | Built-in function base class |
| `Library` | Interface for loadable libraries |
| `JlllException` | Language/interpreter errors |

## Adding New Primitives

1. Create a class extending `Primitive` or implement in a `Library`
2. Use `@JlllName` to define the Lisp-visible name
3. First parameter can be `Enviroment env` (injected automatically)
4. Varargs supported for variable-argument functions

```java
new Primitive("my-func", env)
{
    private static final long serialVersionUID = 1L;

    public Object applay(Cons values, Enviroment env) throws JlllException
    {
        // Implementation
        return result;
    }
};
```

## CI/CD

- **CI**: GitHub Actions (`.github/workflows/ci.yml`)
  - Runs on push/PR to master/main
  - Tests on Java 17 and 21
  - Code coverage with JaCoCo + Codecov
  - Publishes SNAPSHOT to Maven Central Portal on every successful push to master/main

- **Releases**: GitHub Actions (`.github/workflows/release.yml`)
  - Manual workflow dispatch with version input
  - GPG signs artifacts
  - Publishes to Maven Central Portal
  - Creates GitHub Release with notes

### Required GitHub Secrets

| Secret | Purpose |
|--------|---------|
| `CENTRAL_USERNAME` | Sonatype Central Portal token username |
| `CENTRAL_PASSWORD` | Sonatype Central Portal token password |
| `GPG_PRIVATE_KEY` | ASCII-armored GPG private key for signing |
| `GPG_PASSPHRASE` | GPG key passphrase |
| `CODECOV_TOKEN` | (Optional) Codecov upload token |
