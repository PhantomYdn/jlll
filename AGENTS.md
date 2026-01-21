# JLLL - Java Lisp Like Language

Lightweight, embeddable Java library bringing Lisp-flavored programming to Java projects.

**Requirements:** Java 17+

## Language Documentation

See [docs/](docs/) for comprehensive JLLL language documentation:

- [Overview & Quick Start](docs/README.md)
- [Syntax](docs/syntax.md) - Atoms, lists, reader macros, keywords
- [Special Forms](docs/special-forms.md) - define, if, lambda, let, cond
- [Procedures](docs/procedures.md) - Keyword arguments, defaults, rest args
- [Metadata](docs/metadata.md) - Documentation and metadata on bindings
- [Primitives](docs/primitives.md) - Built-in functions by library
- [Macros](docs/macros.md) - Macro definition and expansion
- [Java Interop](docs/java-interop.md) - Calling Java from JLLL

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

## Workflow Rules

Before committing or ending a session:

1. **Format check**: `mvn spotless:check` (or `mvn spotless:apply` to fix)
2. **Run tests**: `mvn test`
3. **Verify build**: `mvn verify` (includes coverage)
4. **Documentation check** (for new/modified code in session):
   - Java: Public classes and methods should have JavaDoc (extracted by `therapi-runtime-javadoc` for primitives)
   - JLLL: Functions and macros in `.jlll` files should have `:doc` metadata
5. **Commit all changes**: Ensure working tree is clean

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
- **Enforcement**: Spotless plugin auto-formats code

### Imports

- Use individual imports, not wildcards
- Static imports allowed for test assertions
- Order: java.*, then external libs, then project imports

### Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| Classes | PascalCase | `KernelLib`, `ConsRenderer` |
| Interfaces | PascalCase | `Library`, `ConsRenderHandler` |
| Methods | camelCase | `applyEvaluated`, `getFieldA` |
| Variables | camelCase | `fieldA`, `jlllCouse` |
| Constants | UPPER_SNAKE_CASE | `GENERIC_COMPARATOR`, `TRUE` |
| Packages | lowercase | `ru.ydn.jlll.common` |
| Test classes | `*TestCase` | `JLLLTestCase`, `RendererTestCase` |

### Type Conventions

- All core classes implement `Serializable` with explicit `serialVersionUID`
- Use generics where appropriate: `List<Object>`, `Map<String,Symbol>`
- Prefer primitives and autoboxing over wrapper constructors

### Error Handling

- Use `JlllException` for interpreter/language errors
- Wrap underlying exceptions with context

### Annotations

- `@JlllName("name")` - Bind Java method to Lisp function name
- `@JlllDoc("description")` - Document a primitive
- `@Override` - Always use when overriding methods

## Testing Patterns

### Test Structure

- Test classes extend nothing (JUnit 4 annotations)
- Use `@Test` annotation on test methods
- Test methods named `test*`

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

    public Object apply(Cons values, Enviroment env) throws JlllException
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
