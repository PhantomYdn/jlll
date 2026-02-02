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
- [Feature Roadmap](ROADMAP.md) - Planned features and enhancements

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
4. **Documentation check** (REQUIRED for new/modified code):
   - Java: Public classes and methods must have JavaDoc (extracted by `therapi-runtime-javadoc` for primitives)
   - JLLL: Functions and macros in `.jlll` files must have `:doc` metadata
   - Update relevant documentation in `docs/` when adding or modifying functionality
5. **Commit all changes**: Ensure working tree is clean

## Lessons Learned

- When modifying output format of a function, search for tests that assert on that output — tests often encode specific format expectations
- JLLL converts Java null to `Null.NULL` — tests should check for both `result == null` and `Cons.isNull()` when expecting null returns

## Design Conventions

### The `:return true` Pattern

Primitives that normally print output to the console can support a `:return true` keyword argument to return data programmatically instead. This enables both interactive and programmatic use:

```lisp
; Interactive use (prints to console)
(env "hash")
(jlll-docs "syntax")

; Programmatic use (returns data structure)
(env "hash" :return true)        ; Returns hash-map or list
(jlll-docs "syntax" :return true) ; Returns markdown string
```

**When to use this pattern:**
- The function has console-oriented output useful for human interaction
- The same data would be useful programmatically (e.g., for AI tools)
- Adding `:return true` doesn't complicate the implementation significantly

**Implementation:**
1. Use `ParameterParser.extractKeywords(values)` to parse arguments
2. Check `ParameterParser.getBoolean(extraction, "return", false)`
3. If true, return appropriate data structure instead of printing

### AI Prompt Style

When writing system prompts for JLLL AI sessions:
- Prefer positive framing ("use eval interactively") over negative prohibitions ("never do X in one call")
- Guide toward good patterns rather than listing anti-patterns

### AI Tools Architecture

AI tools that wrap existing primitives are defined in JLLL (`ai.jlll`) rather than Java:

- **Discovery tools** (`apropos`, `describe`, `env`, `jlll-docs`) - wrap primitives with `:return true`
- **Eval tool** - stays in Java (needs direct Environment access)

Tools are registered via `ai-add-builtin-tools` function called from `ai-session-create`.

See `src/main/resources/ru/ydn/jlll/libs/ai.jlll` for tool definitions.

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

See [ROADMAP.md](ROADMAP.md) for planned features and implementation guidelines.

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
