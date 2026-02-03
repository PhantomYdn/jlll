# Changelog

All notable changes to JLLL are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.4] - 2026-02-02

### Added

- **Lexical Closures**: Lambdas now properly capture their definition-time environment, enabling true closures
  ```lisp
  (define (make-adder n) (lambda (x) (+ x n)))
  (define add5 (make-adder 5))
  (add5 10)  ; => 15
  ```
- **Debug Library**: New debugging and development tools
  - `trace` / `untrace` - Deep function call tracing
  - `assert` - Assertions with messages
  - `inspect` - Object inspection
  - `tap` - Debug in pipelines without breaking flow
- **SAM/Functional Interface Conversion**: Lambdas automatically convert to Java functional interfaces (Runnable, Comparator, Consumer, Function, Predicate, etc.)
- **AI Session Persistence**: Save and restore AI conversations with `ai-session-save`, `ai-session-load`, and auto-save support
- **AI Tool Tracing**: Debug AI tool calls with `ai-session-trace-tools`
- **Tier-Based Model Selection**: Use semantic tiers (`:best`, `:fast`, `:reasoning`) instead of specific model names
- **Scheme Vector Primitives**: `vector`, `vector-ref`, `vector-set!`, `vector-length`, `vector->list`, `list->vector`
- **`apropos` function**: Search for symbols by pattern
- **Cookbook documentation**: Practical examples and patterns in `docs/cookbook.md`

### Changed

- LangChain4j upgraded to fix AI tool execution issues
- AI prompt improvements for better JLLL-specific syntax guidance
- Numeric type widening for better Java method matching

### Fixed

- Tool messages now properly persisted in AI session history
- `ai-session-create` and `ai-session-auto-save` bugs
- Swing `DISPOSE_ON_CLOSE` instead of `EXIT_ON_CLOSE` in demos

## [2.3] - 2026-01-29

### Added

- **AI/LLM Integration Library**: Full-featured AI integration powered by LangChain4j
  - Multiple provider support: Anthropic Claude, OpenAI, Ollama (local)
  - Session management with conversation history
  - Streaming responses as lazy sequences
  - Tool/function calling with automatic JLLL procedure integration
  - Console output capture so AI can see printed results
- **JSON Library**: `json-parse`, `json-stringify`, `json-read-file`, `json-write-file`
- **Regular Expressions Library**: `regex-matches?`, `regex-match`, `regex-match-all`, `regex-replace`, `regex-split`
- **Date/Time Library**: `now`, `make-date`, `date-year`, `date-format`, `date-add`, `date-diff`, and more
- **`nil` binding**: Alias for `null` following Lisp convention
- **Demos directory**: Example scripts showcasing JLLL features

### Changed

- **REPL improvements**:
  - Nil suppression: Side-effect-only operations like `print` no longer show `nil` as result
  - Improved `describe`: Documentation now shown prominently at top of output

## [2.2.0] - 2026-01-27

### Added

- **Dynamic Classpath Support**: Load Maven dependencies at runtime with environment-scoped classloaders
  ```lisp
  (env :depends '("com.google.code.gson:gson:2.10.1")
    (define Gson (class "com.google.gson.Gson"))
    (invoke (new Gson) "toJson" '(1 2 3)))
  ```
  - Full transitive Maven dependency resolution
  - Environment-scoped classloader isolation
  - Custom repository support with `:repos`
  - Introspection via `env-classpath`
- **Console Abstraction**: Unified styled I/O operations across CLI and REPL modes
  - `Console` interface with `PlainConsole` and `JLineConsole` implementations
  - Single `*console*` binding
  - Semantic output methods: `printHeader()`, `printError()`, `printWarning()`, `printSuccess()`

### Breaking Changes

- `*stdout*`, `*stdin*` bindings removed - use `*console*` instead
- `*terminal*`, `*color-enabled*` bindings removed from REPL

## [2.1.0] - 2026-01-26

### Added

- **Lazy Sequences**: Infinite/deferred sequences with transparent integration
  - `delay` / `force` - Basic lazy evaluation
  - `lazy-cons`, `lazy-range`, `lazy-map`, `lazy-filter`
  - `iterate`, `cycle`, `repeat` - Infinite sequence generators
  - `realize` - Force entire lazy sequence to list
- **Module System**: `module`, `import`, `require`, `export` for code organization
- **Exception Handling**:
  - `try` / `catch` / `finally` - Java-style exception handling
  - `guard` - Scheme-style structured exception handling (SRFI-34)
  - `raise` - Signal exceptions
  - `call/cc` - Call with current continuation
- **Concurrency Library**: `future`, `atom`, `pmap`, `pcalls`, `pfor-each` for parallel execution
- **StringLib**: 25+ string operations (split, join, trim, case conversion, etc.)
- **FileLib**: File I/O (`slurp`, `spit`, ports, path manipulation)
- **HashLib**: Hash maps (`hash-map`, `hash-ref`, `hash-set!`, etc.)
- **InputLib**: Input operations (`read`, `read-line`, `read-char`)
- **Keyword arguments**: `(define (greet :name "World") ...)` with defaults
- **Control flow macros**: `when`, `unless`, `let*`, `dotimes`, `dolist`, `do`
- **Type predicates**: `number?`, `integer?`, `real?`, `pair?`, `vector?`, `port?`
- **Numeric functions**: `<=`, `>=`, `zero?`, `positive?`, `negative?`, `even?`, `odd?`, `gcd`, `lcm`, `expt`
- **Symbol utilities**: `gensym`, `symbol=?`
- **REPL Improvements**:
  - JLine 3 integration with advanced line editing and history
  - Syntax highlighting and bracket matching
  - Auto-indentation for multi-line expressions
  - Tab completion for Lisp symbols
  - `describe` for inline function documentation
  - `env` for exploring bindings

## [2.0.0] - 2026-01-19

### Changed

- **Java 17**: Upgraded minimum Java version from 8 to 17
- **CI/CD**: Migrated from Travis CI to GitHub Actions
- **Maven Central**: Automated publishing via GitHub Actions workflow
- **Documentation**: Modernized README with features, quick start guide, and REPL documentation
- **SNAPSHOT Publishing**: Automatic SNAPSHOT publishing on every successful push to master

### Migration Notes

This is a major version bump due to the Java version requirement change. Projects using Java 8-16 should continue using v1.x.

[Unreleased]: https://github.com/PhantomYdn/jlll/compare/v2.4...HEAD
[2.4]: https://github.com/PhantomYdn/jlll/compare/v2.3...v2.4
[2.3]: https://github.com/PhantomYdn/jlll/compare/v2.2.0...v2.3
[2.2.0]: https://github.com/PhantomYdn/jlll/compare/v2.1.0...v2.2.0
[2.1.0]: https://github.com/PhantomYdn/jlll/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/PhantomYdn/jlll/compare/v1.0...v2.0.0
