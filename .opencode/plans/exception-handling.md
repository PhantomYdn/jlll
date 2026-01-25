# Exception Handling Implementation Plan

## Overview

Implement comprehensive exception handling for JLLL with both Java-style `try`/`catch`/`finally` and Scheme-style `guard`.

## Status: Ready to Implement

## Files to Modify

1. `src/main/java/ru/ydn/jlll/libs/KernelLib.java` - Add all primitives
2. `src/test/java/ru/ydn/jlll/tests/JLLLTestCase.java` - Add tests
3. `docs/primitives.md` - Document new functions
4. `docs/special-forms.md` - Document try/guard syntax
5. `ROADMAP.md` - Mark items complete

## Implementation Tasks

| # | Task | Priority |
|---|------|----------|
| 1 | `raise` primitive | High |
| 2 | `error` primitive | High |
| 3 | `exception?` predicate | High |
| 4 | `exception-message` accessor | High |
| 5 | `exception-cause` accessor | High |
| 6 | `try` special form | High |
| 7 | `guard` special form | High |
| 8 | Tests | High |
| 9 | Documentation | Medium |
| 10 | Roadmap update | Medium |

## Code to Add to KernelLib.java

Insert before `Jlll.eval("(load-system-script \"kernel.jlll\")", env);` (around line 916):

```java
// ============== Exception Handling Primitives ==============
new Primitive("raise", env, "Raises an exception. (raise value) throws a JlllException. "
        + "If value is already a Throwable, it is wrapped; otherwise value is converted to string message.")
{
    private static final long serialVersionUID = 8273645091827364501L;

    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        if (values.length() != 1)
        {
            throw new JlllException("raise requires exactly one argument");
        }
        Object err = values.get(0);
        if (err instanceof JlllException)
        {
            throw (JlllException) err;
        }
        else if (err instanceof Throwable)
        {
            throw new JlllException((Throwable) err);
        }
        else
        {
            throw new JlllException(err.toString());
        }
    }
};
new Primitive("error", env, "Raises an error with a message. (error msg...) concatenates all arguments "
        + "into a single error message and raises a JlllException.")
{
    private static final long serialVersionUID = 8273645091827364502L;

    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        StringBuilder sb = new StringBuilder();
        for (Object val : values)
        {
            sb.append(val);
        }
        throw new JlllException(sb.toString());
    }
};
new Primitive("exception?", env,
        "Tests if a value is an exception. (exception? x) returns true if x is a Throwable.")
{
    private static final long serialVersionUID = 8273645091827364503L;

    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        if (values.length() != 1)
        {
            throw new JlllException("exception? requires exactly one argument");
        }
        return values.get(0) instanceof Throwable;
    }
};
new Primitive("exception-message", env,
        "Returns the message of an exception. (exception-message e) returns the error message string.")
{
    private static final long serialVersionUID = 8273645091827364504L;

    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        if (values.length() != 1)
        {
            throw new JlllException("exception-message requires exactly one argument");
        }
        Object err = values.get(0);
        if (err instanceof Throwable)
        {
            return ((Throwable) err).getMessage();
        }
        throw new JlllException("exception-message requires a Throwable, got: " + err.getClass().getName());
    }
};
new Primitive("exception-cause", env,
        "Returns the underlying cause of an exception. (exception-cause e) returns the wrapped Throwable or null.")
{
    private static final long serialVersionUID = 8273645091827364505L;

    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        if (values.length() != 1)
        {
            throw new JlllException("exception-cause requires exactly one argument");
        }
        Object err = values.get(0);
        if (err instanceof Throwable)
        {
            Throwable cause = ((Throwable) err).getCause();
            return cause != null ? cause : Null.NULL;
        }
        throw new JlllException("exception-cause requires a Throwable, got: " + err.getClass().getName());
    }
};
// ============== try/catch/finally Special Form ==============
new Primitive("try", env,
        "Exception handling. (try body... (catch [type] var handler...) (finally cleanup...)). "
                + "Executes body, catches exceptions matching type (string class name or predicate), "
                + "binds to var and runs handler. Finally always runs.")
{
    private static final long serialVersionUID = 8273645091827364506L;

    @Override
    public Object apply(Cons values, Environment env) throws JlllException
    {
        // Parse: separate body, catch clauses, and finally clause
        List<Object> bodyForms = new ArrayList<>();
        List<Cons> catchClauses = new ArrayList<>();
        Cons finallyClause = null;
        Symbol catchSym = Symbol.intern("catch");
        Symbol finallySym = Symbol.intern("finally");
        for (Object form : values)
        {
            if (form instanceof Cons)
            {
                Cons c = (Cons) form;
                if (!c.isNull())
                {
                    Object car = c.car();
                    if (catchSym.equals(car))
                    {
                        catchClauses.add(c);
                        continue;
                    }
                    else if (finallySym.equals(car))
                    {
                        if (finallyClause != null)
                        {
                            throw new JlllException("try: multiple finally clauses not allowed");
                        }
                        finallyClause = c;
                        continue;
                    }
                }
            }
            bodyForms.add(form);
        }
        Object result = null;
        JlllException caughtException = null;
        try
        {
            // Evaluate body forms
            for (Object form : bodyForms)
            {
                result = Evaluator.eval(form, env);
            }
        }
        catch (JlllException e)
        {
            caughtException = e;
            // Try to find a matching catch clause
            boolean handled = false;
            for (Cons catchClause : catchClauses)
            {
                // Parse catch clause: (catch [type-spec] var handler...)
                // Formats:
                //   (catch var handler...)           - catch all
                //   (catch "ClassName" var handler...) - match by class name
                //   (catch predicate var handler...) - match by predicate
                Cons rest = (Cons) catchClause.cdr();
                Object first = rest.car();
                Object typeSpec = null;
                Symbol var;
                Cons handler;
                if (first instanceof Symbol)
                {
                    // Catch-all: (catch var handler...)
                    var = (Symbol) first;
                    handler = (Cons) rest.cdr();
                }
                else
                {
                    // Type-specific: (catch type-spec var handler...)
                    typeSpec = first;
                    Cons afterType = (Cons) rest.cdr();
                    Object varObj = afterType.car();
                    if (!(varObj instanceof Symbol))
                    {
                        throw new JlllException("catch: variable must be a symbol, got: " + varObj);
                    }
                    var = (Symbol) varObj;
                    handler = (Cons) afterType.cdr();
                }
                // Check if this clause matches
                boolean matches = false;
                if (typeSpec == null)
                {
                    // Catch-all always matches
                    matches = true;
                }
                else if (typeSpec instanceof String)
                {
                    // Match by class name
                    String className = (String) typeSpec;
                    try
                    {
                        Class<?> targetClass = Class.forName(className);
                        // Check if the exception or its cause matches
                        matches = targetClass.isAssignableFrom(e.getClass());
                        if (!matches && e.getCause() != null)
                        {
                            matches = targetClass.isAssignableFrom(e.getCause().getClass());
                        }
                    }
                    catch (ClassNotFoundException cnf)
                    {
                        throw new JlllException("catch: unknown exception class: " + className, cnf);
                    }
                }
                else
                {
                    // Evaluate type-spec as predicate
                    Object predValue = Evaluator.eval(typeSpec, env);
                    if (predValue instanceof Procedure)
                    {
                        Object predResult = ((Procedure) predValue).applyEvaluated(env, e);
                        matches = CommonUtil.getBoolean(predResult);
                    }
                    else
                    {
                        throw new JlllException(
                                "catch: type specifier must be a string or procedure, got: " + typeSpec);
                    }
                }
                if (matches)
                {
                    // Bind exception to var and evaluate handler
                    Environment catchEnv = new Environment(env);
                    catchEnv.addBinding(var, e);
                    // Evaluate handler body
                    Object handlerBody = new Cons(Symbol.BEGIN, handler);
                    result = Evaluator.eval(handlerBody, catchEnv);
                    handled = true;
                    caughtException = null; // Exception was handled
                    break;
                }
            }
            if (!handled)
            {
                // No matching catch clause - will re-throw after finally
            }
        }
        finally
        {
            // Execute finally clause if present
            if (finallyClause != null)
            {
                Cons finallyBody = (Cons) finallyClause.cdr();
                Object finallyExpr = new Cons(Symbol.BEGIN, finallyBody);
                Evaluator.eval(finallyExpr, env);
            }
        }
        // Re-throw if not handled
        if (caughtException != null)
        {
            throw caughtException;
        }
        return result;
    }
};
// ============== guard Special Form (Scheme-style) ==============
new Primitive("guard", env,
        "Scheme-style exception handling. (guard (var (test handler...) ... (else default...)) body...). "
                + "Evaluates body, on exception binds it to var and tests clauses like cond.")
{
    private static final long serialVersionUID = 8273645091827364507L;

    @Override
    public Object apply(Cons values, Environment env) throws JlllException
    {
        // Parse: (guard (var clause...) body...)
        if (values.length() < 2)
        {
            throw new JlllException("guard requires at least a clause-spec and body");
        }
        Object clauseSpecObj = values.car();
        if (!(clauseSpecObj instanceof Cons))
        {
            throw new JlllException("guard: first argument must be (var clause...)");
        }
        Cons clauseSpec = (Cons) clauseSpecObj;
        if (clauseSpec.isNull())
        {
            throw new JlllException("guard: clause-spec cannot be empty");
        }
        Object varObj = clauseSpec.car();
        if (!(varObj instanceof Symbol))
        {
            throw new JlllException("guard: variable must be a symbol, got: " + varObj);
        }
        Symbol var = (Symbol) varObj;
        Cons clauses = (Cons) clauseSpec.cdr();
        Cons body = (Cons) values.cdr();
        try
        {
            // Evaluate body
            Object bodyExpr = new Cons(Symbol.BEGIN, body);
            return Evaluator.eval(bodyExpr, env);
        }
        catch (JlllException e)
        {
            // Bind exception to var
            Environment guardEnv = new Environment(env);
            guardEnv.addBinding(var, e);
            // Evaluate clauses like cond
            for (Object clause : clauses)
            {
                if (!(clause instanceof Cons))
                {
                    throw new JlllException("guard: clause must be a list");
                }
                Cons c = (Cons) clause;
                Object test = c.car();
                Cons handlerBody = (Cons) c.cdr();
                boolean matches;
                if (Symbol.intern("else").equals(test))
                {
                    matches = true;
                }
                else
                {
                    Object testResult = Evaluator.eval(test, guardEnv);
                    matches = CommonUtil.getBoolean(testResult);
                }
                if (matches)
                {
                    Object handlerExpr = new Cons(Symbol.BEGIN, handlerBody);
                    return Evaluator.eval(handlerExpr, guardEnv);
                }
            }
            // No matching clause - re-raise
            throw e;
        }
    }
};
```

## Tests to Add to JLLLTestCase.java

Add new test method:

```java
@Test
public void testExceptionHandling() throws Exception
{
    // === raise and error ===
    assertException("(raise \"test error\")", JlllException.class);
    assertException("(error \"test\" \" error\")", JlllException.class);
    
    // === exception? predicate ===
    eval(false, "(exception? 42)");
    eval(false, "(exception? \"string\")");
    // Can't easily test true case without catching an exception
    
    // === Basic try/catch ===
    eval("caught", "(try (raise \"error\") (catch e \"caught\"))");
    eval("no error", "(try \"no error\" (catch e \"caught\"))");
    eval("test error", "(try (raise \"test error\") (catch e (exception-message e)))");
    
    // === try with finally ===
    eval(1, "(define finally-ran 0) (try 1 (finally (set! finally-ran 1))) finally-ran");
    eval(1, "(define finally-ran 0) (try (raise \"e\") (catch e 1) (finally (set! finally-ran 1))) finally-ran");
    
    // === try without matching catch re-raises ===
    assertException("(try (raise \"error\") (catch \"java.io.IOException\" e \"io\"))", JlllException.class);
    
    // === Multiple catch clauses ===
    eval("jlll", "(try (raise \"error\") "
            + "(catch \"java.io.IOException\" e \"io\") "
            + "(catch \"ru.ydn.jlll.common.JlllException\" e \"jlll\") "
            + "(catch e \"other\"))");
    eval("catchall", "(try (raise \"error\") "
            + "(catch \"java.io.IOException\" e \"io\") "
            + "(catch e \"catchall\"))");
    
    // === catch with predicate ===
    eval("matched", "(try (raise \"timeout error\") "
            + "(catch (lambda (e) (string-contains? (exception-message e) \"timeout\")) e \"matched\") "
            + "(catch e \"other\"))");
    eval("other", "(try (raise \"network error\") "
            + "(catch (lambda (e) (string-contains? (exception-message e) \"timeout\")) e \"matched\") "
            + "(catch e \"other\"))");
    
    // === guard basic ===
    eval("caught", "(guard (err (else \"caught\")) (raise \"error\"))");
    eval("no error", "(guard (err (else \"caught\")) \"no error\")");
    
    // === guard with clauses ===
    eval("string error", "(guard (err "
            + "((string-contains? (exception-message err) \"string\") \"string error\") "
            + "(else \"other\")) "
            + "(raise \"string test\"))");
    
    // === guard re-raises when no match ===
    // This needs a specific clause that doesn't match
    assertException("(guard (err ((= 1 2) \"never\")) (raise \"error\"))", JlllException.class);
    
    // === Nested try/catch ===
    eval("inner", "(try "
            + "(try (raise \"inner\") (catch e \"inner\")) "
            + "(catch e \"outer\"))");
    eval("outer", "(try "
            + "(try (raise \"inner\") (catch \"java.io.IOException\" e \"io\")) "
            + "(catch e \"outer\"))");
    
    // === exception-cause ===
    // Test with a wrapped exception from Java interop
    eval(Null.NULL, "(try (raise \"no cause\") (catch e (exception-cause e)))");
}
```

## Documentation Updates

### docs/primitives.md - Add to Kernel Library section:

```markdown
### Exception Handling

| Primitive | Description |
|-----------|-------------|
| `raise` | `(raise value)` - Raises an exception. If value is a Throwable, it's wrapped; otherwise converted to string message. |
| `error` | `(error msg...)` - Raises an error with concatenated message. |
| `exception?` | `(exception? x)` - Returns true if x is a Throwable. |
| `exception-message` | `(exception-message e)` - Returns the message string of an exception. |
| `exception-cause` | `(exception-cause e)` - Returns the underlying cause of an exception, or null. |
```

### docs/special-forms.md - Add new section:

```markdown
## Exception Handling

### try

Java-style exception handling with catch and finally clauses.

**Syntax:**
```lisp
(try
  body...
  (catch [type-spec] var handler...)...
  (finally cleanup...))
```

**Components:**
- `body` - Expressions to evaluate
- `catch` - Exception handler clause (zero or more)
  - `type-spec` - Optional: string class name or predicate function
  - `var` - Symbol to bind the caught exception
  - `handler` - Expressions to evaluate when caught
- `finally` - Optional cleanup code that always runs

**Examples:**
```lisp
;; Basic catch-all
(try
  (risky-operation)
  (catch e
    (println "Error: " (exception-message e))
    default-value))

;; Type-specific catch
(try
  (invoke file "read")
  (catch "java.io.IOException" e
    (println "IO error"))
  (catch e
    (println "Other error")))

;; Predicate-based catch
(try
  (network-call)
  (catch (lambda (e) (string-contains? (exception-message e) "timeout")) e
    (retry))
  (catch e
    (fail)))

;; With finally
(try
  (define conn (open-connection))
  (use conn)
  (finally
    (close conn)))
```

### guard

Scheme-style exception handling (SRFI-34 inspired).

**Syntax:**
```lisp
(guard (var
         (test handler...)
         ...
         (else default...))
  body...)
```

**Components:**
- `var` - Symbol to bind the caught exception
- `test` - Expression evaluated with var bound; if true, run handler
- `else` - Optional catch-all clause (must be last)
- `body` - Expressions to evaluate

**Examples:**
```lisp
;; Basic guard
(guard (err
         ((string? (exception-message err)) "string error")
         (else "unknown error"))
  (risky-operation))

;; Multiple conditions
(guard (err
         ((string-contains? (exception-message err) "not found")
          (println "Item not found")
          default-value)
         ((string-contains? (exception-message err) "permission")
          (println "Access denied")
          (request-permission))
         (else
          (raise err)))  ; re-raise
  (load-resource name))
```
```

## ROADMAP.md Updates

Mark these items as complete:

```markdown
- [x] `raise` - Signal an exception
- [x] `guard` - Scheme-style structured exception handling (Option A)
- [ ] `with-exception-handler` - Install exception handler for dynamic extent (NOT IMPLEMENTED - requires call/cc)
- [x] `try`/`catch`/`finally` - Java-style exception handling (Option B)
- [x] `error` - Convenience for `(raise (make-error message))`
```

## Verification Steps

After implementation:

1. `mvn spotless:apply` - Format code
2. `mvn spotless:check` - Verify formatting
3. `mvn test` - Run all tests
4. `mvn verify` - Full verification with coverage
