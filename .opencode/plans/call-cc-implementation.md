# call/cc Implementation Plan for JLLL

## Overview

Implement full `call-with-current-continuation` (call/cc) support in JLLL using an enhanced `JlllException` with rich stack frames for both error handling and continuation capture.

## Design Philosophy

### Unified Exception Approach

Instead of creating separate exception classes for errors and continuations, we enhance `JlllException` to serve both purposes:

1. **Rich stack frames** replace the string-only `jlllCouse` list
2. **Source field** indicates what raised the exception (error vs continuation vs user raise)
3. **Single catch block** in Evaluator handles all cases uniformly
4. **Identity check** distinguishes "our" continuation from others

### Key Insight

The existing `addJlllCouse(Object)` only stores strings, losing valuable information. By making it capture rich stack frames (expression + environment + position), we enable:

- Better error messages with full context
- Continuation capture and replay
- Future: Common Lisp-style restarts/conditions

---

## Core Data Structures

### 1. JlllStackFrame

Represents one level of the JLLL call stack with full context.

```java
package ru.ydn.jlll.common;

import java.io.Serializable;

/**
 * A single frame of the JLLL call stack.
 * Used for both error reporting and continuation capture.
 * 
 * Represents: "this expression was being evaluated in this environment,
 * with these arguments already evaluated at the time of the exception"
 */
public class JlllStackFrame implements Serializable
{
    private static final long serialVersionUID = 1L;
    
    /** The expression being evaluated (e.g., (+ 1 <hole> 3)) */
    private final Object expression;
    
    /** 
     * Snapshot of environment at this stack level.
     * For continuations: must preserve bindings as they were.
     * For errors: provides context for debugging.
     */
    private final Environment env;
    
    /**
     * Which argument position was being evaluated when exception occurred.
     * -1 = operator position (the procedure itself)
     * 0+ = argument index (0 = first argument, etc.)
     */
    private final int argumentPosition;
    
    /**
     * Arguments that were already evaluated before the exception.
     * For continuations: these don't need re-evaluation during replay.
     * For errors: provides context about what succeeded before failure.
     */
    private final Object[] evaluatedArguments;
    
    public JlllStackFrame(Object expression, Environment env, 
                          int argumentPosition, Object[] evaluatedArguments)
    {
        this.expression = expression;
        this.env = env;  // Note: caller decides whether to snapshot
        this.argumentPosition = argumentPosition;
        this.evaluatedArguments = evaluatedArguments != null 
            ? evaluatedArguments.clone() 
            : null;
    }
    
    public Object getExpression() { return expression; }
    public Environment getEnv() { return env; }
    public int getArgumentPosition() { return argumentPosition; }
    public Object[] getEvaluatedArguments() { return evaluatedArguments; }
    
    /**
     * Replay this frame with the given value substituted at the hole position.
     * Used by continuations to reconstruct the computation.
     */
    public Object replay(Object value) throws JlllException
    {
        if (!(expression instanceof Cons))
        {
            // Not a procedure call - just return the value
            return value;
        }
        
        Cons expr = (Cons) expression;
        if (expr.isNull())
        {
            return value;
        }
        
        // Get the procedure (first element)
        Object procObj = Evaluator.eval(expr.car(), env);
        if (!(procObj instanceof Procedure))
        {
            throw new JlllException("Continuation replay: not a procedure");
        }
        Procedure proc = (Procedure) procObj;
        
        // Reconstruct argument list
        Cons argExprs = (Cons) expr.cdr();
        List<Object> args = new ArrayList<>();
        
        int pos = 0;
        for (Object argExpr : argExprs)
        {
            if (pos < argumentPosition)
            {
                // Use pre-evaluated value if available
                if (evaluatedArguments != null && pos < evaluatedArguments.length)
                {
                    args.add(evaluatedArguments[pos]);
                }
                else
                {
                    // Re-evaluate (fallback)
                    args.add(Evaluator.eval(argExpr, env));
                }
            }
            else if (pos == argumentPosition)
            {
                // Substitute the continuation value
                args.add(value);
            }
            else
            {
                // Evaluate remaining arguments
                args.add(Evaluator.eval(argExpr, env));
            }
            pos++;
        }
        
        // Call the procedure with reconstructed arguments
        Cons argCons = ListUtil.arrayToCons(args.toArray());
        return proc.applyEvaluated(argCons, env);
    }
    
    @Override
    public String toString()
    {
        return expression != null ? expression.toString() : "null";
    }
}
```

### 2. Enhanced JlllException

Single exception class for errors and continuations, differentiated by `source` field.

```java
package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.List;

/**
 * Exception thrown during JLLL parsing or evaluation.
 * 
 * Serves dual purpose:
 * 1. Error reporting - with rich stack trace showing JLLL context
 * 2. Continuation capture - stack frames enable replay
 * 
 * The 'source' field indicates the origin:
 * - null: internal error (eval failure, type error, etc.)
 * - Continuation: continuation invocation (for call/cc)
 * - other object: user's (raise value)
 */
public class JlllException extends Exception
{
    private static final long serialVersionUID = 8810568891407802853L;
    
    /** Rich stack frames (replaces old string-only jlllCouse) */
    private List<JlllStackFrame> stackFrames = null;
    
    /**
     * What raised this exception:
     * - null: internal/system error
     * - Continuation instance: continuation invocation
     * - other: user-raised via (raise value)
     */
    private Object source = null;
    
    /**
     * The value associated with this exception:
     * - For continuations: the return value passed to k
     * - For (raise value): the raised value
     * - For internal errors: null (message is in getMessage())
     */
    private Object value = null;
    
    // === Constructors ===
    
    /** Internal error with message */
    public JlllException(String message)
    {
        super(message);
    }
    
    /** Internal error with message and Java cause */
    public JlllException(String message, Throwable cause)
    {
        super(message, cause);
    }
    
    /** Internal error wrapping another throwable */
    public JlllException(Throwable cause)
    {
        super(cause.getMessage(), cause);
    }
    
    /** Internal error with message and JLLL context */
    public JlllException(String message, Object jlllContext)
    {
        super(message);
        addJlllCouse(jlllContext, null, -1, null);
    }
    
    /** Internal error with message, JLLL context, and Java cause */
    public JlllException(String message, Object jlllContext, Throwable cause)
    {
        super(message, cause);
        addJlllCouse(jlllContext, null, -1, null);
    }
    
    /**
     * Raised exception - from continuation or user (raise ...).
     * 
     * @param source what raised this (Continuation for call/cc, other for user raise)
     * @param value the value being returned/raised
     */
    public JlllException(Object source, Object value)
    {
        super(value != null ? value.toString() : "Exception raised");
        this.source = source;
        this.value = value;
    }
    
    // === Stack Frame Management ===
    
    /**
     * Adds a JLLL stack frame during exception unwinding.
     * Called by Evaluator as exception propagates up the call stack.
     * 
     * @param expression the JLLL expression being evaluated
     * @param env the environment (will be snapshotted for continuations)
     * @param argPosition which argument was being evaluated (-1 = operator)
     * @param evaluatedArgs arguments evaluated before this point
     */
    public void addJlllCouse(Object expression, Environment env, 
                             int argPosition, Object[] evaluatedArgs)
    {
        if (expression == null) return;
        
        if (stackFrames == null)
        {
            stackFrames = new ArrayList<>();
        }
        
        // For continuations, snapshot the environment to preserve state
        Environment frameEnv = env;
        if (source instanceof Continuation && env != null)
        {
            frameEnv = env.snapshot();
        }
        
        stackFrames.add(new JlllStackFrame(expression, frameEnv, argPosition, evaluatedArgs));
    }
    
    /**
     * Backward-compatible: add context with just expression.
     */
    public void addJlllCouse(Object expression)
    {
        addJlllCouse(expression, null, -1, null);
    }
    
    /**
     * Returns the rich stack frames.
     */
    public List<JlllStackFrame> getStackFrames()
    {
        return stackFrames;
    }
    
    // === Source/Value Access ===
    
    public Object getSource() { return source; }
    public Object getValue() { return value; }
    
    // === Backward Compatible String Output ===
    
    /**
     * Returns the JLLL stack trace as a string.
     */
    public String jlllCause()
    {
        StringBuilder sb = new StringBuilder(super.toString());
        if (stackFrames != null && !stackFrames.isEmpty())
        {
            sb.append("\njlll:\n");
            for (JlllStackFrame frame : stackFrames)
            {
                sb.append("\tat ").append(frame.getExpression()).append("\n");
            }
        }
        return sb.toString();
    }
    
    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder(super.toString());
        if (stackFrames != null && !stackFrames.isEmpty())
        {
            sb.append("\njlll:\n");
            for (JlllStackFrame frame : stackFrames)
            {
                sb.append("\tat ").append(frame).append("\n");
            }
            sb.append("java:");
        }
        return sb.toString();
    }
    
    // === Optimization ===
    
    /**
     * Skip expensive Java stack trace for continuations.
     */
    @Override
    public Throwable fillInStackTrace()
    {
        if (source instanceof Continuation)
        {
            return this;
        }
        return super.fillInStackTrace();
    }
}
```

### 3. Continuation (The Callable Continuation Object)

```java
package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.List;

/**
 * A first-class continuation object, created by call/cc.
 * 
 * When invoked:
 * - First time: throws JlllException with source=this to capture stack
 * - Subsequent times: replays captured frames with new value
 */
public class Continuation extends Procedure
{
    private static final long serialVersionUID = 1L;
    
    /** 
     * Captured stack frames after first invocation.
     * null = not yet captured (will throw to capture)
     * non-null = captured (will replay)
     */
    private List<JlllStackFrame> capturedFrames = null;
    
    /** Environment where call/cc was invoked */
    private final Environment captureEnv;
    
    public Continuation(Environment env)
    {
        this.captureEnv = env.snapshot();
    }
    
    /**
     * Called when the continuation is invoked with a value.
     */
    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        Object value = values.length() > 0 ? values.get(0) : Null.NULL;
        
        if (capturedFrames == null)
        {
            // First invocation - throw to unwind and capture
            throw new JlllException(this, value);
        }
        else
        {
            // Subsequent invocation - replay captured frames
            return replay(value);
        }
    }
    
    /**
     * Called by call/cc after catching the exception.
     * Stores the captured frames for future replay.
     */
    public void setCapturedFrames(List<JlllStackFrame> frames)
    {
        this.capturedFrames = new ArrayList<>(frames);
    }
    
    /**
     * Replay the captured continuation with a new value.
     */
    private Object replay(Object value) throws JlllException
    {
        if (capturedFrames == null || capturedFrames.isEmpty())
        {
            // No frames = continuation was at top level
            return value;
        }
        
        Object result = value;
        
        // Replay frames from innermost to outermost
        for (int i = 0; i < capturedFrames.size(); i++)
        {
            JlllStackFrame frame = capturedFrames.get(i);
            result = frame.replay(result);
        }
        
        return result;
    }
    
    @Override
    public String getDoc()
    {
        return "Continuation captured by call/cc. When called with a value, "
             + "returns that value from the original call/cc expression.";
    }
    
    @Override
    public String describe()
    {
        if (capturedFrames == null)
        {
            return "Continuation (not yet invoked)";
        }
        return "Continuation with " + capturedFrames.size() + " captured frames";
    }
}
```

### 4. Environment.snapshot()

```java
// Add to Environment.java

/**
 * Creates a snapshot of this environment for continuation capture.
 * The snapshot preserves the current bindings but shares object references.
 * 
 * This matches Scheme semantics: continuations see mutations to mutable
 * objects, but don't see new bindings added after capture.
 */
public Environment snapshot()
{
    Environment snap = new Environment(parent != null ? parent.snapshot() : null);
    snap.current = new HashMap<>(this.current);
    snap.metadata = new HashMap<>(this.metadata);
    return snap;
}
```

---

## Modified Evaluator

The evaluator needs to:
1. Track evaluation context (current expression, argument position, evaluated args)
2. Catch `JlllException` and add rich stack frame info
3. No special handling for continuations - they're just JlllExceptions

### EvalContext (Evaluation Context Tracking)

```java
// Add to Evaluator.java

/**
 * Tracks evaluation context for stack frame capture.
 * Stored in ThreadLocal to handle nested evaluations.
 */
public static class EvalContext
{
    /** The expression being evaluated */
    public Object expression;
    
    /** Which argument position is being evaluated (-1 = operator) */
    public int argumentPosition = -1;
    
    /** Arguments evaluated so far */
    public Object[] evaluatedArguments = null;
    
    public EvalContext(Object expr)
    {
        this.expression = expr;
    }
}

/** Thread-local evaluation context stack */
private static final ThreadLocal<EvalContext> evalContext = new ThreadLocal<>();

/** Get current evaluation context (for use by evalEvery) */
public static EvalContext getEvalContext()
{
    return evalContext.get();
}
```

### Modified eval() Method

```java
public static Object eval(Object eval, Environment env) throws JlllException
{
    Object ret = null;
    if (eval instanceof Cons)
    {
        Cons cons = (Cons) eval;
        if (cons.isNull())
        {
            ret = Null.NULL;
        }
        else
        {
            // Create evaluation context for this expression
            EvalContext ctx = new EvalContext(eval);
            EvalContext prevCtx = evalContext.get();
            evalContext.set(ctx);
            
            try
            {
                // Evaluate operator (position = -1)
                ctx.argumentPosition = -1;
                Object car = eval(cons.car(), env);
                
                if (car instanceof Procedure)
                {
                    Procedure proc = (Procedure) car;
                    proc.cnt++;
                    ret = proc.apply(cons.cdr(), env);
                }
                else
                {
                    throw new JlllException("First argument is not Procedure: "
                            + (car == null ? "null" : car.getClass().getName()));
                }
            }
            catch (JlllException exc)
            {
                // Add rich stack frame for ALL JlllExceptions
                exc.addJlllCouse(eval, env, ctx.argumentPosition, ctx.evaluatedArguments);
                throw exc;
            }
            catch (Throwable thr)
            {
                throw new JlllException("Unexpected exception", eval, thr);
            }
            finally
            {
                evalContext.set(prevCtx);
            }
        }
    }
    else if (eval instanceof Symbol)
    {
        ret = env.lookup((Symbol) eval);
        if (ret == null)
            throw new JlllException("Symbol is unbound: " + eval);
    }
    else
    {
        ret = eval;
    }
    return CommonUtil.prepareReturn(ret);
}
```

### Modified evalEvery() in Jlll.java

```java
/**
 * Evaluates every element of a cons list.
 * Tracks argument position for continuation capture.
 */
public static Cons evalEvery(Cons cons, Environment env) throws JlllException
{
    Evaluator.EvalContext ctx = Evaluator.getEvalContext();
    Iterator<?> it = cons.iterator();
    List<Object> ret = new ArrayList<Object>();
    int position = 0;
    
    while (it.hasNext())
    {
        // Update context with current position
        if (ctx != null)
        {
            ctx.argumentPosition = position;
            ctx.evaluatedArguments = ret.toArray();
        }
        
        ret.add(Evaluator.eval(it.next(), env));
        position++;
    }
    
    return ListUtil.arrayToCons(ret.toArray());
}
```

---

## call/cc Primitive Implementation

```java
// Add to KernelLib.java

new Primitive("call/cc", env, 
    "Calls procedure with the current continuation. "
    + "(call/cc (lambda (k) body...)) - k is a procedure that, when called, "
    + "returns its argument as the result of the call/cc expression. "
    + "The continuation can be saved and called multiple times.")
{
    private static final long serialVersionUID = 8273645091827364510L;

    @Override
    public Object apply(Cons values, Environment env) throws JlllException
    {
        if (values.length() != 1)
        {
            throw new JlllException("call/cc requires exactly one argument (a procedure)");
        }
        
        // Evaluate the procedure argument
        Object procObj = Evaluator.eval(values.car(), env);
        if (!(procObj instanceof Procedure))
        {
            throw new JlllException("call/cc argument must be a procedure");
        }
        Procedure proc = (Procedure) procObj;
        
        // Create the continuation object
        Continuation k = new Continuation(env);
        
        try
        {
            // Call (proc k)
            return proc.applyEvaluated(env, k);
        }
        catch (JlllException exc)
        {
            // Check if this is OUR continuation being invoked
            if (exc.getSource() == k)
            {
                // Identity check: this exception is from our continuation
                // Save captured frames for potential future invocation
                k.setCapturedFrames(exc.getStackFrames());
                
                // Return the value passed to the continuation
                return exc.getValue();
            }
            
            // Not our continuation (or not a continuation at all)
            // Propagate the exception
            throw exc;
        }
    }
};

// Add alias
env.cloneBinding("call-with-current-continuation", "call/cc");
```

---

## How It All Works Together

### Scenario: Basic Escape

```lisp
(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))
```

1. Evaluate `(+ 1 ???)` - need to evaluate second argument
2. Evaluate `(call/cc ...)` - creates `Continuation k`
3. Calls `(lambda (k) (+ 2 (k 3)))` with `k`
4. Inside lambda, evaluate `(+ 2 ???)`
5. Evaluate `(k 3)`:
   - `k.applyEvaluated(3)` throws `JlllException(source=k, value=3)`
6. Exception unwinds:
   - `(+ 2 (k 3))` catches, adds frame, re-throws
   - `(lambda ...)` propagates
   - `call/cc` catches, checks `exc.getSource() == k` → **true!**
   - Stores frames in `k`, returns `3`
7. Outer `(+ 1 3)` → **4**

### Scenario: Saved Continuation Called Later

```lisp
(define saved #f)
(+ 1 (call/cc (lambda (k) (set! saved k) 2)))  ; Returns 3
(saved 10)  ; Returns 11
```

1. First `call/cc`:
   - Creates `Continuation k`
   - Lambda sets `saved = k`, returns `2`
   - No exception thrown (k not called yet)
   - `k.capturedFrames` is still `null`
   - `(+ 1 2)` → **3**

2. Call `(saved 10)`:
   - `saved` is the Continuation
   - `k.applyEvaluated(10)` - `capturedFrames` is null, so throws
   - Exception unwinds, captures frame for `(+ 1 ???)`
   - `call/cc` is NOT on stack anymore! Exception propagates to top...
   - **Wait, this is a problem!**

### The "Saved Continuation" Challenge

When the continuation is called **outside** the original `call/cc`, there's no `call/cc` to catch it. We need to handle this case.

**Solution**: The first invocation must always be caught by the original `call/cc`. If it escapes, we have a problem.

Actually, re-reading the flow:

1. `(saved 10)` calls `k.applyEvaluated(10)`
2. Since `capturedFrames == null`, it throws `JlllException(this, 10)`
3. This exception unwinds but **there's no `call/cc` to catch it**!
4. The exception escapes to the top level → Error

**Hmm, this design has a flaw.** The continuation must be called at least once while still inside the original `call/cc` for the frames to be captured.

### Revised Design: Capture on Creation

The issue is that we only capture frames during the *first invocation*. But for "save and call later" to work, we need frames captured even if `k` is never called inside the lambda.

**Solution: Two-phase capture**

1. When `call/cc` is entered, we note the "continuation point"
2. When `k` is invoked (anytime), we unwind to that point
3. The frames between invocation and `call/cc` entry are captured

But if `k` is called **outside** the original `call/cc`, we can't unwind "to" it anymore.

**Alternative: Always capture frames at call/cc entry**

This is harder because we don't know what frames exist above us.

**Practical Solution: Detect and handle "stale" invocation**

If `k` is invoked when not inside the original `call/cc`, we can't capture. But for **replay** mode (second+ invocation), we don't need to be inside `call/cc`.

Let me revise the design:

---

## Revised Design: Tag-based Matching

Add a unique tag to each continuation, and check for matching tag during unwinding.

```java
public class Continuation extends Procedure
{
    private static final AtomicLong tagCounter = new AtomicLong(0);
    private final long tag;
    private List<JlllStackFrame> capturedFrames = null;
    
    public Continuation(Environment env)
    {
        this.tag = tagCounter.incrementAndGet();
        this.captureEnv = env.snapshot();
    }
    
    public long getTag() { return tag; }
    
    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        Object value = values.length() > 0 ? values.get(0) : Null.NULL;
        
        if (capturedFrames == null)
        {
            // First invocation - throw with our tag
            throw new JlllException(this, value);
        }
        else
        {
            // Replay mode
            return replay(value);
        }
    }
}
```

In `call/cc`:

```java
try
{
    return proc.applyEvaluated(env, k);
}
catch (JlllException exc)
{
    if (exc.getSource() == k)  // Identity check!
    {
        k.setCapturedFrames(exc.getStackFrames());
        return exc.getValue();
    }
    throw exc;  // Not ours, propagate
}
```

The **identity check** (`exc.getSource() == k`) ensures we only catch our own continuation. If `k` is called outside the `call/cc`, the exception will propagate to the top level (which is correct - it's an error to call an uncaptured continuation outside its dynamic extent on the first invocation).

**For "save and call later"**: The first call MUST happen inside the `call/cc`. Once frames are captured, subsequent calls can happen anywhere.

This matches many Scheme implementations' behavior for upward continuations.

---

## Test Cases

### Basic Tests

```java
@Test
public void testCallCC() throws Exception
{
    // Basic escape
    eval(4, "(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))");
    
    // No escape - normal return
    eval(5, "(+ 1 (call/cc (lambda (k) (+ 2 2))))");
    
    // Direct return from call/cc
    eval(10, "(call/cc (lambda (k) (k 10)))");
    
    // Nested arithmetic with escape
    eval(7, "(+ 1 (call/cc (lambda (k) (* 2 (k 6)))))");
    
    // Value not used after escape
    eval(3, "(call/cc (lambda (k) (k 3) (k 5)))");  ; Only first k executes
}
```

### Saved Continuation Tests

```java
@Test  
public void testSavedContinuation() throws Exception
{
    // Save and call - first call inside, second outside
    eval(11, "(define saved #f) "
           + "(define result (+ 1 (call/cc (lambda (k) (set! saved k) (k 2))))) "
           + "(saved 10)");  // Replays with 10 → 11
    
    // Call multiple times
    eval(Cons.list(101, 11), 
        "(define saved #f) "
        + "(define r1 (+ 1 (call/cc (lambda (k) (set! saved k) (k 2))))) "
        + "(define r2 (saved 10)) "
        + "(define r3 (saved 100)) "
        + "(list r2 r3)");
}
```

### Nested call/cc Tests

```java
@Test
public void testNestedCallCC() throws Exception
{
    // Outer escape
    eval(42, "(call/cc (lambda (outer) "
           + "  (+ 1 (call/cc (lambda (inner) "
           + "    (outer 42))))))");
    
    // Inner escape
    eval(10, "(call/cc (lambda (outer) "
           + "  (+ 1 (call/cc (lambda (inner) "
           + "    (inner 9))))))");
}
```

---

## Implementation Order

### Phase 1: Core Infrastructure

1. **Create `JlllStackFrame` class** - `src/main/java/ru/ydn/jlll/common/JlllStackFrame.java`
2. **Modify `JlllException`** - Add `source`, `value`, rich `addJlllCouse()`
3. **Add `Environment.snapshot()`** - For capturing environment state
4. **Add `Evaluator.EvalContext`** - Track expression, position, evaluated args
5. **Modify `Evaluator.eval()`** - Use EvalContext, catch JlllException uniformly
6. **Modify `Jlll.evalEvery()`** - Track argument position

### Phase 2: Continuation Support

1. **Create `Continuation` class** - `src/main/java/ru/ydn/jlll/common/Continuation.java`
2. **Add `call/cc` primitive** - In KernelLib.java
3. **Add alias `call-with-current-continuation`**

### Phase 3: Testing

1. Basic escape tests
2. Saved continuation tests
3. Nested call/cc tests
4. Edge cases (error in continuation, etc.)

### Phase 4: Documentation

1. Update `docs/special-forms.md` - Document call/cc
2. Update `docs/primitives.md` - Add call/cc to kernel primitives
3. Update `ROADMAP.md` - Mark complete

---

## Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `JlllStackFrame.java` | **Create** | Rich stack frame with replay support |
| `Continuation.java` | **Create** | First-class continuation procedure |
| `JlllException.java` | **Modify** | Add source, value, rich addJlllCouse |
| `Environment.java` | **Modify** | Add snapshot() method |
| `Evaluator.java` | **Modify** | Add EvalContext, unified exception handling |
| `Jlll.java` | **Modify** | Track position in evalEvery |
| `KernelLib.java` | **Modify** | Add call/cc primitive |
| `JLLLTestCase.java` | **Modify** | Add continuation tests |
| `docs/special-forms.md` | **Modify** | Document call/cc |
| `ROADMAP.md` | **Modify** | Mark as complete |

---

## Key Design Decisions

1. **Single Exception Class**: JlllException handles both errors and continuations
2. **Identity Check**: `exc.getSource() == k` distinguishes "our" continuation
3. **Lazy Environment Snapshot**: Only snapshot when capturing continuation frames
4. **First-call Capture**: Continuation must be called once inside call/cc to capture
5. **Replay for Subsequent Calls**: After capture, continuation can be called anywhere

---

## Limitations

1. **First call must be inside call/cc**: Cannot "save without calling" and call later
2. **Java interop boundary**: Continuations can't cross Java method calls
3. **Single thread**: Continuations are thread-local
4. **No delimited continuations**: Only full call/cc, not shift/reset

---

## Success Criteria

1. `(+ 1 (call/cc (lambda (k) (k 3))))` returns `4`
2. Saved continuations can be replayed multiple times
3. Nested call/cc works correctly
4. All existing tests pass
5. `mvn verify` succeeds
6. Documentation is complete

---

## Important: try/catch/guard Interaction

### The Problem

The existing `try` and `guard` implementations catch all `JlllException`:

```java
catch (JlllException e) {
    // Would incorrectly catch continuations!
}
```

### The Solution

Modify `try` and `guard` to skip continuations at the start of their catch blocks:

```java
catch (JlllException e)
{
    // Continuations must propagate to their call/cc - not errors
    if (e.getSource() instanceof Continuation)
    {
        throw e;
    }
    
    // Normal exception handling continues...
}
```

### Why This Isn't Spaghetti

- Only 2 places need this check: `try` and `guard` primitives
- Simple `instanceof` check at the boundary
- Consistent pattern in both places
- This is type dispatch, not scattered flag checking

### finally Semantics

The Java `finally` block still runs correctly:

```java
try {
    // body with call/cc
} catch (JlllException e) {
    if (e.getSource() instanceof Continuation) throw e;  // Re-throw
    // handle error...
} finally {
    // STILL RUNS even when re-throwing continuation
    if (finallyClause != null) { ... }
}
```

This is **correct** - when a continuation passes through a `try` block, `finally` should execute.
