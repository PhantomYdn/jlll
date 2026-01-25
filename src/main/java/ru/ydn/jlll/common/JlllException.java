package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.List;

/**
 * Exception thrown during JLLL parsing or evaluation.
 *
 * <p>
 * Serves dual purpose:
 * </p>
 * <ol>
 * <li>Error reporting - with rich stack trace showing JLLL context</li>
 * <li>Continuation capture - stack frames enable replay for call/cc</li>
 * </ol>
 *
 * <p>
 * The {@code source} field indicates the origin:
 * </p>
 * <ul>
 * <li>{@code null}: internal error (eval failure, type error, etc.)</li>
 * <li>{@code Continuation}: continuation invocation (for call/cc)</li>
 * <li>other object: user-raised via (raise value)</li>
 * </ul>
 */
public class JlllException extends Exception
{
    private static final long serialVersionUID = 8810568891407802853L;
    /** Rich stack frames for continuations and debugging */
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

    /**
     * Creates an exception with the given message.
     *
     * @param s
     *            the error message
     */
    public JlllException(String s)
    {
        super(s);
    }

    /**
     * Creates an exception wrapping another throwable.
     *
     * @param couse
     *            the underlying cause
     */
    public JlllException(Throwable couse)
    {
        this(couse.getMessage(), couse);
    }

    /**
     * Creates an exception with message and cause.
     *
     * @param s
     *            the error message
     * @param couse
     *            the underlying cause
     */
    public JlllException(String s, Throwable couse)
    {
        super(s, couse);
    }

    /**
     * Creates an exception with message and JLLL context.
     *
     * @param s
     *            the error message
     * @param jlllCouse
     *            the JLLL expression that caused the error
     */
    public JlllException(String s, Object jlllCouse)
    {
        super(s);
        addJlllCouse(jlllCouse, null, -1, null);
    }

    /**
     * Creates an exception with message, JLLL context, and Java cause.
     *
     * @param s
     *            the error message
     * @param jlllCouse
     *            the JLLL expression that caused the error
     * @param couse
     *            the underlying Java exception
     */
    public JlllException(String s, Object jlllCouse, Throwable couse)
    {
        super(s, couse);
        addJlllCouse(jlllCouse, null, -1, null);
    }

    /**
     * Creates a raised exception - from continuation or user (raise ...).
     *
     * @param source
     *            what raised this (Continuation for call/cc, other for user raise)
     * @param value
     *            the value being returned/raised
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
     * <p>
     * For continuations, the environment is snapshotted to preserve state.
     * For regular errors, environment may be null (just for display).
     * </p>
     *
     * @param expression
     *            the JLLL expression being evaluated
     * @param env
     *            the environment (will be snapshotted for continuations)
     * @param argPosition
     *            which argument was being evaluated (-1 = operator)
     * @param evaluatedArgs
     *            arguments evaluated before this point
     */
    public void addJlllCouse(Object expression, Environment env, int argPosition, Object[] evaluatedArgs)
    {
        if (expression == null)
        {
            return;
        }
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
     * Used when detailed context is not available.
     *
     * @param couse
     *            the JLLL expression to add to the trace
     */
    public void addJlllCouse(Object couse)
    {
        addJlllCouse(couse, null, -1, null);
    }

    /**
     * Returns the rich stack frames.
     *
     * @return list of stack frames, or null if none
     */
    public List<JlllStackFrame> getStackFrames()
    {
        return stackFrames;
    }
    // === Source/Value Access ===

    /**
     * Returns what raised this exception.
     *
     * @return null for internal errors, Continuation for call/cc, other for user raise
     */
    public Object getSource()
    {
        return source;
    }

    /**
     * Returns the value associated with this exception.
     *
     * @return the continuation return value, raised value, or null
     */
    public Object getValue()
    {
        return value;
    }
    // === Backward Compatible String Output ===

    /**
     * Returns the JLLL stack trace as a string.
     *
     * @return the formatted JLLL trace
     */
    public String jlllCause()
    {
        StringBuffer sb = new StringBuffer(super.toString());
        if (stackFrames != null && stackFrames.size() > 0)
        {
            sb.append("\njlll:\n");
            for (JlllStackFrame frame : stackFrames)
            {
                sb.append("\tat ").append(frame).append("\n");
            }
        }
        return sb.toString();
    }

    @Override
    public String toString()
    {
        StringBuffer sb = new StringBuffer(super.toString());
        if (stackFrames != null && stackFrames.size() > 0)
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
     * Continuations use exception for control flow, not error reporting.
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
