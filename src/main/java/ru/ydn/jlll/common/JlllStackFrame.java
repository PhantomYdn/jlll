package ru.ydn.jlll.common;

import java.io.Serializable;

/**
 * A single frame of the JLLL call stack.
 * Used for error reporting with rich context information.
 *
 * <p>
 * Represents: "this expression was being evaluated in this environment,
 * with these arguments already evaluated at the time of the exception"
 * </p>
 */
public class JlllStackFrame implements Serializable
{
    private static final long serialVersionUID = 1L;
    /** The expression being evaluated */
    private final Object expression;
    /**
     * Environment at this stack level.
     * Provides context for debugging.
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
     * Provides context about what succeeded before failure.
     */
    private final Object[] evaluatedArguments;

    /**
     * Creates a new stack frame.
     *
     * @param expression
     *            the JLLL expression being evaluated
     * @param env
     *            the environment (caller decides whether to snapshot)
     * @param argumentPosition
     *            which argument was being evaluated (-1 = operator)
     * @param evaluatedArguments
     *            arguments evaluated before this point (may be null)
     */
    public JlllStackFrame(Object expression, Environment env, int argumentPosition, Object[] evaluatedArguments)
    {
        this.expression = expression;
        this.env = env;
        this.argumentPosition = argumentPosition;
        this.evaluatedArguments = evaluatedArguments != null ? evaluatedArguments.clone() : null;
    }

    /**
     * Returns the expression being evaluated.
     *
     * @return the expression
     */
    public Object getExpression()
    {
        return expression;
    }

    /**
     * Returns the environment at this stack level.
     *
     * @return the environment
     */
    public Environment getEnv()
    {
        return env;
    }

    /**
     * Returns which argument position was being evaluated.
     *
     * @return -1 for operator, 0+ for argument index
     */
    public int getArgumentPosition()
    {
        return argumentPosition;
    }

    /**
     * Returns the arguments that were already evaluated.
     *
     * @return array of evaluated arguments, or null
     */
    public Object[] getEvaluatedArguments()
    {
        return evaluatedArguments;
    }

    @Override
    public String toString()
    {
        return expression != null ? expression.toString() : "null";
    }
}
