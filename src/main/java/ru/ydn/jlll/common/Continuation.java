package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.List;
import ru.ydn.jlll.util.ListUtil;

/**
 * A first-class continuation object, created by call/cc.
 *
 * <p>
 * When invoked:
 * </p>
 * <ul>
 * <li>First time: throws JlllException with source=this to escape to call/cc</li>
 * <li>Subsequent times: replays the outer context with new value</li>
 * </ul>
 *
 * <p>
 * The continuation captures "what remains to be done" at the point where
 * call/cc was invoked. Calling the continuation with a value causes that
 * value to be returned from the original call/cc expression.
 * </p>
 */
public class Continuation extends Procedure
{
    private static final long serialVersionUID = 1L;
    /**
     * The outer expression containing call/cc (e.g., (+ 1 (call/cc ...))).
     * This is the context that needs to be replayed when the continuation
     * is invoked after initial capture.
     */
    private Object outerExpression = null;
    /** Environment where call/cc was invoked (snapshotted) */
    private final Environment captureEnv;
    /** Which argument position call/cc was in the outer expression */
    private int outerArgPosition = -1;
    /** Arguments evaluated before call/cc in the outer expression */
    private Object[] outerEvaluatedArgs = null;
    /** Whether this continuation has been captured (first invocation completed) */
    private boolean captured = false;

    /**
     * Creates a new continuation.
     *
     * @param env
     *            the environment where call/cc was invoked
     */
    public Continuation(Environment env)
    {
        this.captureEnv = env.snapshot();
    }

    /**
     * Sets the outer context captured from EvalContext when call/cc was invoked.
     * This provides the "rest of the computation" that needs to be replayed.
     *
     * @param expression
     *            the outer expression (e.g., (+ 1 (call/cc ...)))
     * @param argPosition
     *            which argument position call/cc was at
     * @param evaluatedArgs
     *            arguments evaluated before call/cc
     */
    public void setOuterContext(Object expression, int argPosition, Object[] evaluatedArgs)
    {
        this.outerExpression = expression;
        this.outerArgPosition = argPosition;
        this.outerEvaluatedArgs = evaluatedArgs != null ? evaluatedArgs.clone() : null;
    }

    /**
     * Marks this continuation as captured after first invocation.
     */
    public void markCaptured()
    {
        this.captured = true;
    }

    /**
     * Returns whether this continuation has been captured.
     *
     * @return true if captured
     */
    public boolean isCaptured()
    {
        return captured;
    }

    /**
     * Called when the continuation is invoked with a value.
     *
     * <p>
     * On first invocation, throws a JlllException with this continuation
     * as the source. The call/cc primitive catches it and marks the
     * continuation as captured.
     * </p>
     *
     * <p>
     * On subsequent invocations, replays the outer context with the
     * new value, effectively re-executing the computation from where
     * call/cc was invoked.
     * </p>
     *
     * @param values
     *            the arguments (first argument is the return value)
     * @param env
     *            the current environment (not used for continuations)
     * @return the result of replaying the continuation
     * @throws JlllException
     *             thrown on first invocation to escape to call/cc
     */
    @Override
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        Object value = values.length() > 0 ? values.get(0) : Null.NULL;
        if (!captured)
        {
            // First invocation - throw to escape to call/cc
            throw new JlllException(this, value);
        }
        else
        {
            // Subsequent invocation - replay outer context
            return replay(value);
        }
    }

    /**
     * Returns the environment where call/cc was invoked.
     *
     * @return the capture environment
     */
    public Environment getCaptureEnv()
    {
        return captureEnv;
    }

    /**
     * Returns the outer expression.
     *
     * @return the outer expression, or null if none
     */
    public Object getOuterExpression()
    {
        return outerExpression;
    }

    /**
     * Replay the continuation with a new value.
     *
     * <p>
     * Reconstructs the outer expression with the new value substituted
     * at the position where call/cc was invoked.
     * </p>
     *
     * @param value
     *            the value to return from call/cc
     * @return the final result after replaying
     * @throws JlllException
     *             if replay fails
     */
    private Object replay(Object value) throws JlllException
    {
        if (outerExpression == null)
        {
            // No outer context = continuation was at top level
            return value;
        }
        // outerExpression is like (+ 1 (call/cc ...))
        // We need to substitute value at outerArgPosition and evaluate
        if (!(outerExpression instanceof Cons))
        {
            return value;
        }
        Cons expr = (Cons) outerExpression;
        if (expr.isNull())
        {
            return value;
        }
        // Get the procedure (first element)
        Object procObj = Evaluator.eval(expr.car(), captureEnv);
        if (!(procObj instanceof Procedure))
        {
            throw new JlllException("Continuation replay: expected procedure, got: "
                    + (procObj == null ? "null" : procObj.getClass().getName()));
        }
        Procedure proc = (Procedure) procObj;
        // Get argument expressions
        Object cdrObj = expr.cdr();
        if (cdrObj == null || !(cdrObj instanceof Cons))
        {
            // No arguments - just call with the value if position is 0
            if (outerArgPosition == 0)
            {
                return proc.applyEvaluated(new Cons(value), captureEnv);
            }
            return proc.applyEvaluated(Null.NULL, captureEnv);
        }
        Cons argExprs = (Cons) cdrObj;
        List<Object> args = new ArrayList<>();
        int pos = 0;
        for (Object argExpr : argExprs)
        {
            if (pos < outerArgPosition)
            {
                // Use pre-evaluated value if available
                if (outerEvaluatedArgs != null && pos < outerEvaluatedArgs.length)
                {
                    args.add(outerEvaluatedArgs[pos]);
                }
                else
                {
                    // Re-evaluate (fallback - shouldn't normally happen)
                    args.add(Evaluator.eval(argExpr, captureEnv));
                }
            }
            else if (pos == outerArgPosition)
            {
                // Substitute the continuation value
                args.add(value);
            }
            else
            {
                // Evaluate remaining arguments
                args.add(Evaluator.eval(argExpr, captureEnv));
            }
            pos++;
        }
        // Call the procedure with reconstructed arguments
        Cons argCons = ListUtil.arrayToCons(args.toArray());
        return proc.applyEvaluated(argCons, captureEnv);
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
        if (!captured)
        {
            return "Continuation (not yet invoked)";
        }
        return "Continuation (captured, outer: " + outerExpression + ")";
    }
}
