package ru.ydn.jlll.common;

import ru.ydn.jlll.util.CommonUtil;

/**
 * Evaluator is the main class for evaluating of the equations.
 *
 * <p>
 * Supports continuation capture by tracking evaluation context (expression,
 * argument position, evaluated arguments) via thread-local EvalContext.
 * </p>
 */
public class Evaluator
{
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
        /** Parent context (for nested evaluations) */
        public EvalContext parent = null;

        /**
         * Creates a new evaluation context.
         *
         * @param expr
         *            the expression being evaluated
         */
        public EvalContext(Object expr)
        {
            this.expression = expr;
        }
    }

    /** Thread-local evaluation context for continuation support */
    private static final ThreadLocal<EvalContext> evalContext = new ThreadLocal<>();

    /**
     * Get current evaluation context (for use by Jlll.evalEvery).
     *
     * @return the current context, or null if none
     */
    public static EvalContext getEvalContext()
    {
        return evalContext.get();
    }

    /**
     * Main method to evaluate expressions.
     *
     * @param eval
     *            object to eval
     * @param env
     *            environment which is used during evaluation
     * @return result
     * @throws JlllException
     *             thrown if some unusual situation occurred
     */
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
                ctx.parent = prevCtx; // Link to parent context
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
                    // Add rich stack frame for ALL JlllExceptions (errors and continuations)
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
            {
                throw new JlllException("Symbol is unbound: " + eval);
            }
        }
        else
        {
            ret = eval;
        }
        return CommonUtil.prepareReturn(ret);
    }
}
