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
     * Attempts to look up a qualified symbol (module/symbol).
     *
     * @param sym
     *            the symbol to look up
     * @return the value if found via qualified lookup, null otherwise
     */
    private static Object lookupQualified(Symbol sym)
    {
        String name = sym.getName();
        int slashIndex = name.indexOf('/');
        if (slashIndex > 0 && slashIndex < name.length() - 1)
        {
            String moduleName = name.substring(0, slashIndex);
            String symbolName = name.substring(slashIndex + 1);
            ModuleEnvironment module = Environment.getModule(moduleName);
            if (module != null)
            {
                Symbol localSym = Symbol.intern(symbolName);
                if (module.isExported(localSym))
                {
                    return module.lookup(localSym);
                }
            }
        }
        return null;
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
                        // Trace entry if enabled
                        int depth = 0;
                        if (TraceContext.isEnabled())
                        {
                            depth = env.getDepth();
                            TraceContext.traceEntry(depth, eval, env);
                        }
                        ret = proc.apply(cons.cdr(), env);
                        // Trace exit if enabled
                        if (TraceContext.isEnabled())
                        {
                            TraceContext.traceExit(depth, cons.car(), ret, env);
                        }
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
            Symbol sym = (Symbol) eval;
            ret = env.lookup(sym);
            if (ret == null)
            {
                // Try qualified lookup (module/symbol)
                ret = lookupQualified(sym);
                if (ret == null)
                {
                    throw new JlllException("Symbol is unbound: " + eval);
                }
            }
        }
        else
        {
            ret = eval;
        }
        return CommonUtil.prepareReturn(ret);
    }
}
