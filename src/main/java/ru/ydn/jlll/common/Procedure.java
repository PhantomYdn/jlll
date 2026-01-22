package ru.ydn.jlll.common;

import java.io.Serializable;
import ru.ydn.jlll.util.ListUtil;

/**
 * Abstract base class for all JLLL procedures.
 *
 * <p>
 * During evaluation of a procedure call, the {@code apply} method is invoked
 * with the supplied arguments. Subclasses implement either {@code apply} for
 * unevaluated arguments or {@code applyEvaluated} for pre-evaluated arguments.
 * </p>
 */
public abstract class Procedure implements Serializable
{
    private static final long serialVersionUID = -8521523401153038007L;
    /** Invocation counter for debugging and profiling. */
    public transient int cnt = 0;

    /**
     * Convenience method to apply with varargs. Wraps arguments in a Cons list.
     *
     * @param env
     *            the environment to evaluate in
     * @param args
     *            arguments for this procedure
     * @return result of application
     * @throws JlllException
     *             if an error occurs
     */
    public final Object apply(Environment env, Object... args) throws JlllException
    {
        return apply(ListUtil.arrayToCons(args), env);
    }

    /**
     * Applies the procedure to values (as Object). Validates and delegates to Cons version.
     *
     * @param values
     *            arguments as Object (must be Cons or Null)
     * @param env
     *            the environment to evaluate in
     * @return result of application
     * @throws JlllException
     *             if values is not a valid argument list
     */
    public Object apply(Object values, Environment env) throws JlllException
    {
        if (Null.NULL.equals(values))
            values = Null.NULL;//new Cons();
        if (!(values instanceof Cons))
            throw new JlllException("values not a Cons. Class of values: " + values.getClass().getName()
                    + " toString():" + values.toString());
        return apply((Cons) values, env);
    }

    /**
     * Returns evaluated value from supplied arguments
     *
     * @param values
     *            arguments for this procedure
     * @param env
     *            environment to evaluate in
     * @return result of evaluating
     * @throws JlllException
     *             when some error occured
     */
    public Object apply(Cons values, Environment env) throws JlllException
    {
        return applyEvaluated(Jlll.evalEvery(values, env), env);
    }

    /**
     * Convenience method to apply with pre-evaluated varargs.
     *
     * @param env
     *            the environment
     * @param objects
     *            pre-evaluated arguments
     * @return result of application
     * @throws JlllException
     *             if an error occurs
     */
    public final Object applyEvaluated(Environment env, Object... objects) throws JlllException
    {
        return applyEvaluated(Cons.list(objects), env);
    }

    /**
     * Applies the procedure to pre-evaluated arguments.
     * Override this for procedures that work with already-evaluated values.
     *
     * @param values
     *            pre-evaluated arguments as a Cons list
     * @param env
     *            the environment
     * @return result of application
     * @throws JlllException
     *             if procedure doesn't support evaluated args or error occurs
     */
    public Object applyEvaluated(Cons values, Environment env) throws JlllException
    {
        throw new JlllException("You try to use procedure that dosn't allow to apply already evaluated values");
    }

    /**
     * Obtain documentation for this procedure.
     *
     * <p>
     * Note: Documentation should primarily be stored as metadata on the binding
     * using the {@code :doc} key. This method is kept for backward compatibility.
     * Use {@code (doc 'name)} or {@code (meta 'name :doc)} to retrieve documentation.
     * </p>
     *
     * @return documentation for this procedure, or empty string if not available
     */
    public String getDoc()
    {
        return "";
    }

    /**
     * Returns description of this procedure
     *
     * @return description of this procedure
     */
    public abstract String describe();
}
