package ru.ydn.jlll.common;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * A delay (promise) representing a deferred computation with memoization.
 *
 * <p>
 * JlllDelay wraps an unevaluated expression and its captured environment. When {@link #force()}
 * is called, the expression is evaluated and the result is cached. Subsequent calls to
 * {@code force()} return the cached value without re-evaluation.
 * </p>
 *
 * <p>
 * This implements the classic Scheme {@code delay}/{@code force} mechanism for lazy evaluation:
 * </p>
 *
 * <pre>
 * (define p (delay (begin (println "computing...") 42)))
 * (force p)  ; prints "computing...", returns 42
 * (force p)  ; returns 42 (no recomputation)
 * </pre>
 *
 * <p>
 * <b>Difference from LazyThunk:</b> JlllDelay is user-facing and stores an unevaluated JLLL
 * expression with its environment. LazyThunk is internal and wraps a Java Supplier. Both
 * provide memoization and thread-safe forcing.
 * </p>
 *
 * <p>
 * <b>Thread safety:</b> Forcing is synchronized to ensure the expression is evaluated exactly
 * once, even when accessed from multiple threads simultaneously.
 * </p>
 *
 * @see LazyThunk
 */
public class JlllDelay implements Serializable
{
    private static final long serialVersionUID = 1L;
    /**
     * The unevaluated expression. Set to null after forcing to allow GC.
     */
    private transient Object expression;
    /**
     * The captured environment for evaluation. Transient and set to null after forcing.
     */
    private transient Environment capturedEnv;
    /**
     * The cached result after forcing.
     */
    private volatile Object cachedValue;
    /**
     * Whether the delay has been forced.
     */
    private volatile boolean realized;
    /**
     * Any exception that occurred during forcing, cached for re-throwing.
     */
    private volatile Throwable cachedException;

    /**
     * Creates a new JlllDelay with the given expression and environment.
     *
     * @param expression
     *            the unevaluated expression
     * @param env
     *            the environment to use for evaluation
     */
    public JlllDelay(Object expression, Environment env)
    {
        this.expression = expression;
        this.capturedEnv = env;
        this.realized = false;
    }

    /**
     * Forces the delay, evaluating the expression and caching the result.
     *
     * <p>
     * This method is thread-safe: the expression is evaluated exactly once even if called from
     * multiple threads. Subsequent calls return the cached result.
     * </p>
     *
     * @return the evaluated value
     * @throws JlllException
     *             if evaluation fails
     */
    public Object force() throws JlllException
    {
        if (realized)
        {
            if (cachedException != null)
            {
                throw wrapException(cachedException);
            }
            return cachedValue;
        }
        synchronized (this)
        {
            // Double-check after acquiring lock
            if (realized)
            {
                if (cachedException != null)
                {
                    throw wrapException(cachedException);
                }
                return cachedValue;
            }
            try
            {
                cachedValue = Evaluator.eval(expression, capturedEnv);
                // Allow GC of expression and environment
                expression = null;
                capturedEnv = null;
                realized = true;
                return cachedValue;
            }
            catch (Exception e)
            {
                cachedException = e;
                expression = null;
                capturedEnv = null;
                realized = true;
                throw wrapException(e);
            }
        }
    }

    /**
     * Checks if this delay has been forced.
     *
     * @return true if the delay has been forced (successfully or with exception)
     */
    public boolean isRealized()
    {
        return realized;
    }

    /**
     * Wraps a throwable in a JlllException if needed.
     */
    private JlllException wrapException(Throwable t)
    {
        if (t instanceof JlllException)
        {
            return (JlllException) t;
        }
        return new JlllException("Delay evaluation failed: " + t.getMessage(), t);
    }

    /**
     * Returns a string representation of this delay.
     *
     * @return "#&lt;delay pending&gt;" or "#&lt;delay realized&gt;"
     */
    @Override
    public String toString()
    {
        if (realized)
        {
            if (cachedException != null)
            {
                return "#<delay failed>";
            }
            return "#<delay realized>";
        }
        return "#<delay pending>";
    }

    /**
     * Custom serialization - forces the delay before serializing to capture the value.
     */
    private void writeObject(ObjectOutputStream out) throws IOException
    {
        // Force the delay before serialization
        if (!realized && expression != null && capturedEnv != null)
        {
            try
            {
                force();
            }
            catch (JlllException e)
            {
                // Exception is cached, will be serialized
            }
        }
        out.defaultWriteObject();
    }

    /**
     * Custom deserialization - expression and env remain null, but cached value is available.
     */
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
    {
        in.defaultReadObject();
        // expression and capturedEnv remain null - use cached value
    }
}
