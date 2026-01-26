package ru.ydn.jlll.common;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.function.Supplier;

/**
 * A lazy thunk representing a deferred computation that produces a value (typically a Cons cell).
 *
 * <p>
 * LazyThunk is the foundation for lazy sequences in JLLL. When placed as the cdr of a Cons cell,
 * it enables on-demand evaluation of sequence elements. The thunk is automatically forced when
 * {@link Cons#cdr()} is called, and the result is cached for subsequent accesses.
 * </p>
 *
 * <p>
 * Example of how lazy sequences work internally:
 * </p>
 *
 * <pre>
 * ;; (lazy-range 0) creates:
 * ;; Cons(0, LazyThunk) where LazyThunk produces Cons(1, LazyThunk) which produces Cons(2, ...)
 *
 * (define naturals (lazy-range 0))
 * (car naturals)        ; => 0 (immediate)
 * (car (cdr naturals))  ; => 1 (forces first thunk, caches result)
 * (car (cdr naturals))  ; => 1 (returns cached Cons)
 * </pre>
 *
 * <p>
 * <b>Thread safety:</b> Forcing is synchronized to ensure the computation runs exactly once,
 * even when accessed from multiple threads simultaneously.
 * </p>
 *
 * <p>
 * <b>Serialization:</b> Only realized thunks can be properly serialized. Unrealized thunks
 * will force evaluation during serialization to capture the result.
 * </p>
 *
 * @see Cons#cdr()
 * @see JlllDelay
 */
public class LazyThunk implements Serializable
{
    private static final long serialVersionUID = 1L;
    /**
     * The supplier that produces the value. Transient because Supplier is not serializable.
     * Set to null after forcing to allow garbage collection.
     */
    private transient Supplier<Object> supplier;
    /**
     * The cached result after forcing.
     */
    private volatile Object realized;
    /**
     * Whether the thunk has been forced.
     */
    private volatile boolean forced;
    /**
     * Any exception that occurred during forcing, cached for re-throwing.
     */
    private volatile Throwable cachedException;

    /**
     * Creates a new LazyThunk with the given supplier.
     *
     * @param supplier
     *            the supplier that produces the value when forced
     */
    public LazyThunk(Supplier<Object> supplier)
    {
        this.supplier = supplier;
        this.forced = false;
    }

    /**
     * Forces the thunk, computing and caching the result.
     *
     * <p>
     * This method is thread-safe: the computation runs exactly once even if called from multiple
     * threads. Subsequent calls return the cached result.
     * </p>
     *
     * @return the computed value (typically a Cons cell or Null.NULL)
     * @throws JlllException
     *             if the computation fails
     */
    public Object force() throws JlllException
    {
        if (forced)
        {
            if (cachedException != null)
            {
                throw wrapException(cachedException);
            }
            return realized;
        }
        synchronized (this)
        {
            // Double-check after acquiring lock
            if (forced)
            {
                if (cachedException != null)
                {
                    throw wrapException(cachedException);
                }
                return realized;
            }
            try
            {
                realized = supplier.get();
                supplier = null; // Allow GC of closure
                forced = true;
                return realized;
            }
            catch (Exception e)
            {
                cachedException = e;
                supplier = null;
                forced = true;
                throw wrapException(e);
            }
        }
    }

    /**
     * Checks if this thunk has been forced.
     *
     * @return true if the thunk has been forced (successfully or with exception)
     */
    public boolean isRealized()
    {
        return forced;
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
        if (t instanceof RuntimeException && t.getCause() instanceof JlllException)
        {
            return (JlllException) t.getCause();
        }
        return new JlllException("Lazy evaluation failed: " + t.getMessage(), t);
    }

    /**
     * Returns a string representation of this thunk.
     *
     * @return "#&lt;thunk pending&gt;" or "#&lt;thunk realized&gt;"
     */
    @Override
    public String toString()
    {
        if (forced)
        {
            if (cachedException != null)
            {
                return "#<thunk failed>";
            }
            return "#<thunk realized: " + realized + ">";
        }
        return "#<thunk pending>";
    }

    /**
     * Custom serialization - forces the thunk before serializing to capture the value.
     */
    private void writeObject(ObjectOutputStream out) throws IOException
    {
        // Force the thunk before serialization
        if (!forced && supplier != null)
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
     * Custom deserialization - supplier remains null, but realized value is available.
     */
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
    {
        in.defaultReadObject();
        // supplier remains null after deserialization - use cached realized value
    }
}
