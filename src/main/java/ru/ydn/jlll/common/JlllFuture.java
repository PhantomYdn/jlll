package ru.ydn.jlll.common;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * A future representing an asynchronous computation in JLLL.
 *
 * <p>
 * Wraps a {@link CompletableFuture} to provide async execution of JLLL expressions.
 * Created by the {@code future} special form and queried with {@code deref} and {@code realized?}.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>
 * (define f (future (expensive-computation)))
 * (do-other-work)
 * (deref f)  ; blocks until result ready
 * </pre>
 *
 * <p>
 * <b>Thread safety:</b> Each future executes in a separate thread from the ForkJoinPool.
 * The computation receives a snapshot of the environment at creation time. Continuations
 * ({@code call/cc}) cannot cross thread boundaries.
 * </p>
 */
public class JlllFuture implements Serializable
{
    private static final long serialVersionUID = 1L;
    /**
     * The underlying CompletableFuture. Transient because CompletableFuture is not serializable.
     */
    private transient CompletableFuture<Object> future;
    /**
     * The result value, cached after completion for serialization.
     */
    private Object cachedResult;
    /**
     * The exception, cached after completion for serialization.
     */
    private Throwable cachedException;
    /**
     * Whether the future has completed (for serialization).
     */
    private boolean completed;

    /**
     * Creates a new JlllFuture wrapping the given CompletableFuture.
     *
     * @param future
     *            the CompletableFuture to wrap
     */
    public JlllFuture(CompletableFuture<Object> future)
    {
        this.future = future;
        this.completed = false;
    }

    /**
     * Gets the result, blocking until available.
     *
     * @return the computation result
     * @throws JlllException
     *             if the computation threw an exception
     */
    public Object get() throws JlllException
    {
        if (completed)
        {
            if (cachedException != null)
            {
                throw wrapException(cachedException);
            }
            return cachedResult;
        }
        try
        {
            Object result = future.get();
            cacheCompletion(result, null);
            return result;
        }
        catch (ExecutionException e)
        {
            cacheCompletion(null, e.getCause());
            throw wrapException(e.getCause());
        }
        catch (InterruptedException e)
        {
            Thread.currentThread().interrupt();
            throw new JlllException("Future interrupted", e);
        }
        catch (CancellationException e)
        {
            throw new JlllException("Future was cancelled", e);
        }
    }

    /**
     * Gets the result with a timeout.
     *
     * @param timeoutMs
     *            timeout in milliseconds
     * @param defaultValue
     *            value to return if timeout expires
     * @return the computation result, or defaultValue if timeout expires
     * @throws JlllException
     *             if the computation threw an exception
     */
    public Object get(long timeoutMs, Object defaultValue) throws JlllException
    {
        if (completed)
        {
            if (cachedException != null)
            {
                throw wrapException(cachedException);
            }
            return cachedResult;
        }
        try
        {
            Object result = future.get(timeoutMs, TimeUnit.MILLISECONDS);
            cacheCompletion(result, null);
            return result;
        }
        catch (TimeoutException e)
        {
            return defaultValue;
        }
        catch (ExecutionException e)
        {
            cacheCompletion(null, e.getCause());
            throw wrapException(e.getCause());
        }
        catch (InterruptedException e)
        {
            Thread.currentThread().interrupt();
            throw new JlllException("Future interrupted", e);
        }
        catch (CancellationException e)
        {
            throw new JlllException("Future was cancelled", e);
        }
    }

    /**
     * Checks if the computation has completed.
     *
     * @return true if completed (successfully or with exception)
     */
    public boolean isRealized()
    {
        if (completed)
        {
            return true;
        }
        if (future != null && future.isDone())
        {
            // Cache the result for serialization
            try
            {
                Object result = future.getNow(null);
                if (!future.isCompletedExceptionally())
                {
                    cacheCompletion(result, null);
                }
            }
            catch (Exception e)
            {
                // Exception case handled separately
            }
            return true;
        }
        return false;
    }

    /**
     * Caches the completion state for serialization.
     */
    private void cacheCompletion(Object result, Throwable exception)
    {
        this.cachedResult = result;
        this.cachedException = exception;
        this.completed = true;
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
        return new JlllException("Future computation failed: " + t.getMessage(), t);
    }

    /**
     * Returns a string representation of this future.
     *
     * @return "#&lt;future pending&gt;", "#&lt;future completed&gt;", or "#&lt;future failed&gt;"
     */
    @Override
    public String toString()
    {
        if (completed || (future != null && future.isDone()))
        {
            if (cachedException != null || (future != null && future.isCompletedExceptionally()))
            {
                return "#<future failed>";
            }
            return "#<future completed>";
        }
        return "#<future pending>";
    }

    /**
     * Custom serialization to handle transient CompletableFuture.
     */
    private void writeObject(ObjectOutputStream out) throws IOException
    {
        // Ensure completion state is cached before serialization
        if (!completed && future != null && future.isDone())
        {
            try
            {
                Object result = future.get();
                cacheCompletion(result, null);
            }
            catch (ExecutionException e)
            {
                cacheCompletion(null, e.getCause());
            }
            catch (InterruptedException e)
            {
                Thread.currentThread().interrupt();
            }
        }
        out.defaultWriteObject();
    }

    /**
     * Custom deserialization - future field will be null, but cached values are available.
     */
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
    {
        in.defaultReadObject();
        // future remains null after deserialization - use cached values
    }
}
