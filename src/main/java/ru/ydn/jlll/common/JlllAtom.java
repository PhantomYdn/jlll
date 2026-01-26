package ru.ydn.jlll.common;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A thread-safe mutable reference for JLLL (Clojure-style atom).
 *
 * <p>
 * Atoms provide a way to manage shared, synchronous, independent state.
 * Updates are atomic using compare-and-swap semantics.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>
 * (define counter (atom 0))
 * (swap! counter (lambda (x) (+ x 1)))  ; atomically increment
 * (deref counter)                       ; => 1
 * (reset! counter 0)                    ; => 0
 * </pre>
 *
 * <p>
 * <b>Thread safety:</b> All operations are thread-safe. {@code swap!} uses
 * compare-and-swap in a spin loop, retrying if another thread modified
 * the value concurrently.
 * </p>
 */
public class JlllAtom implements Serializable
{
    private static final long serialVersionUID = 1L;
    /**
     * The underlying atomic reference.
     */
    private transient AtomicReference<Object> ref;
    /**
     * Cached value for serialization.
     */
    private Object cachedValue;

    /**
     * Creates a new atom with the given initial value.
     *
     * @param initialValue
     *            the initial value
     */
    public JlllAtom(Object initialValue)
    {
        this.ref = new AtomicReference<>(initialValue);
        this.cachedValue = initialValue;
    }

    /**
     * Gets the current value of the atom.
     *
     * @return the current value
     */
    public Object deref()
    {
        return ref.get();
    }

    /**
     * Atomically sets the value to the given value.
     *
     * @param newValue
     *            the new value
     * @return the new value
     */
    public Object reset(Object newValue)
    {
        ref.set(newValue);
        cachedValue = newValue;
        return newValue;
    }

    /**
     * Atomically updates the value by applying a function.
     *
     * <p>
     * The function may be called multiple times if there is contention,
     * so it should be side-effect free.
     * </p>
     *
     * @param fn
     *            the update function (takes old value, returns new value)
     * @param env
     *            the environment for function evaluation
     * @return the new value
     * @throws JlllException
     *             if the function throws
     */
    public Object swap(Procedure fn, Environment env) throws JlllException
    {
        while (true)
        {
            Object oldValue = ref.get();
            Object newValue = fn.applyEvaluated(Cons.list(oldValue), env);
            if (ref.compareAndSet(oldValue, newValue))
            {
                cachedValue = newValue;
                return newValue;
            }
            // CAS failed, retry with new value
        }
    }

    /**
     * Atomically sets the value if it equals the expected value.
     *
     * @param expected
     *            the expected current value
     * @param newValue
     *            the new value to set
     * @return true if successful (value was expected and is now newValue)
     */
    public boolean compareAndSet(Object expected, Object newValue)
    {
        // Use equals for value comparison (not reference equality)
        while (true)
        {
            Object current = ref.get();
            if (!equals(current, expected))
            {
                return false;
            }
            if (ref.compareAndSet(current, newValue))
            {
                cachedValue = newValue;
                return true;
            }
            // CAS failed due to concurrent modification, retry comparison
        }
    }

    /**
     * Compares two values for equality, handling nulls.
     */
    private static boolean equals(Object a, Object b)
    {
        if (a == b)
        {
            return true;
        }
        if (a == null || b == null)
        {
            return false;
        }
        return a.equals(b);
    }

    /**
     * Returns a string representation of this atom.
     *
     * @return "#&lt;atom value&gt;"
     */
    @Override
    public String toString()
    {
        return "#<atom " + ref.get() + ">";
    }

    /**
     * Custom serialization.
     */
    private void writeObject(ObjectOutputStream out) throws IOException
    {
        cachedValue = ref.get();
        out.defaultWriteObject();
    }

    /**
     * Custom deserialization.
     */
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
    {
        in.defaultReadObject();
        ref = new AtomicReference<>(cachedValue);
    }
}
