package ru.ydn.jlll.libs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ForkJoinPool;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.JlllAtom;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.JlllFuture;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * Concurrency primitives for parallel execution.
 *
 * <p>
 * Provides futures for async computation and atoms for thread-safe mutable state.
 * </p>
 *
 * <h3>Futures</h3>
 * <ul>
 * <li><b>future:</b> Creates an async computation</li>
 * <li><b>deref:</b> Gets the value (blocks if not ready)</li>
 * <li><b>realized?:</b> Checks if computation completed</li>
 * <li><b>future?:</b> Tests if value is a future</li>
 * </ul>
 *
 * <h3>Atoms</h3>
 * <ul>
 * <li><b>atom:</b> Creates a thread-safe mutable reference</li>
 * <li><b>swap!:</b> Atomically updates with a function</li>
 * <li><b>reset!:</b> Atomically sets to a value</li>
 * <li><b>compare-and-set!:</b> CAS operation</li>
 * <li><b>atom?:</b> Tests if value is an atom (note: conflicts with existing atom? for non-pairs)</li>
 * </ul>
 *
 * <h3>Parallel Operations</h3>
 * <ul>
 * <li><b>pmap:</b> Parallel map over a list</li>
 * <li><b>pfor-each:</b> Parallel for-each for side effects</li>
 * <li><b>pcalls:</b> Execute multiple thunks in parallel</li>
 * </ul>
 */
public class ConcurrencyLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // future: Create async computation (special form - unevaluated)
        new Primitive("future", env,
                "Creates an asynchronous computation. (future expr) returns immediately with a future object. "
                        + "Use (deref f) to get the result (blocking if not ready).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.car() == null)
                {
                    throw new JlllException("future requires an expression");
                }
                Object expr = values.car();
                // Capture the current environment for the async execution
                Environment capturedEnv = env;
                CompletableFuture<Object> cf = CompletableFuture.supplyAsync(() ->
                {
                    try
                    {
                        return Evaluator.eval(expr, capturedEnv);
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                }, ForkJoinPool.commonPool());
                return new JlllFuture(cf);
            }
        };
        // deref: Get value from future or atom (with optional timeout)
        new Primitive("deref", env,
                "Gets the value from a future or atom. (deref f) blocks until the future completes. "
                        + "(deref f timeout default) returns default if timeout (in ms) expires. "
                        + "For atoms, returns the current value immediately.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object target = values.get(0);
                if (target instanceof JlllFuture)
                {
                    JlllFuture future = (JlllFuture) target;
                    if (values.length() >= 3)
                    {
                        long timeout = ((Number) values.get(1)).longValue();
                        Object defaultValue = values.get(2);
                        return future.get(timeout, defaultValue);
                    }
                    return future.get();
                }
                else if (target instanceof JlllAtom)
                {
                    return ((JlllAtom) target).deref();
                }
                else
                {
                    throw new JlllException("deref: expected future or atom, got " + target);
                }
            }
        };
        // swap!: Atomically update atom with function
        new Primitive("swap!", env,
                "Atomically updates an atom by applying a function to its current value. "
                        + "(swap! a f) calls (f current-value) and sets the result. "
                        + "The function may be called multiple times if there is contention.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                JlllAtom atom = (JlllAtom) values.get(0);
                Procedure fn = (Procedure) values.get(1);
                return atom.swap(fn, env);
            }
        };
        // pmap: Parallel map
        new Primitive("pmap", env,
                "Parallel map - applies function to each element in parallel. "
                        + "(pmap f '(1 2 3 4)) returns a list of results. "
                        + "Order is preserved but execution order is not guaranteed.")
        {
            private static final long serialVersionUID = 4L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure fn = (Procedure) values.get(0);
                Cons list = (Cons) values.get(1);
                if (list == null || list.car() == null)
                {
                    return new Cons(null, null);
                }
                List<CompletableFuture<Object>> futures = new ArrayList<>();
                Iterator<?> it = list.iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    Environment capturedEnv = env;
                    futures.add(CompletableFuture.supplyAsync(() ->
                    {
                        try
                        {
                            return fn.applyEvaluated(capturedEnv, item);
                        }
                        catch (JlllException e)
                        {
                            throw new RuntimeException(e);
                        }
                    }, ForkJoinPool.commonPool()));
                }
                // Collect results in order
                List<Object> results = new ArrayList<>();
                for (CompletableFuture<Object> future : futures)
                {
                    try
                    {
                        results.add(future.join());
                    }
                    catch (Exception e)
                    {
                        Throwable cause = e.getCause();
                        if (cause instanceof JlllException)
                        {
                            throw (JlllException) cause;
                        }
                        throw new JlllException("pmap: computation failed", e);
                    }
                }
                return ListUtil.arrayToCons(results.toArray());
            }
        };
        // pfor-each: Parallel for-each
        new Primitive("pfor-each", env,
                "Parallel for-each - applies function to each element for side effects. "
                        + "(pfor-each f '(1 2 3 4)) executes in parallel, returns null. "
                        + "Execution order is not guaranteed.")
        {
            private static final long serialVersionUID = 5L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure fn = (Procedure) values.get(0);
                Cons list = (Cons) values.get(1);
                if (list == null || list.car() == null)
                {
                    return null;
                }
                List<CompletableFuture<Void>> futures = new ArrayList<>();
                Iterator<?> it = list.iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    Environment capturedEnv = env;
                    futures.add(CompletableFuture.runAsync(() ->
                    {
                        try
                        {
                            fn.applyEvaluated(capturedEnv, item);
                        }
                        catch (JlllException e)
                        {
                            throw new RuntimeException(e);
                        }
                    }, ForkJoinPool.commonPool()));
                }
                // Wait for all to complete
                for (CompletableFuture<Void> future : futures)
                {
                    try
                    {
                        future.join();
                    }
                    catch (Exception e)
                    {
                        Throwable cause = e.getCause();
                        if (cause instanceof JlllException)
                        {
                            throw (JlllException) cause;
                        }
                        throw new JlllException("pfor-each: computation failed", e);
                    }
                }
                return null;
            }
        };
        // pcalls: Execute multiple thunks in parallel
        new Primitive("pcalls", env, "Executes multiple thunks (zero-arg functions) in parallel. "
                + "(pcalls f1 f2 f3) returns a list of results in argument order.")
        {
            private static final long serialVersionUID = 6L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.car() == null)
                {
                    return new Cons(null, null);
                }
                List<CompletableFuture<Object>> futures = new ArrayList<>();
                Iterator<?> it = values.iterator();
                while (it.hasNext())
                {
                    Procedure thunk = (Procedure) it.next();
                    Environment capturedEnv = env;
                    futures.add(CompletableFuture.supplyAsync(() ->
                    {
                        try
                        {
                            return thunk.applyEvaluated(capturedEnv);
                        }
                        catch (JlllException e)
                        {
                            throw new RuntimeException(e);
                        }
                    }, ForkJoinPool.commonPool()));
                }
                // Collect results in order
                List<Object> results = new ArrayList<>();
                for (CompletableFuture<Object> future : futures)
                {
                    try
                    {
                        results.add(future.join());
                    }
                    catch (Exception e)
                    {
                        Throwable cause = e.getCause();
                        if (cause instanceof JlllException)
                        {
                            throw (JlllException) cause;
                        }
                        throw new JlllException("pcalls: computation failed", e);
                    }
                }
                return ListUtil.arrayToCons(results.toArray());
            }
        };
    }
    // ========== Simple methods using @JlllName ==========

    /**
     * Creates a new atom with the given initial value.
     * ({@code (atom 0)}) returns a new atom holding 0.
     *
     * @param initialValue
     *            the initial value
     * @return a new JlllAtom
     */
    @JlllName("atom")
    public JlllAtom createAtom(Object initialValue)
    {
        return new JlllAtom(initialValue);
    }

    /**
     * Atomically sets the atom to a new value.
     * ({@code (reset! a 42)}) sets the atom to 42 and returns 42.
     *
     * @param atom
     *            the atom
     * @param newValue
     *            the new value
     * @return the new value
     */
    @JlllName("reset!")
    public Object resetAtom(JlllAtom atom, Object newValue)
    {
        return atom.reset(newValue);
    }

    /**
     * Atomically sets the atom if it equals the expected value.
     * ({@code (compare-and-set! a expected new)}) returns true if successful.
     *
     * @param atom
     *            the atom
     * @param expected
     *            the expected current value
     * @param newValue
     *            the new value to set
     * @return true if the value was expected and is now newValue
     */
    @JlllName("compare-and-set!")
    public Boolean compareAndSet(JlllAtom atom, Object expected, Object newValue)
    {
        return atom.compareAndSet(expected, newValue);
    }

    /**
     * Tests if a value is a future.
     * ({@code (future? f)}) returns true if f is a JlllFuture.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a JlllFuture
     */
    @JlllName("future?")
    public Boolean isFuture(Object obj)
    {
        return obj instanceof JlllFuture;
    }

    /**
     * Tests if a value is a JLLL atom (thread-safe mutable reference).
     * ({@code (jlll-atom? a)}) returns true if a is a JlllAtom.
     *
     * <p>
     * Note: Named jlll-atom? to avoid conflict with the existing atom? predicate
     * which tests if a value is not a pair.
     * </p>
     *
     * @param obj
     *            the object to test
     * @return true if obj is a JlllAtom
     */
    @JlllName("jlll-atom?")
    public Boolean isJlllAtom(Object obj)
    {
        return obj instanceof JlllAtom;
    }

    /**
     * Tests if a future or atom has completed/is ready.
     * ({@code (realized? f)}) returns true if the future has completed.
     * For atoms, always returns true since they always have a value.
     *
     * @param obj
     *            the future or atom to test
     * @return true if the computation has completed
     * @throws JlllException
     *             if obj is neither a future nor an atom
     */
    @JlllName("realized?")
    public Boolean isRealized(Object obj) throws JlllException
    {
        if (obj instanceof JlllFuture)
        {
            return ((JlllFuture) obj).isRealized();
        }
        else if (obj instanceof JlllAtom)
        {
            return true; // Atoms always have a value
        }
        throw new JlllException("realized?: expected future or atom, got " + obj);
    }
}
