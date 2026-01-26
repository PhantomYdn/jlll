package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllDelay;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.LazyThunk;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Lazy evaluation primitives for JLLL.
 *
 * <p>
 * Provides lazy sequences and deferred computation through thunks. Lazy sequences allow
 * working with potentially infinite data structures by computing elements on demand.
 * </p>
 *
 * <h3>Basic Lazy Evaluation</h3>
 * <ul>
 * <li><b>delay:</b> Creates a delayed computation (promise)</li>
 * <li><b>force:</b> Forces evaluation of a delay or thunk</li>
 * <li><b>delay?:</b> Tests if value is a delay</li>
 * </ul>
 *
 * <h3>Lazy Sequences</h3>
 * <ul>
 * <li><b>lazy-cons:</b> Creates a cons with lazy cdr</li>
 * <li><b>lazy-seq?:</b> Tests if a cons has a lazy (unrealized) cdr</li>
 * <li><b>realize:</b> Forces entire lazy sequence to a regular list</li>
 * </ul>
 *
 * <h3>Lazy Generators</h3>
 * <ul>
 * <li><b>lazy-range:</b> Lazy numeric range (possibly infinite)</li>
 * <li><b>iterate:</b> Repeated function application</li>
 * <li><b>cycle:</b> Infinite repetition of a sequence</li>
 * <li><b>repeat:</b> Infinite copies of a value</li>
 * </ul>
 *
 * <h3>Lazy Transformations</h3>
 * <ul>
 * <li><b>lazy-map:</b> Lazy map over sequence</li>
 * <li><b>lazy-filter:</b> Lazy filter of sequence</li>
 * </ul>
 *
 * <p>
 * Note: The existing {@code take}, {@code drop}, {@code take-while}, {@code drop-while}
 * functions work transparently with lazy sequences due to automatic thunk forcing in
 * {@code Cons.cdr()}.
 * </p>
 */
public class LazyLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // ========== delay: Create delayed computation (special form) ==========
        new Primitive("delay", env, "Creates a delayed computation (promise). The expression is not evaluated until "
                + "(force p) is called. Result is cached - subsequent forces return same value.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("delay requires an expression");
                }
                // Capture expression unevaluated
                Object expr = values.car();
                return new JlllDelay(expr, env);
            }
        };
        // ========== lazy-cons: Create cons with lazy cdr (special form) ==========
        new Primitive("lazy-cons", env,
                "Creates a cons cell where the cdr is lazily evaluated. "
                        + "(lazy-cons head tail-expr) evaluates head immediately, delays tail-expr. "
                        + "The tail is forced when cdr is accessed.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.length() < 2)
                {
                    throw new JlllException("lazy-cons requires head and tail expressions");
                }
                // Evaluate head immediately
                Object head = Evaluator.eval(values.car(), env);
                // Capture tail expression as a lazy thunk
                Object tailExpr = values.cadr();
                Environment capturedEnv = env;
                LazyThunk lazyTail = new LazyThunk(() ->
                {
                    try
                    {
                        return Evaluator.eval(tailExpr, capturedEnv);
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                });
                return new Cons(head, lazyTail);
            }
        };
        // ========== lazy-range: Lazy numeric sequence ==========
        new Primitive("lazy-range", env, "Creates a lazy numeric range. (lazy-range start) is infinite from start. "
                + "(lazy-range start end) is finite. (lazy-range start end step) with step.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                int argc = values.length();
                if (argc == 0)
                {
                    throw new JlllException("lazy-range requires at least one argument");
                }
                long start = ((Number) values.get(0)).longValue();
                Long end = null;
                long step = 1L;
                if (argc >= 2)
                {
                    end = ((Number) values.get(1)).longValue();
                }
                if (argc >= 3)
                {
                    step = ((Number) values.get(2)).longValue();
                }
                return makeLazyRange(start, end, step);
            }

            private Object makeLazyRange(long current, Long end, long step)
            {
                // Check termination for finite ranges
                if (end != null)
                {
                    if (step > 0 && current >= end)
                    {
                        return Null.NULL;
                    }
                    if (step < 0 && current <= end)
                    {
                        return Null.NULL;
                    }
                }
                // Create thunk for the rest of the sequence
                LazyThunk nextThunk = new LazyThunk(() -> makeLazyRange(current + step, end, step));
                // Return Integer if in range, otherwise Long (consistent with JLLL numeric handling)
                // Note: Using if/else instead of ternary to avoid Java's numeric type widening
                Number value;
                if (current >= Integer.MIN_VALUE && current <= Integer.MAX_VALUE)
                {
                    value = Integer.valueOf((int) current);
                }
                else
                {
                    value = Long.valueOf(current);
                }
                return new Cons(value, nextThunk);
            }
        };
        // ========== iterate: Repeated function application ==========
        new Primitive("iterate", env, "Creates an infinite lazy sequence by repeatedly applying f. "
                + "(iterate f x) produces x, (f x), (f (f x)), ...")
        {
            private static final long serialVersionUID = 4L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure fn = (Procedure) values.get(0);
                Object seed = values.get(1);
                return makeIterate(fn, seed, env);
            }

            private Object makeIterate(Procedure fn, Object current, Environment env)
            {
                LazyThunk nextThunk = new LazyThunk(() ->
                {
                    try
                    {
                        Object next = fn.applyEvaluated(env, current);
                        return makeIterate(fn, next, env);
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                });
                return new Cons(current, nextThunk);
            }
        };
        // ========== cycle: Infinite repetition of sequence ==========
        new Primitive("cycle", env, "Creates an infinite lazy sequence by cycling through the given list. "
                + "(cycle '(a b c)) produces a, b, c, a, b, c, ...")
        {
            private static final long serialVersionUID = 5L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Cons list = (Cons) values.get(0);
                if (list == null || list.isNull())
                {
                    return Null.NULL;
                }
                return makeCycle(list, list);
            }

            private Object makeCycle(Cons current, Cons original)
            {
                if (current == null || current.isNull())
                {
                    // Restart from beginning
                    return makeCycle(original, original);
                }
                LazyThunk nextThunk = new LazyThunk(() ->
                {
                    Object nextCdr = current.cdr();
                    Cons next = (nextCdr instanceof Cons) ? (Cons) nextCdr : null;
                    return makeCycle(next, original);
                });
                return new Cons(current.car(), nextThunk);
            }
        };
        // ========== repeat: Infinite copies of value ==========
        new Primitive("repeat", env,
                "Creates an infinite lazy sequence of the same value. " + "(repeat x) produces x, x, x, ...")
        {
            private static final long serialVersionUID = 6L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object value = values.get(0);
                return makeRepeat(value);
            }

            private Object makeRepeat(Object value)
            {
                LazyThunk nextThunk = new LazyThunk(() -> makeRepeat(value));
                return new Cons(value, nextThunk);
            }
        };
        // ========== lazy-map: Lazy map transformation ==========
        new Primitive("lazy-map", env, "Lazily maps a function over a sequence. "
                + "(lazy-map f seq) returns a lazy sequence of (f elem) for each element.")
        {
            private static final long serialVersionUID = 7L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure fn = (Procedure) values.get(0);
                Object seq = values.get(1);
                return makeLazyMap(fn, seq, env);
            }

            private Object makeLazyMap(Procedure fn, Object seq, Environment env) throws JlllException
            {
                // Handle end of sequence
                if (seq == null || Null.NULL.equals(seq))
                {
                    return Null.NULL;
                }
                if (!(seq instanceof Cons))
                {
                    throw new JlllException("lazy-map: expected sequence, got " + seq);
                }
                Cons cons = (Cons) seq;
                if (cons.isNull())
                {
                    return Null.NULL;
                }
                // Apply function to head
                Object mappedHead = fn.applyEvaluated(env, cons.car());
                // Create lazy tail
                LazyThunk lazyTail = new LazyThunk(() ->
                {
                    try
                    {
                        return makeLazyMap(fn, cons.cdr(), env);
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                });
                return new Cons(mappedHead, lazyTail);
            }
        };
        // ========== lazy-filter: Lazy filter transformation ==========
        new Primitive("lazy-filter", env, "Lazily filters a sequence by a predicate. "
                + "(lazy-filter pred seq) returns elements where (pred elem) is true.")
        {
            private static final long serialVersionUID = 8L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure pred = (Procedure) values.get(0);
                Object seq = values.get(1);
                return makeLazyFilter(pred, seq, env);
            }

            private Object makeLazyFilter(Procedure pred, Object seq, Environment env) throws JlllException
            {
                // Scan for next matching element
                while (seq != null && !Null.NULL.equals(seq) && seq instanceof Cons)
                {
                    Cons cons = (Cons) seq;
                    if (cons.isNull())
                    {
                        return Null.NULL;
                    }
                    Object elem = cons.car();
                    Object result = pred.applyEvaluated(env, elem);
                    if (isTrue(result))
                    {
                        // Found matching element - return it with lazy rest
                        Object rest = cons.cdr();
                        LazyThunk lazyTail = new LazyThunk(() ->
                        {
                            try
                            {
                                return makeLazyFilter(pred, rest, env);
                            }
                            catch (JlllException e)
                            {
                                throw new RuntimeException(e);
                            }
                        });
                        return new Cons(elem, lazyTail);
                    }
                    // Not matching, continue to next
                    seq = cons.cdr();
                }
                return Null.NULL;
            }

            private boolean isTrue(Object val)
            {
                if (val == null)
                    return false;
                if (val instanceof Boolean)
                    return (Boolean) val;
                return true;
            }
        };
        // ========== realize: Force entire lazy sequence ==========
        new Primitive("realize", env,
                "Forces an entire lazy sequence to a regular (eager) list. "
                        + "WARNING: Will not terminate on infinite sequences! "
                        + "Use (take seq n) first to limit infinite sequences.")
        {
            private static final long serialVersionUID = 13L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object seq = values.get(0);
                return realizeSeq(seq, 0, 1000000); // Safety limit
            }

            private Object realizeSeq(Object seq, int count, int limit) throws JlllException
            {
                if (count > limit)
                {
                    throw new JlllException("realize: sequence too long (over " + limit + " elements). "
                            + "Use (take seq n) to limit infinite sequences.");
                }
                if (seq == null || Null.NULL.equals(seq))
                {
                    return Null.NULL;
                }
                if (!(seq instanceof Cons))
                {
                    throw new JlllException("realize: expected sequence, got " + seq);
                }
                Cons cons = (Cons) seq;
                if (cons.isNull())
                {
                    return Null.NULL;
                }
                Object head = cons.car();
                Object tail = cons.cdr(); // Forces any lazy thunk
                Object realizedTail = realizeSeq(tail, count + 1, limit);
                return new Cons(head, realizedTail);
            }
        };
        // Load the JLLL helper functions
        Jlll.eval("(load-system-script \"lazy.jlll\")", env);
    }
    // ========== Simple predicates using @JlllName ==========

    /**
     * Forces evaluation of a delay.
     * ({@code (force p)}) evaluates the delayed expression and returns/caches the result.
     *
     * @param obj
     *            the delay to force
     * @return the forced value
     * @throws JlllException
     *             if not a delay or forcing fails
     */
    @JlllName("force")
    public Object force(Object obj) throws JlllException
    {
        if (obj instanceof JlllDelay)
        {
            return ((JlllDelay) obj).force();
        }
        if (obj instanceof LazyThunk)
        {
            return ((LazyThunk) obj).force();
        }
        throw new JlllException("force: expected delay, got " + obj);
    }

    /**
     * Tests if a value is a delay.
     * ({@code (delay? p)}) returns true if p is a JlllDelay.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a JlllDelay
     */
    @JlllName("delay?")
    public Boolean isDelay(Object obj)
    {
        return obj instanceof JlllDelay;
    }

    /**
     * Tests if a value is a lazy thunk.
     * ({@code (thunk? p)}) returns true if p is a LazyThunk.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a LazyThunk
     */
    @JlllName("thunk?")
    public Boolean isThunk(Object obj)
    {
        return obj instanceof LazyThunk;
    }

    /**
     * Tests if a cons cell has a lazy (unrealized) cdr.
     * ({@code (lazy-seq? seq)}) returns true if seq has an unrealized lazy tail.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a Cons with unrealized lazy cdr
     */
    @JlllName("lazy-seq?")
    public Boolean isLazySeq(Object obj)
    {
        if (obj instanceof Cons)
        {
            return ((Cons) obj).hasLazyCdr();
        }
        return false;
    }

    /**
     * Tests if a cons cell has any lazy elements (realized or not).
     * ({@code (has-lazy? seq)}) returns true if the cdr is or was a LazyThunk.
     *
     * @param obj
     *            the object to test
     * @return true if the Cons cdr is a LazyThunk (realized or not)
     */
    @JlllName("has-lazy?")
    public Boolean hasLazy(Object obj)
    {
        if (obj instanceof Cons)
        {
            Object cdr = ((Cons) obj).cdrRaw();
            return cdr instanceof LazyThunk;
        }
        return false;
    }
}
