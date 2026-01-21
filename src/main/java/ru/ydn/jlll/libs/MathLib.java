package ru.ydn.jlll.libs;

import java.util.Collections;
import java.util.Iterator;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.CommonUtil;
import ru.ydn.jlll.util.ListUtil;

/**
 * Mathematical and logical primitives.
 *
 * <p>
 * Provides arithmetic and comparison operations:
 * </p>
 * <ul>
 * <li><b>Arithmetic:</b> +, -, *, /</li>
 * <li><b>Comparison:</b> &lt;, &gt;, =, between</li>
 * <li><b>Aggregation:</b> max, min</li>
 * <li><b>Logic:</b> and, or, not</li>
 * </ul>
 *
 * <p>
 * Arithmetic operations support both integers and floating-point numbers,
 * preserving integer type when all operands are integers.
 * </p>
 */
public class MathLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    public void load(Enviroment env) throws JlllException
    {
        super.load(env);
        new Primitive("+", env,
                "Addition. (+ a b ...) returns the sum of all arguments. Preserves integer type if all operands are integers.")
        {
            private static final long serialVersionUID = -2122014676433877891L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                boolean isInteger = values.get(0) instanceof Integer;
                double ret = 0.0;
                while (it.hasNext())
                {
                    Object val = it.next();
                    if (val instanceof Double)
                    {
                        ret += ((Double) val).doubleValue();
                    }
                    else if (val instanceof Integer)
                    {
                        ret += ((Integer) val).intValue();
                    }
                    else
                    {
                        throw new JlllException("Not a double: " + val.getClass().getName());
                    }
                }
                if (isInteger)
                {
                    return (int) ret;
                }
                else
                {
                    return ret;
                }
            }
        };
        new Primitive("-", env, "Subtraction. (- a b) returns a-b. (- a) returns -a (negation).")
        {
            private static final long serialVersionUID = -3793672312398700280L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Object first = values.get(0);
                Object second = values.get(1);
                double ret = 0;
                if (second == null)
                {
                    ret = -((Number) first).doubleValue();
                }
                else
                {
                    ret = ((Number) first).doubleValue() - ((Number) second).doubleValue();
                }
                Class<? extends Object> clzz = first.getClass();
                return CommonUtil.getNumber(ret, clzz);
            }
        };
        new Primitive("*", env,
                "Multiplication. (* a b ...) returns the product of all arguments. Preserves integer type if all operands are integers.")
        {
            private static final long serialVersionUID = 5494871817634832943L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                boolean isInteger = values.get(0) instanceof Integer;
                double ret = 1.0;
                while (it.hasNext())
                {
                    Object val = it.next();
                    if (val instanceof Double)
                    {
                        ret *= ((Double) val).doubleValue();
                    }
                    else if (val instanceof Integer)
                    {
                        ret *= ((Integer) val).intValue();
                    }
                    else
                    {
                        throw new JlllException("Not a double: " + val.getClass().getName());
                    }
                }
                if (isInteger)
                {
                    return (int) ret;
                }
                else
                {
                    return ret;
                }
            }
        };
        new Primitive("/", env, "Division. (/ a b) returns a/b. (/ a) returns 1/a (reciprocal).")
        {
            private static final long serialVersionUID = 5672648515824387253L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Object first = values.get(0);
                Object second = values.get(1);
                double ret = 0;
                if (second == null)
                {
                    ret = 1 / ((Number) first).doubleValue();
                }
                else
                {
                    ret = ((Number) first).doubleValue() / ((Number) second).doubleValue();
                }
                Class<? extends Object> clzz = first.getClass();
                return CommonUtil.getNumber(ret, clzz);
            }
        };
        /*
         * new Primitive("<", env)
         * {
         * public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
         * {
         * Comparable obj1 = (Comparable) values.get(0);
         * Comparable obj2 = (Comparable) values.get(1);
         * boolean ret = obj1.compareTo(obj2) < 0 ? true : false;
         * return new Boolean(ret);
         * }
         * };
         */
        /*
         * new Primitive(">", env)
         * {
         * public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
         * {
         * Comparable obj1 = (Comparable) values.get(0);
         * Comparable obj2 = (Comparable) values.get(1);
         * boolean ret = obj1.compareTo(obj2) > 0 ? true : false;
         * return new Boolean(ret);
         * }
         * };
         */
        /*
         * new Primitive("=", env)
         * {
         * public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
         * {
         * Comparable obj1 = (Comparable) values.get(0);
         * Comparable obj2 = (Comparable) values.get(1);
         * boolean ret = obj1.compareTo(obj2) == 0 ? true : false;
         * return new Boolean(ret);
         * }
         * };
         */
        new Primitive("and", env,
                "Logical and with short-circuit evaluation. Returns the first false value or the last value if all are true.")
        {
            private static final long serialVersionUID = -6620736129309146338L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                Object next = null;
                while (it.hasNext())
                {
                    next = Evaluator.eval(it.next(), env);
                    if (!CommonUtil.getBoolean(next))
                        return next;
                }
                return next == null ? Boolean.TRUE : next;
            }
        };
        new Primitive("or", env,
                "Logical or with short-circuit evaluation. Returns the first true value or the last value if all are false.")
        {
            private static final long serialVersionUID = -5676965755997109300L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                Object next = null;
                while (it.hasNext())
                {
                    next = Evaluator.eval(it.next(), env);
                    if (CommonUtil.getBoolean(next))
                        return next;
                }
                return next == null ? Boolean.FALSE : next;
            }
        };
        new Primitive("max", env,
                "Returns the maximum value. (max a b ...) compares all arguments and returns the largest.")
        {
            private static final long serialVersionUID = -4780145838015746273L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                return Collections.max(ListUtil.consToList(values), ListUtil.GENERIC_COMPARATOR);
            }
        };
        new Primitive("min", env,
                "Returns the minimum value. (min a b ...) compares all arguments and returns the smallest.")
        {
            private static final long serialVersionUID = -4062151075693391985L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                return Collections.min(ListUtil.consToList(values), ListUtil.GENERIC_COMPARATOR);
            }
        };
        /*
         * new Primitive("not", env)
         * {
         *
         * @Override
         * public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
         * {
         * return !CommonUtil.getBoolean(values.get(0));
         * }
         *
         * };
         */
        Jlll.eval("(load-system-script \"math.jlll\")", env);
    }

    /**
     * Less-than comparison. ({@code (< a b)}) returns true if a &lt; b.
     *
     * @param obj1
     *            first value
     * @param obj2
     *            second value
     * @return true if obj1 &lt; obj2
     */
    @JlllName("<")
    public boolean lessThen(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) < 0;
    }

    /**
     * Greater-than comparison. ({@code (> a b)}) returns true if a &gt; b.
     *
     * @param obj1
     *            first value
     * @param obj2
     *            second value
     * @return true if obj1 &gt; obj2
     */
    @JlllName(">")
    public boolean graterThen(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) > 0;
    }

    /**
     * Equality comparison. ({@code (= a b)}) returns true if a equals b.
     *
     * @param obj1
     *            first value
     * @param obj2
     *            second value
     * @return true if obj1 equals obj2
     */
    @JlllName("=")
    public boolean equalTo(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) == 0;
    }

    /**
     * Range check. ({@code (between start finish obj)}) returns true if start &lt;= obj &lt;= finish.
     *
     * @param <K>
     *            comparable type
     * @param start
     *            lower bound (inclusive)
     * @param finish
     *            upper bound (inclusive)
     * @param obj
     *            value to check
     * @return true if obj is within [start, finish]
     */
    @JlllName("between")
    public <K extends Comparable<K>> boolean between(K start, K finish, K obj)
    {
        return obj.compareTo(start) >= 0 && finish.compareTo(obj) >= 0;
    }

    /**
     * Logical negation. ({@code (not x)}) returns true if x is false/null, false otherwise.
     *
     * @param obj
     *            value to negate
     * @return logical negation of obj
     */
    @JlllName("not")
    public boolean not(Object obj)
    {
        return !CommonUtil.getBoolean(obj);
    }
}
