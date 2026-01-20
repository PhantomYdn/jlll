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
    public void load(Enviroment env) throws JlllException
    {
        super.load(env);
        new Primitive("+", env)
        {
            /**
             *
             */
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
        new Primitive("-", env)
        {
            /**
             *
             */
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
        new Primitive("*", env)
        {
            /**
             *
             */
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
        new Primitive("/", env)
        {
            /**
             *
             */
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
        new Primitive("and", env)
        {
            /**
             *
             */
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
        new Primitive("or", env)
        {
            /**
             *
             */
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
        new Primitive("max", env)
        {
            private static final long serialVersionUID = -4780145838015746273L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                return Collections.max(ListUtil.consToList(values), ListUtil.GENERIC_COMPARATOR);
            }
        };
        new Primitive("min", env)
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

    @JlllName("<")
    public boolean lessThen(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) < 0;
    }

    @JlllName(">")
    public boolean graterThen(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) > 0;
    }

    @JlllName("=")
    public boolean equalTo(Comparable<Comparable<?>> obj1, Comparable<?> obj2)
    {
        return obj1.compareTo(obj2) == 0;
    }

    @JlllName("between")
    public <K extends Comparable<K>> boolean between(K start, K finish, K obj)
    {
        return obj.compareTo(start) >= 0 && finish.compareTo(obj) >= 0;
    }

    @JlllName("not")
    public boolean not(Object obj)
    {
        return !CommonUtil.getBoolean(obj);
    }
}
