package ru.ydn.jlll.libs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.util.ListUtil;

/**
 * List manipulation primitives.
 *
 * <p>
 * Provides list utilities in addition to core list operations (cons, car, cdr):
 * </p>
 * <ul>
 * <li><b>list-&gt;vector:</b> converts a list to a Java array</li>
 * <li><b>collection-&gt;list:</b> converts a Java Collection to a Cons list</li>
 * </ul>
 *
 * <p>
 * Also loads list.jlll which provides additional functions like
 * list, append, reverse, length, nth, etc.
 * </p>
 */
public class ListLib implements Library
{
    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        new Primitive("list->vector", env,
                "Converts a Cons list to a Java Object array. (list->vector '(a b c)) returns an Object[].")
        {
            private static final long serialVersionUID = -4084286307884490319L;

            public Object apply(Cons vaCons, Environment env) throws JlllException
            {
                return ListUtil.listVector((Cons) vaCons.cdr());
            }
        };
        new Primitive("collection->list", env,
                "Converts a Java Collection to a Cons list. (collection->list java-list) returns a JLLL list.")
        {
            private static final long serialVersionUID = -6045559114098496174L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Collection<?> col = (Collection<?>) values.get(0);
                Cons ret = new Cons();
                for (Iterator<?> it = col.iterator(); it.hasNext();)
                {
                    Object o = it.next();
                    ListUtil.append(ret, o);
                }
                return ret;
            }
        };
        // sort - Sort list using comparator function
        new Primitive("sort", env, "Sorts a list using a comparator function. (sort '(3 1 4) <) => (1 3 4). "
                + "The comparator should return true if first arg should come before second.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Cons list = (Cons) values.get(0);
                Object comparator = values.get(1);
                List<Object> javaList = new ArrayList<>(ListUtil.consToList(list));
                Collections.sort(javaList, (a, b) ->
                {
                    try
                    {
                        Cons args = new Cons(a, new Cons(b, new Cons()));
                        Object result;
                        if (comparator instanceof Procedure)
                        {
                            result = ((Procedure) comparator).applyEvaluated(args, env);
                        }
                        else if (comparator instanceof Primitive)
                        {
                            result = ((Primitive) comparator).applyEvaluated(args, env);
                        }
                        else
                        {
                            throw new RuntimeException("sort: comparator must be a procedure");
                        }
                        if (Boolean.TRUE.equals(result))
                        {
                            return -1;
                        }
                        else if (Boolean.FALSE.equals(result))
                        {
                            return 1;
                        }
                        return 0;
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                });
                return ListUtil.listToCons(javaList);
            }
        };
        // sort-by - Sort list by key function
        new Primitive("sort-by", env, "Sorts a list by a key function. (sort-by car '((3 a) (1 b))) => ((1 b) (3 a)). "
                + "Extracts keys using key-fn and sorts by natural order.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object keyFn = values.get(0);
                Cons list = (Cons) values.get(1);
                List<Object> javaList = new ArrayList<>(ListUtil.consToList(list));
                Collections.sort(javaList, (a, b) ->
                {
                    try
                    {
                        Cons argsA = new Cons(a, new Cons());
                        Cons argsB = new Cons(b, new Cons());
                        Object keyA, keyB;
                        if (keyFn instanceof Procedure)
                        {
                            keyA = ((Procedure) keyFn).applyEvaluated(argsA, env);
                            keyB = ((Procedure) keyFn).applyEvaluated(argsB, env);
                        }
                        else if (keyFn instanceof Primitive)
                        {
                            keyA = ((Primitive) keyFn).applyEvaluated(argsA, env);
                            keyB = ((Primitive) keyFn).applyEvaluated(argsB, env);
                        }
                        else
                        {
                            throw new RuntimeException("sort-by: key function must be a procedure");
                        }
                        return ListUtil.GENERIC_COMPARATOR.compare(keyA, keyB);
                    }
                    catch (JlllException e)
                    {
                        throw new RuntimeException(e);
                    }
                });
                return ListUtil.listToCons(javaList);
            }
        };
        Jlll.eval("(load-system-script \"list.jlll\")", env);
    }
}
