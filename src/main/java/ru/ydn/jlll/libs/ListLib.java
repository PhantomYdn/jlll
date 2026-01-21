package ru.ydn.jlll.libs;

import java.util.Collection;
import java.util.Iterator;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Primitive;
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
    public void load(Enviroment env) throws JlllException
    {
        new Primitive("list->vector", env,
                "Converts a Cons list to a Java Object array. (list->vector '(a b c)) returns an Object[].")
        {
            private static final long serialVersionUID = -4084286307884490319L;

            public Object applay(Cons vaCons, Enviroment env) throws JlllException
            {
                return ListUtil.listVector((Cons) vaCons.cdr());
            }
        };
        new Primitive("collection->list", env,
                "Converts a Java Collection to a Cons list. (collection->list java-list) returns a JLLL list.")
        {
            private static final long serialVersionUID = -6045559114098496174L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
        Jlll.eval("(load-system-script \"list.jlll\")", env);
    }
}
