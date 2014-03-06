package jlll.libs;

import java.util.Collection;
import java.util.Iterator;

import jlll.common.Cons;
import jlll.common.Enviroment;
import jlll.common.Jlll;
import jlll.common.JlllException;
import jlll.common.Library;
import jlll.common.Primitive;
import jlll.util.ListUtil;

/**
 * Created by IntelliJ IDEA.
 * User: Eleas
 * Date: 18.05.2003
 * Time: 18:32:25
 * To change this template use Options | File Templates.
 */
public class ListLib implements Library
{
    public void load(Enviroment env) throws JlllException
    {
        new Primitive("list->vector", env)
        {
            /**
			 * 
			 */
			private static final long serialVersionUID = -4084286307884490319L;

			public Object applay(Cons vaCons, Enviroment env) throws JlllException
            {
                return ListUtil.listVector((Cons) vaCons.cdr());
            }
        };
        new Primitive("collection->list", env)
        {            
			private static final long serialVersionUID = -6045559114098496174L;

			public Object applayEvaluated(Cons values, Enviroment env) throws JlllException

            {
                Collection<?> col = (Collection<?>) values.get(0);
                Cons ret = new Cons();
                for (Iterator<?> it = col.iterator(); it.hasNext();)
                {
                    Object o = it.next();
                    ListUtil.append(ret,o);
                }
                return ret;
            }
        };

        Jlll.eval("(load-system-script \"list.jlll\")",env);

    }

}
