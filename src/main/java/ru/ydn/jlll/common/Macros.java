package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.util.ListUtil;

/**
 * Created by IntelliJ IDEA.
 * User: Eleas
 * Date: 18.05.2003
 * Time: 1:44:23
 * To change this template use Options | File Templates.
 */
public class Macros extends CompaundProcedure
{
    private static final long serialVersionUID = 6419680788279882844L;

    public Macros(Object variables, Object body)
    {
        super(variables, body);
    }

    public Object applay(Cons values, Enviroment env) throws JlllException
    {
        Object eval = macroExpand(values, env);
        //        Logger.getLogger(Macros.class).debug("Macroexpand: "+eval );
        return Evaluator.eval(eval, env);
    }

    private Cons quoteAll(Cons values) throws JlllException
    {
        Iterator<?> it = values.iterator();
        List<Object> ret = new ArrayList<Object>(values.length());
        while (it.hasNext())
        {
            ret.add(ListUtil.list(Symbol.QUOTE, it.next()));
        }
        return ListUtil.arrayToCons(ret.toArray());
    }

    public Object macroExpand(Cons values, Enviroment env) throws JlllException
    {
        return super.applay(quoteAll(values), env);
    }

    public String describe()
    {
        return "Macros.\n" + "Doc: " + getDoc() + "\n" + "Arguments: " + variables + "\n" + "Body: " + body;
    }
}
