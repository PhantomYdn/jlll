package ru.ydn.jlll.common;

import java.util.Iterator;

/**
 * Environment for procedure execution with bound arguments.
 * Created when a procedure is called to bind parameter names to argument values.
 */
public class ProcEnvironment extends Enviroment
{
    private static final long serialVersionUID = -8061750297696026162L;

    /**
     *
     * @param vars
     *            list of names of aruments
     * @param values
     *            already evaluated values
     * @param env
     *            environment to evaluate in
     * @throws JlllException
     */
    public ProcEnvironment(Object vars, Cons values, Enviroment env) throws JlllException
    {
        super(env);
        subst(vars, values);
    }

    private void subst(Object vars, Cons values) throws JlllException
    {
        if (vars instanceof Symbol)
        {
            addBinding((Symbol) vars, values);
        }
        else if (vars instanceof Cons)
        {
            Cons variables = (Cons) vars;
            if ((values == null && variables.length() > 0) || values.length() < variables.length())
                throw new JlllException("No enough values");
            Iterator<?> itVariables = variables.iterator();
            Iterator<?> itValues = values.iterator();
            while (itVariables.hasNext())
            {
                Symbol name = (Symbol) itVariables.next();
                Object value = itValues.next();
                addBinding(name, value);
            }
            if (((Cons.ConsIterator) itVariables).getDotted() != null)
            {
                Symbol name = (Symbol) ((Cons.ConsIterator) itVariables).getDotted();
                addBinding(name, ((Cons.ConsIterator) itValues).getTail());
            }
        }
    }
}
