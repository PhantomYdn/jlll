package jlll.common;

import java.io.Serializable;

import jlll.util.ListUtil;

/**
 * Abstact class for all jlll procedures.
 * During evaluating of value of the procedure applay method 
 * is invoked with specified arguments
 *
 */
public abstract class Procedure implements Serializable
{
	private static final long serialVersionUID = -8521523401153038007L;
	public transient int cnt = 0;
    
    public final Object applay(Enviroment env, Object... args) throws JlllException
    {
        return applay(ListUtil.arrayToCons(args), env);
    }

    public Object applay(Object values, Enviroment env) throws JlllException
    {
    	if(Null.NULL.equals(values)) values = Null.NULL;//new Cons();
        if(!(values instanceof Cons)) throw new JlllException("values not a Cons. Class of values: "+values.getClass().getName()+" toString():"+values.toString());
        return applay((Cons)values,env);
    }
    /**
     * Returns evaluated value from supplied arguments
     * @param values arguments for this procedure
     * @param env environment to evaluate in
     * @return result of evaluating
     * @throws JlllException when some error occured
     */    
    public Object applay(Cons values, Enviroment env) throws JlllException
    {
        return applayEvaluated(Jlll.evalEvery(values,env),env);
    }
    
    public final Object applayEvaluated(Enviroment env, Object...objects) throws JlllException
    {
        return applayEvaluated(Cons.list(objects),env);
    }
    public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
    {
        throw new JlllException("You try to use procedure that dosn't allow to applay already evaluated values");
    }
    
    /**
     * Obtain documentation for this procedure
     * @return documentation for this procedure
     */
    public abstract String getDoc();
    /**
     * Returns description of this procedure
     * @return description of this procedure
     */
    public abstract String describe();
}
