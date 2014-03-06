package jlll.libs;

import jlll.common.Enviroment;
import jlll.common.Evaluator;
import jlll.common.Jlll;
import jlll.common.JlllException;
import jlll.common.Null;
import jlll.common.ReflectionLibrary;
import jlll.common.annotation.JlllName;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: May 19, 2003
 * Time: 10:53:59 AM
 * To change this template use Options | File Templates.
 */
public class PredicatesLib extends ReflectionLibrary
{
    public void load(Enviroment env) throws JlllException
    {
    	super.load(env);    	                                    
        Jlll.eval("(load-system-script \"predicates.jlll\")",env);
    }
    
    @JlllName(value="null?", useEvaluated=false)
    public boolean isNull(Enviroment env, Object obj) throws JlllException
    {
    	Object firstArg = obj==null?null:Evaluator.eval(obj,env);
        return firstArg==null || Null.NULL.equals(firstArg);
    }
    
    @JlllName("jlll-bound?")
    public boolean isJlllBound(Enviroment env, Object obj)
    {
    	return env.lookup(obj.toString())!=null;
    }
}

