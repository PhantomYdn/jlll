package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Type predicates and testing functions.
 *
 * <p>
 * Provides type checking and value testing:
 * </p>
 * <ul>
 * <li><b>null?:</b> tests if value is null/nil</li>
 * <li><b>jlll-bound?:</b> tests if a symbol is bound in the environment</li>
 * </ul>
 *
 * <p>
 * Also loads predicates.jlll which provides additional predicates like
 * list?, number?, string?, symbol?, procedure?, etc.
 * </p>
 */
public class PredicatesLib extends ReflectionLibrary
{
    public void load(Enviroment env) throws JlllException
    {
        super.load(env);
        Jlll.eval("(load-system-script \"predicates.jlll\")", env);
    }

    @JlllName(value = "null?", useEvaluated = false)
    public boolean isNull(Enviroment env, Object obj) throws JlllException
    {
        Object firstArg = obj == null ? null : Evaluator.eval(obj, env);
        return firstArg == null || Null.NULL.equals(firstArg);
    }

    @JlllName("jlll-bound?")
    public boolean isJlllBound(Enviroment env, Object obj)
    {
        return env.lookup(obj.toString()) != null;
    }
}
