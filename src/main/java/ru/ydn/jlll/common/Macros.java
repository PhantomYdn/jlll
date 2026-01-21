package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.util.ListUtil;

/**
 * Macro procedure that transforms code before evaluation.
 * Unlike regular procedures, macros receive their arguments unevaluated and return
 * code that will then be evaluated.
 *
 * <p>
 * Created with {@code (defmacro name (args) body)} or {@code (macro (args) body)}.
 * </p>
 */
public class Macros extends CompaundProcedure
{
    private static final long serialVersionUID = 6419680788279882844L;

    /**
     * Creates a new macro with the given parameter list and body.
     *
     * @param variables
     *            the parameter list (Symbol or Cons of Symbols)
     * @param body
     *            the macro body expression
     */
    public Macros(Object variables, Object body)
    {
        super(variables, body);
    }

    /**
     * Expands the macro and evaluates the result.
     *
     * @param values
     *            the unevaluated argument expressions
     * @param env
     *            the evaluation environment
     * @return the result of evaluating the expanded code
     * @throws JlllException
     *             if expansion or evaluation fails
     */
    public Object apply(Cons values, Enviroment env) throws JlllException
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

    /**
     * Expands the macro without evaluating the result.
     * Useful for debugging macro transformations.
     *
     * @param values
     *            the unevaluated argument expressions
     * @param env
     *            the evaluation environment
     * @return the expanded code (before evaluation)
     * @throws JlllException
     *             if expansion fails
     */
    public Object macroExpand(Cons values, Enviroment env) throws JlllException
    {
        return super.apply(quoteAll(values), env);
    }

    /**
     * Returns a description of this macro for debugging.
     *
     * @return a string containing arguments and body
     */
    @Override
    public String describe()
    {
        return "Macros.\n" + "Arguments: " + variables + "\n" + "Body: " + body;
    }

    /**
     * Returns the documentation string for this macro.
     * For user-defined macros, documentation should be stored as metadata on the binding.
     *
     * @return empty string (documentation is in metadata)
     */
    @Override
    public String getDoc()
    {
        return "";
    }
}
