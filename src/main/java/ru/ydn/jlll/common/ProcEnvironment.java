package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import ru.ydn.jlll.util.ListUtil;

/**
 * Environment for procedure execution with bound arguments.
 * Created when a procedure is called to bind parameter names to argument values.
 *
 * <p>
 * Supports:
 * </p>
 * <ul>
 * <li>Positional arguments</li>
 * <li>Keyword arguments (:name value)</li>
 * <li>Optional parameters with defaults</li>
 * <li>Rest parameters</li>
 * <li>Unknown keywords added to environment</li>
 * </ul>
 */
public class ProcEnvironment extends Environment
{
    private static final long serialVersionUID = -8061750297696026162L;

    /**
     * Legacy constructor for simple variable lists (no defaults or keywords).
     *
     * @param vars
     *            list of names of arguments
     * @param values
     *            already evaluated values
     * @param env
     *            environment to evaluate in
     * @throws JlllException
     */
    public ProcEnvironment(Object vars, Cons values, Environment env) throws JlllException
    {
        super(env);
        substLegacy(vars, values);
    }

    /**
     * New constructor supporting keyword arguments and defaults.
     *
     * @param parameters
     *            list of ParameterInfo objects
     * @param values
     *            already evaluated argument values (may contain Keywords)
     * @param env
     *            parent environment
     * @param definitionEnv
     *            environment for evaluating defaults (may be null to use env)
     * @throws JlllException
     */
    public ProcEnvironment(List<ParameterInfo> parameters, Cons values, Environment env, Environment definitionEnv)
            throws JlllException
    {
        super(env);
        substWithKeywords(parameters, values, definitionEnv != null ? definitionEnv : env);
    }

    /**
     * Legacy binding for backward compatibility with old-style (symbol ...) parameter lists.
     */
    private void substLegacy(Object vars, Cons values) throws JlllException
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

    /**
     * New binding logic supporting keyword arguments.
     *
     * <p>
     * Algorithm:
     * </p>
     * <ol>
     * <li>Extract keyword arguments from values</li>
     * <li>Bind positional arguments left-to-right</li>
     * <li>Keyword arguments override positional bindings</li>
     * <li>Evaluate defaults for unbound optional parameters</li>
     * <li>Collect remaining values for rest parameter</li>
     * <li>Add unknown keywords to environment</li>
     * </ol>
     */
    private void substWithKeywords(List<ParameterInfo> parameters, Cons values, Environment definitionEnv)
            throws JlllException
    {
        // Step 1: Separate keyword args from positional args
        Map<Symbol, Object> keywordArgs = new HashMap<>();
        List<Object> positionalArgs = new ArrayList<>();
        if (values != null && !values.isNull())
        {
            Iterator<?> it = values.iterator();
            while (it.hasNext())
            {
                Object arg = it.next();
                if (arg instanceof Keyword)
                {
                    Keyword kw = (Keyword) arg;
                    if (!it.hasNext())
                    {
                        throw new JlllException("Missing value for keyword argument " + kw);
                    }
                    Object value = it.next();
                    keywordArgs.put(kw.toSymbol(), value);
                }
                else
                {
                    positionalArgs.add(arg);
                }
            }
        }
        // Step 2: Find the rest parameter and regular parameters
        ParameterInfo restParam = null;
        List<ParameterInfo> regularParams = new ArrayList<>();
        for (ParameterInfo param : parameters)
        {
            if (param.isRest())
            {
                restParam = param;
            }
            else
            {
                regularParams.add(param);
            }
        }
        // Step 3 & 4: Bind arguments to parameters
        // Keywords take precedence over positional arguments
        int positionalIndex = 0;
        Map<Symbol, Object> bindings = new HashMap<>();
        Map<Symbol, Object> unusedKeywords = new HashMap<>(keywordArgs);
        for (ParameterInfo param : regularParams)
        {
            Symbol name = param.getName();
            if (keywordArgs.containsKey(name))
            {
                // Keyword argument - use it and don't consume positional
                bindings.put(name, keywordArgs.get(name));
                unusedKeywords.remove(name);
            }
            else if (positionalIndex < positionalArgs.size())
            {
                // Use next positional argument
                bindings.put(name, positionalArgs.get(positionalIndex));
                positionalIndex++;
            }
        }
        // Step 5: Apply defaults for unbound optional parameters, left-to-right
        for (ParameterInfo param : regularParams)
        {
            Symbol name = param.getName();
            if (!bindings.containsKey(name))
            {
                if (param.hasDefault())
                {
                    // Apply previously bound parameters to this environment for reference
                    for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
                    {
                        if (!hasLocalBinding(entry.getKey()))
                        {
                            addBinding(entry.getKey(), entry.getValue());
                        }
                    }
                    // Evaluate default
                    Object defaultValue;
                    if (param.isDefaultEvaluatedAtDefinition())
                    {
                        // Already evaluated, just use it
                        defaultValue = param.getDefaultExpression();
                    }
                    else
                    {
                        // Evaluate in current environment (which now has earlier params bound)
                        defaultValue = Evaluator.eval(param.getDefaultExpression(), this);
                    }
                    bindings.put(name, defaultValue);
                }
                else if (param.isRequired())
                {
                    throw new JlllException("Missing required argument: " + name);
                }
            }
        }
        // Step 6: Bind all parameters to environment
        for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
        {
            if (!hasLocalBinding(entry.getKey()))
            {
                addBinding(entry.getKey(), entry.getValue());
            }
            else
            {
                // Update existing binding (for case where we added for default evaluation)
                setBinding(entry.getKey(), entry.getValue());
            }
        }
        // Step 7: Handle rest parameter
        if (restParam != null)
        {
            // Collect remaining positional args
            List<Object> restValues = new ArrayList<>();
            for (int i = positionalIndex; i < positionalArgs.size(); i++)
            {
                restValues.add(positionalArgs.get(i));
            }
            addBinding(restParam.getName(), ListUtil.arrayToCons(restValues.toArray()));
        }
        else if (positionalIndex < positionalArgs.size())
        {
            // Extra positional args with no rest parameter - this is an error
            throw new JlllException(
                    "Too many arguments: expected " + regularParams.size() + ", got " + positionalArgs.size());
        }
        // Step 8: Add unknown keywords to environment
        for (Map.Entry<Symbol, Object> entry : unusedKeywords.entrySet())
        {
            addBinding(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Check if a symbol has a local binding (not inherited from parent).
     */
    private boolean hasLocalBinding(Symbol symbol)
    {
        return current.containsKey(symbol);
    }
}
