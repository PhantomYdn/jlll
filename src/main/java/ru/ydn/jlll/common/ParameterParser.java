package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Parses parameter lists for procedures, handling:
 * <ul>
 * <li>Simple symbols: x</li>
 * <li>Optional with default: (x default-expr)</li>
 * <li>Optional with definition-time default: (x !default-expr)</li>
 * <li>Rest parameter: . rest (in dotted list)</li>
 * </ul>
 */
public class ParameterParser
{
    /**
     * Parses a parameter list into ParameterInfo objects.
     *
     * @param vars
     *            the parameter specification (Symbol, Cons, or null)
     * @param env
     *            environment for evaluating !defaults
     * @return list of ParameterInfo objects
     * @throws JlllException
     *             if parameter syntax is invalid
     */
    public static List<ParameterInfo> parse(Object vars, Environment env) throws JlllException
    {
        List<ParameterInfo> result = new ArrayList<>();
        if (vars == null || (vars instanceof Cons && ((Cons) vars).isNull()))
        {
            return result;
        }
        if (vars instanceof Symbol)
        {
            // Single rest parameter: (lambda args ...)
            result.add(ParameterInfo.restParameter((Symbol) vars));
            return result;
        }
        if (!(vars instanceof Cons))
        {
            throw new JlllException("Invalid parameter list: " + vars);
        }
        Cons paramList = (Cons) vars;
        Cons.ConsIterator it = paramList.iterator();
        while (it.hasNext())
        {
            Object param = it.next();
            result.add(parseParameter(param, env));
        }
        // Handle dotted rest parameter
        Object dotted = it.getDotted();
        if (dotted != null)
        {
            if (!(dotted instanceof Symbol))
            {
                throw new JlllException("Rest parameter must be a symbol: " + dotted);
            }
            result.add(ParameterInfo.restParameter((Symbol) dotted));
        }
        return result;
    }

    /**
     * Parse a single parameter specification.
     */
    private static ParameterInfo parseParameter(Object param, Environment env) throws JlllException
    {
        if (param instanceof Symbol)
        {
            // Simple required parameter
            return new ParameterInfo((Symbol) param);
        }
        if (param instanceof Cons)
        {
            Cons spec = (Cons) param;
            if (spec.isNull() || spec.length() < 2)
            {
                throw new JlllException("Invalid parameter specification: " + spec);
            }
            Object nameObj = spec.car();
            if (!(nameObj instanceof Symbol))
            {
                throw new JlllException("Parameter name must be a symbol: " + nameObj);
            }
            Symbol name = (Symbol) nameObj;
            Object defaultExpr = spec.cadr();
            // Check if it's an exclamation (definition-time evaluation)
            if (defaultExpr instanceof Cons)
            {
                Cons defaultCons = (Cons) defaultExpr;
                if (Symbol.EXCLAMATION.equals(defaultCons.car()) && defaultCons.length() == 2)
                {
                    // Evaluate now
                    Object evaluatedDefault = Evaluator.eval(defaultCons.cadr(), env);
                    return new ParameterInfo(name, evaluatedDefault, true);
                }
            }
            // Invocation-time evaluation - store expression as-is
            return new ParameterInfo(name, defaultExpr, false);
        }
        throw new JlllException("Invalid parameter: " + param);
    }

    /**
     * Check if a parameter list uses the new syntax (has optional params with defaults).
     * Used to determine whether to use legacy or new procedure binding.
     */
    public static boolean usesNewSyntax(Object vars)
    {
        if (vars == null || vars instanceof Symbol)
        {
            return false;
        }
        if (!(vars instanceof Cons))
        {
            return false;
        }
        Cons paramList = (Cons) vars;
        for (Object param : paramList)
        {
            if (param instanceof Cons)
            {
                // Has (name default) style parameter
                return true;
            }
        }
        return false;
    }

    /**
     * Check if call arguments contain keyword arguments.
     * Used to decide whether to use keyword-aware binding even for legacy-style parameters.
     */
    public static boolean hasKeywordArgs(Cons values)
    {
        if (values == null || values.isNull())
        {
            return false;
        }
        for (Object arg : values)
        {
            if (arg instanceof Keyword)
            {
                return true;
            }
        }
        return false;
    }

    /**
     * Result of extracting keywords from an argument list.
     * Separates keyword arguments (:key value) from positional arguments.
     */
    public static class KeywordExtraction
    {
        /** Positional arguments (non-keyword values) in order of appearance. */
        public final List<Object> positional;
        /** Keyword arguments as Symbol-to-Value map. */
        public final Map<Symbol, Object> keywords;

        /**
         * Creates an extraction result.
         *
         * @param positional
         *            the positional arguments
         * @param keywords
         *            the keyword arguments
         */
        public KeywordExtraction(List<Object> positional, Map<Symbol, Object> keywords)
        {
            this.positional = positional;
            this.keywords = keywords;
        }
    }

    /**
     * Extracts keyword arguments from a Cons list.
     * Keywords (:key value) are collected into a map, other values are positional.
     *
     * @param values
     *            the argument list (may be null)
     * @return extraction result with positional list and keyword map
     * @throws JlllException
     *             if keyword is missing its value
     */
    public static KeywordExtraction extractKeywords(Cons values) throws JlllException
    {
        Map<Symbol, Object> keywords = new HashMap<>();
        List<Object> positional = new ArrayList<>();
        if (values == null || values.isNull())
        {
            return new KeywordExtraction(positional, keywords);
        }
        java.util.Iterator<?> it = values.iterator();
        while (it.hasNext())
        {
            Object arg = it.next();
            if (arg instanceof Keyword)
            {
                Keyword kw = (Keyword) arg;
                if (!it.hasNext())
                {
                    throw new JlllException("Missing value for keyword " + kw);
                }
                Object value = it.next();
                keywords.put(kw.toSymbol(), value);
            }
            else
            {
                positional.add(arg);
            }
        }
        return new KeywordExtraction(positional, keywords);
    }

    /**
     * Gets a boolean keyword argument from an extraction result.
     *
     * @param extraction
     *            the keyword extraction result
     * @param name
     *            the keyword name (without colon prefix)
     * @param defaultValue
     *            value to return if keyword is not present
     * @return the boolean value or defaultValue
     */
    public static boolean getBoolean(KeywordExtraction extraction, String name, boolean defaultValue)
    {
        Symbol key = Symbol.intern(name);
        Object value = extraction.keywords.get(key);
        if (value == null)
        {
            return defaultValue;
        }
        if (value instanceof Boolean)
        {
            return (Boolean) value;
        }
        // Handle truthy values
        return Boolean.TRUE.equals(value) || "true".equalsIgnoreCase(value.toString());
    }

    /**
     * Gets a string keyword argument from an extraction result.
     *
     * @param extraction
     *            the keyword extraction result
     * @param name
     *            the keyword name (without colon prefix)
     * @param defaultValue
     *            value to return if keyword is not present
     * @return the string value or defaultValue
     */
    public static String getString(KeywordExtraction extraction, String name, String defaultValue)
    {
        Symbol key = Symbol.intern(name);
        Object value = extraction.keywords.get(key);
        if (value == null)
        {
            return defaultValue;
        }
        return value.toString();
    }

    /**
     * Checks if a keyword argument is present in an extraction result.
     *
     * @param extraction
     *            the keyword extraction result
     * @param name
     *            the keyword name (without colon prefix)
     * @return true if the keyword is present
     */
    public static boolean hasKeyword(KeywordExtraction extraction, String name)
    {
        return extraction.keywords.containsKey(Symbol.intern(name));
    }
}
