package ru.ydn.jlll.common;

import java.io.Serializable;

/**
 * Holds metadata about a procedure parameter.
 *
 * <p>
 * Supports:
 * </p>
 * <ul>
 * <li>Required parameters: just a symbol name</li>
 * <li>Optional parameters with defaults: (name default-expr)</li>
 * <li>Definition-time defaults: (name !default-expr)</li>
 * <li>Rest parameter: indicated by isRest flag</li>
 * </ul>
 */
public class ParameterInfo implements Serializable
{
    private static final long serialVersionUID = 2837465019283746501L;
    private final Symbol name;
    private final Object defaultExpression;
    private final boolean hasDefault;
    private final boolean defaultEvaluatedAtDefinition;
    private final boolean isRest;

    /**
     * Creates a required parameter (no default).
     *
     * @param name
     *            the parameter name
     */
    public ParameterInfo(Symbol name)
    {
        this(name, null, false, false, false);
    }

    /**
     * Creates an optional parameter with default.
     *
     * @param name
     *            the parameter name
     * @param defaultExpression
     *            the default value or expression
     * @param evaluatedAtDefinition
     *            true if default was evaluated at definition time (using !)
     */
    public ParameterInfo(Symbol name, Object defaultExpression, boolean evaluatedAtDefinition)
    {
        this(name, defaultExpression, true, evaluatedAtDefinition, false);
    }

    /**
     * Creates a rest parameter.
     *
     * @param name
     *            the parameter name
     * @param isRest
     *            must be true
     */
    public static ParameterInfo restParameter(Symbol name)
    {
        return new ParameterInfo(name, null, false, false, true);
    }

    private ParameterInfo(Symbol name, Object defaultExpression, boolean hasDefault,
            boolean defaultEvaluatedAtDefinition, boolean isRest)
    {
        this.name = name;
        this.defaultExpression = defaultExpression;
        this.hasDefault = hasDefault;
        this.defaultEvaluatedAtDefinition = defaultEvaluatedAtDefinition;
        this.isRest = isRest;
    }

    /**
     * Returns the parameter name.
     *
     * @return the parameter symbol
     */
    public Symbol getName()
    {
        return name;
    }

    /**
     * Returns the default expression or pre-evaluated value.
     *
     * @return the default, or null if no default
     */
    public Object getDefaultExpression()
    {
        return defaultExpression;
    }

    /**
     * Returns whether this parameter has a default value.
     *
     * @return true if optional with default
     */
    public boolean hasDefault()
    {
        return hasDefault;
    }

    /**
     * Returns whether the default was evaluated at definition time.
     *
     * @return true if using !default syntax
     */
    public boolean isDefaultEvaluatedAtDefinition()
    {
        return defaultEvaluatedAtDefinition;
    }

    /**
     * Returns whether this is a rest parameter (collects remaining args).
     *
     * @return true if rest parameter
     */
    public boolean isRest()
    {
        return isRest;
    }

    /**
     * Returns whether this parameter is required (no default, not rest).
     *
     * @return true if required
     */
    public boolean isRequired()
    {
        return !hasDefault && !isRest;
    }

    @Override
    public String toString()
    {
        if (isRest)
        {
            return ". " + name;
        }
        else if (hasDefault)
        {
            return "(" + name + " " + (defaultEvaluatedAtDefinition ? "!" : "") + defaultExpression + ")";
        }
        else
        {
            return name.toString();
        }
    }
}
