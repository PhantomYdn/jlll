package ru.ydn.jlll.common;

import java.util.List;

/**
 * CompaundProcedure is used in <tt>define</tt> statement to define new procedure.
 *
 * <p>
 * Supports:
 * </p>
 * <ul>
 * <li>Simple parameters: (define (f x y) ...)</li>
 * <li>Optional with default: (define (f (x 10)) ...)</li>
 * <li>Definition-time default: (define (f (x !(random))) ...)</li>
 * <li>Rest parameter: (define (f x . rest) ...)</li>
 * <li>Keyword arguments at call: (f :x 10)</li>
 * </ul>
 */
public class CompaundProcedure extends Procedure
{
    private static final long serialVersionUID = -7249454179972745005L;
    protected final Object variables;
    protected final Object body;
    protected final List<ParameterInfo> parameters;
    protected final boolean useNewParameterBinding;

    /**
     * Constructor of compaund procedure
     *
     * @param variables
     *            names of arguments for this procedure
     * @param body
     *            body of the procedure
     */
    public CompaundProcedure(Object variables, Object body)
    {
        this(variables, body, null, null);
    }

    /**
     * Constructor with environment for parsing default values.
     *
     * @param variables
     *            names of arguments for this procedure
     * @param body
     *            body of the procedure
     * @param env
     *            environment for evaluating !defaults
     */
    public CompaundProcedure(Object variables, Object body, Enviroment env)
    {
        this(variables, body, env, null);
    }

    /**
     * Full constructor.
     *
     * @param variables
     *            names of arguments for this procedure
     * @param body
     *            body of the procedure
     * @param env
     *            environment for evaluating !defaults
     * @param precomputedParams
     *            pre-parsed parameters (or null to parse from variables)
     */
    public CompaundProcedure(Object variables, Object body, Enviroment env, List<ParameterInfo> precomputedParams)
    {
        this.variables = variables;
        if (body != null && body instanceof Cons && ((Cons) body).cdr().equals(Null.NULL))
            this.body = ((Cons) body).car();
        else
            this.body = new Cons(Symbol.BEGIN, body);
        // Determine if we should use new parameter binding
        this.useNewParameterBinding = ParameterParser.usesNewSyntax(variables);
        // Parse parameters if using new syntax
        if (this.useNewParameterBinding)
        {
            if (precomputedParams != null)
            {
                this.parameters = precomputedParams;
            }
            else if (env != null)
            {
                try
                {
                    this.parameters = ParameterParser.parse(variables, env);
                }
                catch (JlllException e)
                {
                    throw new RuntimeException("Failed to parse parameters", e);
                }
            }
            else
            {
                // No environment, can't evaluate !defaults - store null and parse later
                this.parameters = null;
            }
        }
        else
        {
            this.parameters = null;
        }
    }

    @Override
    public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
    {
        ProcEnvironment pe;
        // Decide whether to use keyword-aware binding:
        // 1. Procedure has defaults (useNewParameterBinding), OR
        // 2. Call contains keywords AND procedure has regular params (not pure rest)
        boolean hasKeywordsInCall = ParameterParser.hasKeywordArgs(values);
        boolean shouldUseKeywordBinding = (useNewParameterBinding && parameters != null)
                || (hasKeywordsInCall && hasRegularParameters());
        if (shouldUseKeywordBinding)
        {
            List<ParameterInfo> params = parameters;
            if (params == null)
            {
                // Parse parameters on demand (legacy syntax but called with keywords)
                try
                {
                    params = ParameterParser.parse(variables, env);
                }
                catch (JlllException e)
                {
                    throw new JlllException("Failed to parse parameters for keyword binding", e);
                }
            }
            pe = new ProcEnvironment(params, values, env, null);
        }
        else
        {
            pe = new ProcEnvironment(variables, values, env);
        }
        return Evaluator.eval(body, pe);
    }

    /**
     * Check if this procedure has any regular (non-rest) parameters.
     * Used to determine if keyword processing is appropriate.
     *
     * <p>
     * A procedure defined as (lambda args body) or (define (f . rest) body) has only rest params.
     * A procedure with (lambda (a b) body) or (define (f a . rest) body) has regular params.
     * </p>
     */
    private boolean hasRegularParameters()
    {
        if (variables == null)
        {
            return false;
        }
        if (variables instanceof Symbol)
        {
            // Single symbol = pure rest parameter (lambda args ...)
            return false;
        }
        if (variables instanceof Cons)
        {
            Cons vars = (Cons) variables;
            // An empty list () or (() . rest) means no regular params
            // A non-empty proper part means there are regular params
            // Check: is the car null/empty?
            if (vars.isNull())
            {
                return false;
            }
            // Check for pattern (() . rest) which is (. rest) - pure rest
            Object car = vars.car();
            if (car == null || (car instanceof Cons && ((Cons) car).isNull()))
            {
                // First element is null or empty list - this might be (. rest) form
                // But actually (. rest) parses differently. Let's check length
                return vars.length() > 0;
            }
            return true;
        }
        return false;
    }

    public Object getBody()
    {
        return new Cons(Symbol.intern("lambda"),
                new Cons(variables, (body instanceof Cons ? ((Cons) body).clone() : body)));
    }

    @Override
    public String getDoc()
    {
        return describe();
    }

    public String describe()
    {
        if (useNewParameterBinding && parameters != null)
        {
            StringBuilder sb = new StringBuilder();
            sb.append("Compaund procedure.\nArguments: (");
            for (int i = 0; i < parameters.size(); i++)
            {
                if (i > 0)
                    sb.append(" ");
                sb.append(parameters.get(i));
            }
            sb.append(")\nBody: ").append(body);
            return sb.toString();
        }
        return "Compaund procedure.\n" + "Arguments: " + variables + "\n" + "Body: " + body;
    }
}
