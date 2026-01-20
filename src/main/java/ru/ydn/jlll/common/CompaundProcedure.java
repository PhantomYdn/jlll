package ru.ydn.jlll.common;

/**
 * CompaundProcedure is used in <tt>define</tt> statement to define new procedure
 */
public class CompaundProcedure extends Procedure
{
    private static final long serialVersionUID = -7249454179972745005L;
    protected final Object variables;
    protected final Object body;

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
        this.variables = variables;
        if (body != null && body instanceof Cons && ((Cons) body).cdr().equals(Null.NULL))
            this.body = ((Cons) body).car();
        else
            this.body = new Cons(Symbol.BEGIN, body);
    }

    @Override
    public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
    {
        ProcEnvironment pe = new ProcEnvironment(variables, values, env);
        return Evaluator.eval(body, pe);
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
        return "Compaund procedure.\n" + "Arguments: " + variables + "\n" + "Body: " + body;
    }
}
