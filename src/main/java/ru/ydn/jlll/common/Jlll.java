package ru.ydn.jlll.common;

import java.io.*;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.io.JlllTokenizer;
import ru.ydn.jlll.util.ListUtil;

/**
 * Main entry point for the JLLL interpreter.
 * Provides static methods to evaluate Lisp expressions from strings, streams, files, and URLs.
 *
 * <p>
 * Basic usage:
 * </p>
 *
 * <pre>
 * // Evaluate a simple expression
 * Object result = Jlll.eval("(+ 1 2 3)");
 * // Evaluate with custom environment
 * Environment env = new Environment(Environment.top);
 * Jlll.eval("(define x 10)", env);
 * // Invoke with Java arguments accessible via $1, $2, etc.
 * Object sum = Jlll.invoke("(+ $1 $2)", 10, 20);
 * </pre>
 */
public class Jlll
{
    /**
     * Evaluates a parsed JLLL expression in the given environment.
     *
     * @param eval
     *            the parsed expression (Cons, Symbol, or literal)
     * @param env
     *            the environment for variable bindings
     * @return the result of evaluation
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object eval(Object eval, Environment env) throws JlllException
    {
        return Evaluator.eval(eval, env);
    }

    /**
     * Evaluates a parsed JLLL expression in the top-level environment.
     *
     * @param eval
     *            the parsed expression
     * @return the result of evaluation
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object eval(Object eval) throws JlllException
    {
        return eval(eval, Environment.top);
    }

    /**
     * Parses and evaluates a JLLL source string in the given environment.
     *
     * @param str
     *            the JLLL source code
     * @param env
     *            the environment for variable bindings
     * @return the result of the last expression
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(String str, Environment env) throws JlllException
    {
        return eval(new StringReader(str), env);
    }

    /**
     * Parses and evaluates a JLLL source string in the top-level environment.
     *
     * @param str
     *            the JLLL source code
     * @return the result of the last expression
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(String str) throws JlllException
    {
        return eval(str, Environment.top);
    }

    /**
     * Parses and evaluates JLLL source from an input stream.
     *
     * @param is
     *            the input stream containing JLLL source
     * @param env
     *            the environment for variable bindings
     * @return the result of the last expression
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(InputStream is, Environment env) throws JlllException
    {
        return eval(new InputStreamReader(is), env);
    }

    /**
     * Parses and evaluates JLLL source from an input stream in the top-level environment.
     *
     * @param is
     *            the input stream containing JLLL source
     * @return the result of the last expression
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(InputStream is) throws JlllException
    {
        return eval(is, Environment.top);
    }

    /**
     * Fetches and evaluates JLLL source from a URL.
     *
     * @param url
     *            the URL pointing to JLLL source
     * @param env
     *            the environment for variable bindings
     * @return the result of the last expression
     * @throws JlllException
     *             if fetching, parsing, or evaluation fails
     */
    public static Object eval(URL url, Environment env) throws JlllException
    {
        Object ret;
        try
        {
            ret = eval(url.openConnection().getInputStream(), env);
        }
        catch (IOException e)
        {
            throw new JlllException("IOERROR", e);
        }
        return ret;
    }

    /**
     * Fetches and evaluates JLLL source from a URL in the top-level environment.
     *
     * @param url
     *            the URL pointing to JLLL source
     * @return the result of the last expression
     * @throws JlllException
     *             if fetching, parsing, or evaluation fails
     */
    public static Object eval(URL url) throws JlllException
    {
        return eval(url, Environment.top);
    }

    /**
     * Parses and evaluates JLLL source from a reader.
     * Reads and evaluates all expressions, returning the result of the last one.
     *
     * @param reader
     *            the reader providing JLLL source
     * @param env
     *            the environment for variable bindings
     * @return the result of the last expression, or null if empty
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(Reader reader, Environment env) throws JlllException
    {
        JlllTokenizer jt = new JlllTokenizer(reader);
        Object ret = null;
        Object next = null;
        while (true)
        {
            try
            {
                next = jt.nextObject();
                if (next == null)
                    break;
                ret = Evaluator.eval(next, env);
            }
            catch (IOException e)
            {
                throw new JlllException("IOERROR", e);
            }
        }
        return ret;
    }

    /**
     * Parses and evaluates JLLL source from a reader in the top-level environment.
     *
     * @param reader
     *            the reader providing JLLL source
     * @return the result of the last expression
     * @throws JlllException
     *             if parsing or evaluation fails
     */
    public static Object eval(Reader reader) throws JlllException
    {
        return eval(reader, Environment.top);
    }

    /**
     * Evaluates each element in a Cons list and returns a new list of results.
     *
     * <p>
     * Tracks argument position in the evaluation context for continuation capture.
     * This allows continuations to know which argument was being evaluated when
     * they were invoked.
     * </p>
     *
     * @param cons
     *            the list of expressions to evaluate
     * @param env
     *            the environment for variable bindings
     * @return a new Cons list containing the evaluation results
     * @throws JlllException
     *             if evaluation of any element fails
     */
    public static Cons evalEvery(Cons cons, Environment env) throws JlllException
    {
        Evaluator.EvalContext ctx = Evaluator.getEvalContext();
        Iterator<?> it = cons.iterator();
        List<Object> ret = new ArrayList<Object>();
        int position = 0;
        while (it.hasNext())
        {
            // Update context with current position and evaluated args so far
            if (ctx != null)
            {
                ctx.argumentPosition = position;
                ctx.evaluatedArguments = ret.toArray();
            }
            ret.add(Evaluator.eval(it.next(), env));
            position++;
        }
        return ListUtil.arrayToCons(ret.toArray());
    }

    /**
     * Evaluates JLLL code with Java arguments accessible as $1, $2, $3, etc.
     * Uses the top-level environment.
     *
     * @param code
     *            the JLLL source code
     * @param args
     *            Java objects accessible in code as $1, $2, etc.
     * @return the result of evaluation, or null if result is Null
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object invoke(String code, Object... args) throws JlllException
    {
        return invoke(code, Environment.top, args);
    }

    /**
     * Evaluates JLLL code with Java arguments accessible as $1, $2, $3, etc.
     *
     * @param code
     *            the JLLL source code
     * @param env
     *            the parent environment
     * @param args
     *            Java objects accessible in code as $1, $2, etc.
     * @return the result of evaluation, or null if result is Null
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object invoke(String code, Environment env, Object... args) throws JlllException
    {
        ArgsEnvironment newEnv = new ArgsEnvironment(env, args);
        Object ret = Jlll.eval(code, newEnv);
        return ret == null || ret instanceof Null ? null : ret;
    }

    /**
     * Invokes a JLLL procedure by name with the given arguments.
     *
     * @param primitiveName
     *            the name of the procedure to invoke
     * @param env
     *            the environment containing the procedure binding
     * @param args
     *            the arguments to pass to the procedure
     * @return the result of the procedure call
     * @throws JlllException
     *             if the procedure is not found or execution fails
     */
    public static Object invokeProcedure(String primitiveName, Environment env, Object... args) throws JlllException
    {
        return eval(new Cons(Symbol.intern(primitiveName), Cons.list(args)), env);
    }

    /**
     * Evaluates code and returns a debug description including execution time and result type.
     *
     * @param code
     *            the JLLL source code to evaluate
     * @param env
     *            the environment for variable bindings
     * @return a multi-line string describing the evaluation result
     */
    public static String describeEval(String code, Environment env)
    {
        StringWriter sw = new StringWriter();
        PrintWriter out = new PrintWriter(sw);
        out.println("Code: " + code);
        try
        {
            long startTime = System.currentTimeMillis();
            Object ret = Jlll.eval(code, env);
            long finishTime = System.currentTimeMillis();
            out.println("Execution time: " + (finishTime - startTime) + "mil.");
            out.println("Class: " + (ret == null || ret instanceof Null ? "null" : ret.getClass().getName()));
            out.println("toString: " + ret);
        }
        catch (Exception e)
        {
            e.printStackTrace(out);
        }
        return sw.toString();
    }

    /**
     * Parses JLLL source code without evaluating it.
     * Returns the parsed expression tree for later evaluation.
     *
     * @param str
     *            the JLLL source code
     * @return the parsed expression (Cons tree, Symbol, or literal)
     * @throws JlllException
     *             if parsing fails
     */
    public static Object prepare(String str) throws JlllException
    {
        return prepare(new StringReader(str));
    }

    /**
     * Parses JLLL source from a reader without evaluating it.
     * Multiple expressions are wrapped in a (begin ...) form.
     *
     * @param reader
     *            the reader providing JLLL source
     * @return the parsed expression tree
     * @throws JlllException
     *             if parsing fails
     */
    public static Object prepare(Reader reader) throws JlllException
    {
        JlllTokenizer jt = new JlllTokenizer(reader);
        Object next = null;
        Object ret = null;
        boolean list = false;
        while (true)
        {
            try
            {
                next = jt.nextObject();
                if (next == null)
                    break;
                if (ret == null)
                {
                    ret = next;
                }
                else
                {
                    if (list)
                    {
                        ListUtil.append((Cons) ret, next);
                    }
                    else
                    {
                        ret = new Cons(Symbol.BEGIN, new Cons(ret));
                        ListUtil.append((Cons) ret, next);
                        list = true;
                    }
                }
            }
            catch (IOException e)
            {
                throw new JlllException("IOException", e);
            }
        }
        return ret;
    }

    /**
     * Simple REPL for testing. For production use, see {@link ru.ydn.jlll.cli.JlllCli}.
     *
     * @param args
     *            command line arguments (ignored)
     */
    public static void main(String[] args)
    {
        try
        {
            Reader in = new InputStreamReader(System.in);
            BufferedReader br = new BufferedReader(in);
            Environment env = new Environment(Environment.top);
            while (true)
            {
                System.out.print(">");
                String line = br.readLine();
                if (line == null)
                    break;
                try
                {
                    System.out.println(eval(line, env));
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                }
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
    }
}
