package ru.ydn.jlll.libs;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import ru.ydn.jlll.common.CompoundProcedure;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Continuation;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.ModuleEnvironment;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ParameterParser;
import ru.ydn.jlll.common.PlainConsole;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.deps.DependencyResolver;
import ru.ydn.jlll.util.CommonUtil;
import ru.ydn.jlll.util.ListUtil;

/**
 * Core JLLL primitives and special forms.
 *
 * <p>
 * Provides fundamental language constructs:
 * </p>
 * <ul>
 * <li><b>Definitions:</b> define, set!, defmacro</li>
 * <li><b>Control flow:</b> if, cond, case, begin</li>
 * <li><b>Procedures:</b> lambda, apply, eval</li>
 * <li><b>List operations:</b> cons, car, cdr, quote, quasiquote</li>
 * <li><b>Higher-order:</b> map, mapall, filter</li>
 * <li><b>Utilities:</b> concat, time, sleep, describe, exit</li>
 * <li><b>Loading:</b> load-url, load-system-script</li>
 * <li><b>Environment:</b> current-environment, top-environment</li>
 * </ul>
 */
public class KernelLib implements Library
{
    /**
     * Gets the Console from the environment, with fallback to a PlainConsole wrapping System.out.
     *
     * @param env
     *            the environment to look up *console*
     * @return the Console for I/O operations
     */
    public static Console getConsole(Environment env)
    {
        Object console = env.lookup(Symbol.CONSOLE);
        if (console instanceof Console)
        {
            return (Console) console;
        }
        // Fallback: create PlainConsole wrapping System.out
        return new PlainConsole(new PrintWriter(System.out, true), null);
    }

    /**
     * Returns a human-readable type name for a value.
     *
     * @param value
     *            the value to describe
     * @return the type name
     */
    public static String getTypeName(Object value)
    {
        if (value instanceof Primitive)
        {
            return "Primitive";
        }
        else if (value instanceof Macros)
        {
            return "Macro";
        }
        else if (value instanceof Procedure)
        {
            return "Procedure";
        }
        else if (value instanceof Cons)
        {
            return "List";
        }
        else if (value instanceof Symbol)
        {
            return "Symbol";
        }
        else if (value instanceof Keyword)
        {
            return "Keyword";
        }
        else if (value instanceof String)
        {
            return "String";
        }
        else if (value instanceof Number)
        {
            return "Number";
        }
        else if (value instanceof Boolean)
        {
            return "Boolean";
        }
        else if (value == null || value instanceof Null)
        {
            return "Nil";
        }
        else
        {
            return "Java: " + value.getClass().getSimpleName();
        }
    }

    /**
     * Handles the (env :depends ...) special form for dynamic dependency loading.
     * This is extracted as a static method so it can be reused by ReplLib.
     *
     * @param values
     *            the unevaluated arguments (starting with :depends)
     * @param env
     *            the current environment
     * @return the result of evaluating the body forms in the child environment,
     *         or the child environment itself if no body forms are provided
     * @throws JlllException
     *             if dependency resolution fails or arguments are invalid
     */
    public static Object handleEnvDepends(Cons values, Environment env) throws JlllException
    {
        // Parse: (env :depends deps-list [:repos repos-list] body...)
        List<String> dependencies = new ArrayList<>();
        List<String> repositories = new ArrayList<>();
        List<Object> bodyForms = new ArrayList<>();
        Iterator<Object> it = values.iterator();
        while (it.hasNext())
        {
            Object item = it.next();
            if (item instanceof Keyword)
            {
                String kwName = ((Keyword) item).getName();
                if (kwName.equals("depends"))
                {
                    if (!it.hasNext())
                    {
                        throw new JlllException(":depends requires a list of dependency coordinates");
                    }
                    Object depsObj = Evaluator.eval(it.next(), env);
                    if (!(depsObj instanceof Cons))
                    {
                        throw new JlllException(":depends value must be a list of strings");
                    }
                    for (Object dep : (Cons) depsObj)
                    {
                        if (dep instanceof String)
                        {
                            dependencies.add((String) dep);
                        }
                        else if (dep instanceof Symbol)
                        {
                            dependencies.add(((Symbol) dep).getName());
                        }
                        else
                        {
                            throw new JlllException(
                                    "Dependency must be a string in format 'group:artifact:version', got: " + dep);
                        }
                    }
                }
                else if (kwName.equals("repos"))
                {
                    if (!it.hasNext())
                    {
                        throw new JlllException(":repos requires a list of repository URLs");
                    }
                    Object reposObj = Evaluator.eval(it.next(), env);
                    if (!(reposObj instanceof Cons))
                    {
                        throw new JlllException(":repos value must be a list of URL strings");
                    }
                    for (Object repo : (Cons) reposObj)
                    {
                        if (repo instanceof String)
                        {
                            repositories.add((String) repo);
                        }
                        else
                        {
                            throw new JlllException("Repository must be a URL string, got: " + repo);
                        }
                    }
                }
                else
                {
                    throw new JlllException("Unknown keyword in env: :" + kwName + ". Expected :depends or :repos");
                }
            }
            else
            {
                // This and remaining items are body forms
                bodyForms.add(item);
                while (it.hasNext())
                {
                    bodyForms.add(it.next());
                }
            }
        }
        if (dependencies.isEmpty())
        {
            throw new JlllException("(env :depends ...) requires at least one dependency");
        }
        // Resolve dependencies and create classloader
        DependencyResolver resolver = new DependencyResolver();
        for (String repo : repositories)
        {
            resolver.addRepository(repo);
        }
        List<File> jars = resolver.resolve(dependencies);
        ClassLoader childLoader = resolver.createClassLoader(jars, env.getClassLoader());
        // Create child environment with new classloader
        Environment childEnv = new Environment(env);
        childEnv.setClassLoader(childLoader, jars);
        // If no body, return the environment object
        if (bodyForms.isEmpty())
        {
            return childEnv;
        }
        // Evaluate body forms in child environment
        Object result = Null.NULL;
        for (Object form : bodyForms)
        {
            result = Evaluator.eval(form, childEnv);
        }
        return result;
    }

    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        env.addBinding(Symbol.TRUE, Boolean.TRUE);
        //       env.addBinding(Symbol.TRUE2, new Boolean(true));
        env.addBinding(Symbol.FALSE, Boolean.FALSE);
        //       env.addBinding(Symbol.FALSE2, new Boolean(false));
        env.addBinding(Symbol.NULL, Null.NULL);
        env.addBinding(Symbol.intern("nil"), Null.NULL); // Lisp-style alias for null
        new Primitive("define", env, "Defines a new binding. (define name value) for variables, "
                + "(define (name args...) body...) for functions. Supports :doc and other metadata keywords.")
        {
            private static final long serialVersionUID = -7343798584300580807L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Extract keywords (metadata) from the argument list
                ParameterParser.KeywordExtraction extraction = ParameterParser.extractKeywords(values);
                List<Object> positional = extraction.positional;
                Map<Symbol, Object> metadata = extraction.keywords;
                if (positional.isEmpty())
                {
                    throw new JlllException("define requires at least a name");
                }
                Object first = positional.get(0);
                if (first instanceof Symbol)
                {
                    // Simple variable definition: (define name value) or (define name :doc "..." value)
                    Symbol symbol = (Symbol) first;
                    if (positional.size() < 2)
                    {
                        throw new JlllException("define requires a value");
                    }
                    Object value = Evaluator.eval(positional.get(1), env);
                    env.addBindingWithMeta(symbol, value, metadata);
                    return value;
                }
                else if (first instanceof Cons)
                {
                    // Function definition: (define (name args...) body...)
                    // or (define (name args...) :doc "..." body...)
                    Cons def = (Cons) first;
                    String name = def.get(0).toString();
                    Object variables = def.cdr();
                    // Body is everything after the function signature in positional args
                    Object body;
                    if (positional.size() > 1)
                    {
                        // Build body from remaining positional args
                        List<Object> bodyParts = positional.subList(1, positional.size());
                        body = ru.ydn.jlll.util.ListUtil.arrayToCons(bodyParts.toArray());
                    }
                    else
                    {
                        body = Null.NULL;
                    }
                    // Pass env for evaluating !defaults
                    CompoundProcedure procedure = new CompoundProcedure(variables, body, env);
                    env.addBindingWithMeta(Symbol.intern(name), procedure, metadata);
                    return procedure;
                }
                throw new JlllException("define: first argument must be symbol or list, got: " + first);
            }
        };
        new Primitive("apply", env, "Applies a procedure to a list of arguments. (apply proc args-list)")
        {
            private static final long serialVersionUID = -5122951526914178471L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Cons args = (Cons) values.get(1);
                //System.out.println(args);
                return proc.applyEvaluated(args, env);
            }
        };
        new Primitive("set", env, "Modifies an existing binding. (set! name value) for variables, "
                + "(set! (name args...) body...) for functions. Alias: set!")
        {
            private static final long serialVersionUID = 7346093880732180126L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object car = values.car();
                if (car instanceof Symbol)
                {
                    Symbol symbol = (Symbol) car;
                    Object value = Evaluator.eval(values.cadr(), env);
                    env.setBinding(symbol, value);
                    return value;
                }
                else if (car instanceof Cons)
                {
                    Cons def = (Cons) car;
                    String name = def.get(0).toString();
                    Object variables = def.cdr();
                    Object body = values.tail(1);
                    CompoundProcedure procedure = new CompoundProcedure(variables, body);
                    env.setBinding(Symbol.intern(name), procedure);
                    return procedure;
                }
                return null;
            }
        };
        env.cloneBinding("set!", "set");
        new Primitive("defmacro", env,
                "Defines a macro. (defmacro (name args...) body...). "
                        + "Macros transform code at expansion time before evaluation. "
                        + "Supports :doc and other metadata keywords.")
        {
            private static final long serialVersionUID = 1953809997016714629L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Extract keywords (metadata) from the argument list
                ParameterParser.KeywordExtraction extraction = ParameterParser.extractKeywords(values);
                List<Object> positional = extraction.positional;
                Map<Symbol, Object> metadata = extraction.keywords;
                if (positional.isEmpty())
                {
                    throw new JlllException("defmacro requires at least a signature");
                }
                Object first = positional.get(0);
                if (!(first instanceof Cons))
                {
                    throw new JlllException("Symbol in defmacro is not allowed");
                }
                Cons def = (Cons) first;
                String name = def.get(0).toString();
                Object variables = def.cdr();
                // Body is everything after the signature in positional args
                Object body;
                if (positional.size() > 1)
                {
                    List<Object> bodyParts = positional.subList(1, positional.size());
                    body = ru.ydn.jlll.util.ListUtil.arrayToCons(bodyParts.toArray());
                }
                else
                {
                    body = Null.NULL;
                }
                Macros macros = new Macros(variables, body);
                env.addBindingWithMeta(Symbol.intern(name), macros, metadata);
                return macros;
            }
        };
        new Primitive("if", env, "Conditional expression. (if condition then-expr else-expr). "
                + "Evaluates then-expr if condition is true, else-expr otherwise.")
        {
            private static final long serialVersionUID = 8411805766481542038L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object dec = Evaluator.eval(values.get(0), env);
                Cons args = (Cons) values.cdr();
                if (CommonUtil.getBoolean(dec))
                {
                    return Evaluator.eval(args.car(), env);
                }
                else
                {
                    return Evaluator.eval(args.cadr(), env);
                }
            }
        };
        new Primitive("cond", env, "Multi-way conditional. (cond (test1 expr1...) (test2 expr2...) (else default...)). "
                + "Evaluates first clause whose test is true.")
        {
            private static final long serialVersionUID = -83499329550871262L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                for (Object next : values)
                {
                    Cons nextCond = (Cons) next;
                    Object cond = nextCond.car();
                    Object todo = new Cons(Symbol.BEGIN, nextCond.cdr());
                    if (Symbol.intern("else").equals(cond) || CommonUtil.getBoolean(Evaluator.eval(cond, env)))
                    {
                        return Evaluator.eval(todo, env);
                    }
                }
                return null;
            }
        };
        new Primitive("case", env,
                "Value-based dispatch. (case value ((key1 key2) expr1...) ((key3) expr2...) (else default...)). "
                        + "Matches value against key lists.")
        {
            private static final long serialVersionUID = -8203023265311714220L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object caseObject = Evaluator.eval(values.car(), env);
                Iterator<?> it = values.iterator();
                it.next();
                //TO Skip first
                while (it.hasNext())
                {
                    Cons nextCase = (Cons) it.next();
                    Object keysObject = nextCase.car();
                    Cons todo = new Cons(Symbol.BEGIN, nextCase.cdr());
                    if (Symbol.intern("else").equals(keysObject)
                            || (keysObject instanceof Cons && ((Cons) keysObject).contain(caseObject)))
                    {
                        return Evaluator.eval(todo, env);
                    }
                }
                return null;
            }
        };
        new Primitive("lambda", env, "Creates an anonymous procedure. (lambda (args...) body...). "
                + "Supports keyword args, defaults, and rest parameters.")
        {
            private static final long serialVersionUID = -840641725868209645L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object variables = values.get(0);
                Object body = values.tail(1);
                // Pass env for evaluating !defaults
                CompoundProcedure procedure = new CompoundProcedure(variables, body, env);
                return procedure;
            }
        };
        new Primitive("let", env, "Local bindings with parallel evaluation. (let ((var1 val1) ...) body...). "
                + "Also supports named let: (let name ((var1 init1) ...) body...) for recursive loops.")
        {
            private static final long serialVersionUID = 8273645091827364509L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 2)
                {
                    throw new JlllException("let requires bindings and at least one body expression");
                }
                Object first = values.car();
                // Check for named let: (let name ((var init) ...) body...)
                if (first instanceof Symbol)
                {
                    Symbol loopName = (Symbol) first;
                    if (values.length() < 3)
                    {
                        throw new JlllException("named let requires name, bindings, and at least one body expression");
                    }
                    Object bindingsObj = values.cadr();
                    if (!(bindingsObj instanceof Cons))
                    {
                        throw new JlllException("let: bindings must be a list, got: " + bindingsObj);
                    }
                    Cons bindings = (Cons) bindingsObj;
                    Cons body = (Cons) values.tail(2);
                    // Extract variables, initial values
                    List<Symbol> vars = new ArrayList<>();
                    List<Object> initVals = new ArrayList<>();
                    for (Object binding : bindings)
                    {
                        if (!(binding instanceof Cons))
                        {
                            throw new JlllException("let: each binding must be a list, got: " + binding);
                        }
                        Cons b = (Cons) binding;
                        if (b.length() != 2)
                        {
                            throw new JlllException(
                                    "let: each binding must have exactly 2 elements (var val), got: " + b);
                        }
                        Object varObj = b.car();
                        if (!(varObj instanceof Symbol))
                        {
                            throw new JlllException("let: variable must be a symbol, got: " + varObj);
                        }
                        vars.add((Symbol) varObj);
                        initVals.add(Evaluator.eval(b.cadr(), env));
                    }
                    // Create environment and bind the loop procedure
                    Environment letEnv = new Environment(env);
                    // Build lambda parameters list
                    Cons params = new Cons();
                    for (int i = vars.size() - 1; i >= 0; i--)
                    {
                        params = new Cons(vars.get(i), params);
                    }
                    // Create the loop procedure
                    CompoundProcedure loopProc = new CompoundProcedure(params, body, letEnv);
                    letEnv.addBinding(loopName, loopProc);
                    // Bind initial values
                    for (int i = 0; i < vars.size(); i++)
                    {
                        letEnv.addBinding(vars.get(i), initVals.get(i));
                    }
                    // Evaluate body
                    Object bodyExpr = new Cons(Symbol.BEGIN, body);
                    return Evaluator.eval(bodyExpr, letEnv);
                }
                // Regular let
                Object bindingsObj = first;
                if (!(bindingsObj instanceof Cons))
                {
                    throw new JlllException("let: bindings must be a list, got: " + bindingsObj);
                }
                Cons bindings = (Cons) bindingsObj;
                Cons body = (Cons) values.cdr();
                // Evaluate all values in CURRENT environment (parallel binding semantics)
                List<Symbol> vars = new ArrayList<>();
                List<Object> vals = new ArrayList<>();
                for (Object binding : bindings)
                {
                    if (!(binding instanceof Cons))
                    {
                        throw new JlllException("let: each binding must be a list, got: " + binding);
                    }
                    Cons b = (Cons) binding;
                    if (b.length() != 2)
                    {
                        throw new JlllException("let: each binding must have exactly 2 elements (var val), got: " + b);
                    }
                    Object varObj = b.car();
                    if (!(varObj instanceof Symbol))
                    {
                        throw new JlllException("let: variable must be a symbol, got: " + varObj);
                    }
                    vars.add((Symbol) varObj);
                    vals.add(Evaluator.eval(b.cadr(), env)); // Evaluate in current env
                }
                // Create new environment with all bindings
                Environment letEnv = new Environment(env);
                for (int i = 0; i < vars.size(); i++)
                {
                    letEnv.addBinding(vars.get(i), vals.get(i));
                }
                // Evaluate body in new environment
                Object bodyExpr = new Cons(Symbol.BEGIN, body);
                return Evaluator.eval(bodyExpr, letEnv);
            }
        };
        new Primitive("cons", env, "Constructs a pair. (cons a b) creates a cons cell with car=a and cdr=b.")
        {
            private static final long serialVersionUID = -7652629513702176997L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                return new Cons(values.get(0), values.get(1));
            }
        };
        new Primitive("begin", env,
                "Evaluates expressions sequentially. (begin expr1 expr2 ...). Returns the last result.")
        {
            private static final long serialVersionUID = 2744358405000249124L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object ret = null;
                Iterator<?> it = values.iterator();
                while (it.hasNext())
                {
                    ret = Evaluator.eval(it.next(), env);
                }
                return ret;
            }
        };
        new Primitive("quote", env,
                "Returns argument unevaluated. (quote x) or 'x. Prevents evaluation of the expression.")
        {
            private static final long serialVersionUID = -8365426058879553368L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() > 1)
                    throw new JlllException("So many parameters");
                return values.car();
            }
        };
        new Primitive("car", env, "Returns the first element of a pair/list. (car '(a b c)) returns a.")
        {
            private static final long serialVersionUID = 6224440278990904488L;

            public Object apply(Cons vaCons, Environment env) throws JlllException
            {
                Object obj = Evaluator.eval(vaCons.car(), env);
                if (!(obj instanceof Cons))
                    throw new JlllException("This is not a list: " + vaCons.car());
                return ((Cons) obj).car();
            }
        };
        new Primitive("cdr", env, "Returns the rest of a pair/list. (cdr '(a b c)) returns (b c).")
        {
            private static final long serialVersionUID = 3572491558064466506L;

            public Object apply(Cons vaCons, Environment env) throws JlllException
            {
                Object obj = Evaluator.eval(vaCons.car(), env);
                if (!(obj instanceof Cons))
                    throw new JlllException("This is not a list: " + vaCons.car());
                return ((Cons) obj).cdr();
            }
        };
        new Primitive("current-environment", env,
                "Returns a list of all symbols bound in the current environment scope.")
        {
            private static final long serialVersionUID = 8816274024795448776L;

            public Object apply(Object vaCons, Environment env) throws JlllException
            {
                return ListUtil.toCons(env.getAllBindings().keySet().toArray());
            }
        };
        new Primitive("top-environment", env,
                "Returns a list of all symbols bound in the top-level (global) environment.")
        {
            private static final long serialVersionUID = -2819367499932684513L;

            public Object apply(Object vaCons, Environment env) throws JlllException
            {
                return ListUtil.toCons(env.getTopEnvironment().getAllBindings().keySet().toArray());
            }
        };
        /*
         * new Primitive("exit")
         *
         * {
         *
         *
         * public Object apply(Object vaCons, Environment env) throws JlllException
         *
         * {
         *
         * System.exit(0);
         *
         * return null;
         *
         *
         * }
         *
         *
         * }
         * ;
         */
        new Primitive("load-system-script", env, "Loads and evaluates a JLLL script from the classpath resources.")
        {
            private static final long serialVersionUID = -8036963916533498346L;

            public Object apply(Cons vaCons, Environment env) throws JlllException
            {
                Jlll.eval(KernelLib.class.getResourceAsStream(Evaluator.eval(vaCons.get(0), env).toString()));
                return null;
            }
        };
        new Primitive("load-url", env,
                "Loads and evaluates a JLLL script from a URL. (load-url \"http://example.com/script.jlll\")")
        {
            private static final long serialVersionUID = -8736628261895814580L;

            public Object applyEvaluated(Cons vaCons, Environment env) throws JlllException
            {
                try
                {
                    Object urlObj = vaCons.get(0);
                    URL url = urlObj instanceof URL ? (URL) urlObj : new URL(urlObj.toString());
                    Jlll.eval(url, env);
                }
                catch (MalformedURLException e)
                {
                    throw new JlllException("Can't load: " + vaCons.get(0).toString(), e);
                }
                return null;
            }
        };
        new Primitive("load", env, "Loads and evaluates a JLLL script from a file path. (load \"path/to/script.jlll\")")
        {
            private static final long serialVersionUID = 7284619283746192837L;

            public Object applyEvaluated(Cons vaCons, Environment env) throws JlllException
            {
                String pathStr = vaCons.get(0).toString();
                Path filePath = Paths.get(pathStr);
                if (!Files.exists(filePath))
                {
                    throw new JlllException("File not found: " + pathStr);
                }
                if (!Files.isReadable(filePath))
                {
                    throw new JlllException("File not readable: " + pathStr);
                }
                try (BufferedReader reader = Files.newBufferedReader(filePath))
                {
                    Jlll.eval(reader, env);
                }
                catch (IOException e)
                {
                    throw new JlllException("Cannot load file: " + pathStr, e);
                }
                return null;
            }
        };
        new Primitive("module", env,
                "Defines a module with optional exports and dependencies. "
                        + "(module name body...) or (module name :depends '(\"group:artifact:version\") body...) "
                        + "Use (export sym1 sym2) or (export *) within the body to mark exports. "
                        + "Modules with :depends get their own classloader for loading external JARs. "
                        + "Note: Modules with :depends cannot be redefined.")
        {
            private static final long serialVersionUID = 8374619283746192838L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                // (module name [:depends deps-list] [:repos repos-list] body...)
                if (values.length() < 1)
                {
                    throw new JlllException("module requires a name");
                }
                Object nameObj = values.get(0);
                if (!(nameObj instanceof Symbol))
                {
                    throw new JlllException("module name must be a symbol: " + nameObj);
                }
                String moduleName = ((Symbol) nameObj).getName();
                // Parse rest of arguments: look for :depends, :repos, and body
                List<String> dependencies = new ArrayList<>();
                List<String> repositories = new ArrayList<>();
                List<Object> bodyForms = new ArrayList<>();
                Iterator<Object> it = ((Cons) values.cdr()).iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    if (item instanceof Keyword)
                    {
                        String kwName = ((Keyword) item).getName();
                        if (kwName.equals("depends"))
                        {
                            if (!it.hasNext())
                            {
                                throw new JlllException(":depends requires a list of dependency coordinates");
                            }
                            Object depsObj = Evaluator.eval(it.next(), env);
                            if (!(depsObj instanceof Cons))
                            {
                                throw new JlllException(":depends value must be a list of strings");
                            }
                            for (Object dep : (Cons) depsObj)
                            {
                                if (dep instanceof String)
                                {
                                    dependencies.add((String) dep);
                                }
                                else if (dep instanceof Symbol)
                                {
                                    dependencies.add(((Symbol) dep).getName());
                                }
                                else
                                {
                                    throw new JlllException(
                                            "Dependency must be a string 'group:artifact:version', got: " + dep);
                                }
                            }
                        }
                        else if (kwName.equals("repos"))
                        {
                            if (!it.hasNext())
                            {
                                throw new JlllException(":repos requires a list of repository URLs");
                            }
                            Object reposObj = Evaluator.eval(it.next(), env);
                            if (!(reposObj instanceof Cons))
                            {
                                throw new JlllException(":repos value must be a list of URL strings");
                            }
                            for (Object repo : (Cons) reposObj)
                            {
                                if (repo instanceof String)
                                {
                                    repositories.add((String) repo);
                                }
                                else
                                {
                                    throw new JlllException("Repository must be a URL string, got: " + repo);
                                }
                            }
                        }
                        else
                        {
                            throw new JlllException(
                                    "Unknown keyword in module: :" + kwName + ". Expected :depends or :repos");
                        }
                    }
                    else
                    {
                        // This and remaining items are body forms
                        bodyForms.add(item);
                        while (it.hasNext())
                        {
                            bodyForms.add(it.next());
                        }
                    }
                }
                // Check for re-definition
                ModuleEnvironment moduleEnv = Environment.getModule(moduleName);
                if (moduleEnv != null)
                {
                    // Modules with dependencies cannot be redefined (immutable classloader)
                    if (moduleEnv.hasOwnClassLoader())
                    {
                        throw new JlllException("Module '" + moduleName + "' has dependencies and cannot be redefined");
                    }
                    if (!dependencies.isEmpty())
                    {
                        throw new JlllException("Module '" + moduleName
                                + "' already exists; cannot add dependencies to existing module");
                    }
                }
                else
                {
                    // Create new module with top environment as parent
                    moduleEnv = new ModuleEnvironment(moduleName, env.getTopEnvironment());
                    // If there are dependencies, resolve them and set classloader
                    if (!dependencies.isEmpty())
                    {
                        DependencyResolver resolver = new DependencyResolver();
                        for (String repo : repositories)
                        {
                            resolver.addRepository(repo);
                        }
                        List<File> jars = resolver.resolve(dependencies);
                        ClassLoader childLoader = resolver.createClassLoader(jars, env.getClassLoader());
                        moduleEnv.setClassLoader(childLoader, jars);
                    }
                    Environment.registerModule(moduleName, moduleEnv);
                }
                // Evaluate body in module environment
                Object result = null;
                for (Object form : bodyForms)
                {
                    result = Evaluator.eval(form, moduleEnv);
                }
                moduleEnv.setLoaded(true);
                return result;
            }
        };
        new Primitive("export", env, "Marks symbols for export from current module. (export sym1 sym2 ...) "
                + "or (export *) to export all bindings.")
        {
            private static final long serialVersionUID = 8374619283746192839L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Find enclosing ModuleEnvironment
                Environment current = env;
                while (current != null && !(current instanceof ModuleEnvironment))
                {
                    current = current.getParent();
                }
                if (current == null)
                {
                    throw new JlllException("export can only be used inside a module");
                }
                ModuleEnvironment module = (ModuleEnvironment) current;
                for (Object item : values)
                {
                    if (item instanceof Symbol)
                    {
                        Symbol sym = (Symbol) item;
                        if (sym.getName().equals("*"))
                        {
                            module.exportAll();
                        }
                        else
                        {
                            module.export(sym);
                        }
                    }
                    else
                    {
                        throw new JlllException("export expects symbols, got: " + item);
                    }
                }
                return null;
            }
        };
        new Primitive("import", env, "Imports symbols from a module. (import modname) imports all exports, "
                + "(import modname :only (a b)) imports specific symbols, "
                + "(import modname :prefix foo/) adds prefix, " + "(import modname :except (a)) excludes symbols.")
        {
            private static final long serialVersionUID = 8374619283746192840L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("import requires a module name");
                }
                // Get module name
                Object nameObj = values.get(0);
                if (!(nameObj instanceof Symbol))
                {
                    throw new JlllException("import: module name must be a symbol: " + nameObj);
                }
                String moduleName = ((Symbol) nameObj).getName();
                ModuleEnvironment module = Environment.getModule(moduleName);
                if (module == null)
                {
                    throw new JlllException("import: module not found: " + moduleName);
                }
                // Parse options
                Set<Symbol> onlySymbols = null;
                Set<Symbol> exceptSymbols = new HashSet<>();
                String prefix = "";
                Cons options = (Cons) values.cdr();
                Iterator<Object> it = options.iterator();
                while (it.hasNext())
                {
                    Object opt = it.next();
                    if (opt instanceof Keyword)
                    {
                        String kw = ((Keyword) opt).getName();
                        if (!it.hasNext())
                        {
                            throw new JlllException("import: keyword " + kw + " requires a value");
                        }
                        Object val = it.next();
                        if (kw.equals("only"))
                        {
                            onlySymbols = parseSymbolList(val, "import :only");
                        }
                        else if (kw.equals("except"))
                        {
                            exceptSymbols = parseSymbolList(val, "import :except");
                        }
                        else if (kw.equals("prefix"))
                        {
                            if (val instanceof Symbol)
                            {
                                prefix = ((Symbol) val).getName();
                            }
                            else if (val instanceof String)
                            {
                                prefix = (String) val;
                            }
                            else
                            {
                                throw new JlllException("import :prefix requires a symbol or string");
                            }
                        }
                        else
                        {
                            throw new JlllException("import: unknown option :" + kw);
                        }
                    }
                    else
                    {
                        throw new JlllException("import: expected keyword option, got: " + opt);
                    }
                }
                // Get exports and filter
                Map<Symbol, Object> exports = module.getExports();
                Map<Symbol, Map<Symbol, Object>> exportedMeta = module.getExportedMetadata();
                for (Map.Entry<Symbol, Object> entry : exports.entrySet())
                {
                    Symbol sym = entry.getKey();
                    // Apply :only filter
                    if (onlySymbols != null && !onlySymbols.contains(sym))
                    {
                        continue;
                    }
                    // Apply :except filter
                    if (exceptSymbols.contains(sym))
                    {
                        continue;
                    }
                    // Create target symbol with optional prefix
                    Symbol targetSym = prefix.isEmpty() ? sym : Symbol.intern(prefix + sym.getName());
                    // Copy binding with metadata
                    env.addBinding(targetSym, entry.getValue());
                    Map<Symbol, Object> meta = exportedMeta.get(sym);
                    if (meta != null)
                    {
                        for (Map.Entry<Symbol, Object> metaEntry : meta.entrySet())
                        {
                            env.setMeta(targetSym, metaEntry.getKey(), metaEntry.getValue());
                        }
                    }
                }
                return null;
            }

            private Set<Symbol> parseSymbolList(Object val, String context) throws JlllException
            {
                Set<Symbol> result = new HashSet<>();
                if (val instanceof Cons)
                {
                    for (Object item : (Cons) val)
                    {
                        if (item instanceof Symbol)
                        {
                            result.add((Symbol) item);
                        }
                        else
                        {
                            throw new JlllException(context + " requires a list of symbols");
                        }
                    }
                }
                else if (val instanceof Symbol)
                {
                    result.add((Symbol) val);
                }
                else
                {
                    throw new JlllException(context + " requires a list of symbols");
                }
                return result;
            }
        };
        new Primitive("require", env, "Loads a file and imports its module. (require \"path.jlll\") loads and imports, "
                + "(require \"path.jlll\" :as prefix) adds prefix to imports.")
        {
            private static final long serialVersionUID = 8374619283746192841L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("require needs a file path");
                }
                // Evaluate the path argument
                String pathStr = Evaluator.eval(values.get(0), env).toString();
                // Parse options
                String asPrefix = null;
                Cons options = (Cons) values.cdr();
                Iterator<Object> it = options.iterator();
                while (it.hasNext())
                {
                    Object opt = it.next();
                    if (opt instanceof Keyword)
                    {
                        String kw = ((Keyword) opt).getName();
                        if (!it.hasNext())
                        {
                            throw new JlllException("require: keyword " + kw + " requires a value");
                        }
                        Object val = it.next();
                        if (kw.equals("as"))
                        {
                            if (val instanceof Symbol)
                            {
                                asPrefix = ((Symbol) val).getName();
                                // Ensure prefix ends with /
                                if (!asPrefix.endsWith("/"))
                                {
                                    asPrefix = asPrefix + "/";
                                }
                            }
                            else
                            {
                                throw new JlllException("require :as requires a symbol");
                            }
                        }
                        else
                        {
                            throw new JlllException("require: unknown option :" + kw);
                        }
                    }
                }
                // Load the file
                Path filePath = Paths.get(pathStr);
                if (!Files.exists(filePath))
                {
                    throw new JlllException("require: file not found: " + pathStr);
                }
                if (!Files.isReadable(filePath))
                {
                    throw new JlllException("require: file not readable: " + pathStr);
                }
                // Record existing modules before loading
                Set<String> modulesBefore = new HashSet<>(Environment.getModuleNames());
                // Create a temporary environment to capture any module definitions
                Environment loadEnv = new Environment(env.getTopEnvironment());
                try (BufferedReader reader = Files.newBufferedReader(filePath))
                {
                    Jlll.eval(reader, loadEnv);
                }
                catch (IOException e)
                {
                    throw new JlllException("require: cannot load file: " + pathStr, e);
                }
                // Find modules that were defined during the load
                Set<String> modulesAfter = new HashSet<>(Environment.getModuleNames());
                modulesAfter.removeAll(modulesBefore);
                if (!modulesAfter.isEmpty())
                {
                    // Import from newly defined modules
                    for (String moduleName : modulesAfter)
                    {
                        ModuleEnvironment module = Environment.getModule(moduleName);
                        if (module != null)
                        {
                            Map<Symbol, Object> exports = module.getExports();
                            Map<Symbol, Map<Symbol, Object>> exportedMeta = module.getExportedMetadata();
                            for (Map.Entry<Symbol, Object> entry : exports.entrySet())
                            {
                                Symbol sym = entry.getKey();
                                Symbol targetSym = asPrefix != null ? Symbol.intern(asPrefix + sym.getName()) : sym;
                                env.addBinding(targetSym, entry.getValue());
                                Map<Symbol, Object> meta = exportedMeta.get(sym);
                                if (meta != null)
                                {
                                    for (Map.Entry<Symbol, Object> metaEntry : meta.entrySet())
                                    {
                                        env.setMeta(targetSym, metaEntry.getKey(), metaEntry.getValue());
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    // No module found - just copy all top-level definitions from loadEnv
                    // This handles files that don't use (module ...)
                    for (Map.Entry<Symbol, Object> entry : loadEnv.getAllBindings().entrySet())
                    {
                        // Skip built-in primitives (they're in top env)
                        if (env.getTopEnvironment().lookup(entry.getKey()) != null)
                        {
                            continue;
                        }
                        Symbol sym = entry.getKey();
                        Symbol targetSym = asPrefix != null ? Symbol.intern(asPrefix + sym.getName()) : sym;
                        env.addBinding(targetSym, entry.getValue());
                    }
                }
                return null;
            }
        };
        new Primitive("quasiquote", env,
                "Template with selective evaluation. `(a ,b ,@c) quotes a, evaluates b, splices c. "
                        + "Use , for unquote and ,@ for unquote-splicing.")
        {
            private static final long serialVersionUID = 2509375750196259260L;

            class RetObject
            {
                public Object object;
                public boolean splicing;

                public RetObject(Object object, boolean splicing)
                {
                    this.object = object;
                    this.splicing = splicing;
                }
            }

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object what = values.get(0);
                return evalQQ(what, env).object;
            }

            private RetObject evalQQ(Object what, Environment env) throws JlllException
            {
                if (what instanceof Cons)
                {
                    Cons whatCons = (Cons) what;
                    if (whatCons.car() == Symbol.UNQUOTE)
                    {
                        Object ret = Evaluator.eval(whatCons.get(1), env);
                        return new RetObject(ret, false);
                    }
                    else if (whatCons.car() == Symbol.UNQUOTE_SPLICING)
                    {
                        Object ret = Evaluator.eval(whatCons.get(1), env);
                        return new RetObject(ret, true);
                    }
                    else
                    {
                        Cons returnCons = new Cons();
                        Cons.ConsIterator it = whatCons.iterator();
                        while (it.hasNext())
                        {
                            Object next = it.next();
                            RetObject retObj = evalQQ(next, env);
                            if (retObj.splicing == false)
                            {
                                ListUtil.append(returnCons, retObj.object);
                            }
                            else
                            {
                                ListUtil.getLastCons(returnCons).cdr(retObj.object);
                            }
                        }
                        Object dotted = it.getDotted();
                        if (dotted != null)
                        {
                            RetObject retObj = evalQQ(dotted, env);
                            if (retObj.splicing == false)
                            {
                                ListUtil.getLastCons(returnCons).cdr(retObj.object);
                            }
                            else
                            {
                                if (retObj.object instanceof Cons)
                                {
                                    ListUtil.getLastCons(returnCons).cdr(((Cons) retObj.object).car());
                                }
                                else
                                {
                                    throw new JlllException("Can't add splicing dotted object");
                                }
                            }
                        }
                        return new RetObject(returnCons, false);
                    }
                }
                else
                {
                    return new RetObject(what, false);
                }
            }
        };
        new Primitive("eval", env, "Evaluates an expression. (eval '(+ 1 2)) returns 3. "
                + "(eval :env my-env '(expression)) evaluates expression in specified environment. "
                + "Useful for dynamic code execution and executing code in environments with custom classpaths.")
        {
            private static final long serialVersionUID = -8630743653655996673L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Environment targetEnv = env;
                Object expr = null;
                // Check for :env keyword
                Iterator<Object> it = values.iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    if (item instanceof Keyword && ((Keyword) item).getName().equals("env"))
                    {
                        if (!it.hasNext())
                        {
                            throw new JlllException(":env requires an environment argument");
                        }
                        Object envObj = it.next();
                        if (!(envObj instanceof Environment))
                        {
                            throw new JlllException(":env argument must be an Environment, got: "
                                    + (envObj == null ? "null" : envObj.getClass().getSimpleName()));
                        }
                        targetEnv = (Environment) envObj;
                    }
                    else
                    {
                        // This is the expression to evaluate
                        if (expr != null)
                        {
                            throw new JlllException("eval expects only one expression (use begin for multiple)");
                        }
                        expr = item;
                    }
                }
                if (expr == null)
                {
                    throw new JlllException("eval requires an expression to evaluate");
                }
                return Evaluator.eval(expr, targetEnv);
            }
        };
        new Primitive("time", env,
                "Measures execution time. (time expr) evaluates expr and returns elapsed milliseconds.")
        {
            private static final long serialVersionUID = 1296484298301550805L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object toEval = values.get(0);
                long start = System.currentTimeMillis();
                Evaluator.eval(toEval, env);
                /*
                 * System.out.println("Try to exec:"+toEval);
                 *
                 * Object result = Evaluator.eval(toEval, env);
                 *
                 * System.out.println("result:"+result);
                 */
                long finish = System.currentTimeMillis();
                return (int) (finish - start);
            }
        };
        new Primitive("sleep", env,
                "Pauses execution. (sleep milliseconds) suspends the current thread for the specified duration.")
        {
            private static final long serialVersionUID = 5958807729111336106L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object toEval = values.get(0);
                try
                {
                    Thread.sleep(((Number) toEval).longValue());
                }
                catch (InterruptedException e)
                {
                    Thread.currentThread().interrupt();
                }
                return null;
            }
        };
        new Primitive("concat", env, "Concatenates values to a string. (concat \"a\" 1 \"b\") returns \"a1b\".")
        {
            private static final long serialVersionUID = -2789439764629510145L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                StringBuilder sb = new StringBuilder();
                while (it.hasNext())
                {
                    sb.append(it.next());
                }
                return sb.toString();
            }
        };
        new Primitive("map", env,
                "Applies a procedure to each element. (map proc list) returns a new list with results.")
        {
            private static final long serialVersionUID = -6920908225941152717L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Iterator<?> it = ((Cons) values.get(1)).iterator();
                List<Object> ret = new ArrayList<Object>(values.length() - 1);
                while (it.hasNext())
                {
                    Object next = it.next();
                    ret.add(proc.applyEvaluated(env, next));
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("mapall", env, "Recursively applies a procedure to all elements in nested lists.")
        {
            private static final long serialVersionUID = 7289150232747124300L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                return mapAll(proc, (Cons) values.get(1), env);
                /*
                 * Iterator it = ((Cons)values.get(1)).iterator();
                 *
                 * List<Object> ret = new ArrayList<Object>(values.length()-1);
                 *
                 * while(it.hasNext())
                 *
                 * {
                 *
                 * Object next = it.next();
                 *
                 * ret.add(proc.apply(env, next));
                 *
                 *
                 * }
                 *
                 * return ListUtil.arrayToCons(ret.toArray());
                 */
            }

            private Cons mapAll(Procedure proc, Cons cons, Environment env) throws JlllException
            {
                Iterator<?> it = cons.iterator();
                List<Object> ret = new ArrayList<Object>(cons.length());
                while (it.hasNext())
                {
                    Object next = it.next();
                    Object toSet = (next instanceof Cons && !((Cons) next).isNull())
                            ? mapAll(proc, (Cons) next, env)
                            : proc.apply(new Cons(next), env);
                    ret.add(toSet);
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("filter", env,
                "Filters a list by predicate. (filter pred list) returns elements where (pred elem) is true.")
        {
            private static final long serialVersionUID = 6564713665261707057L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Iterator<?> it = ((Cons) values.get(1)).iterator();
                List<Object> ret = new ArrayList<Object>(values.length() - 1);
                while (it.hasNext())
                {
                    Object next = it.next();
                    if (CommonUtil.getBoolean(proc.applyEvaluated(env, next)))
                        ret.add(next);
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("jlll-extract-body", env,
                "Returns the body of a compound procedure. For introspection and debugging.")
        {
            private static final long serialVersionUID = 4782501546001349950L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object function = values.get(0);
                if (!(function instanceof CompoundProcedure))
                    throw new JlllException("Function isn't compound: " + function.getClass());
                return ((CompoundProcedure) function).getBody();
            }
        };
        new Primitive("jlll-macro-expand", env,
                "Expands a macro without evaluating. (jlll-macro-expand macro-name args...) shows expansion result.")
        {
            private static final long serialVersionUID = 3835175014722360095L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                Object function = Evaluator.eval(values.get(0), env);
                if (!(function instanceof Macros))
                    throw new JlllException("Function isn't macros: " + function.getClass());
                return ((Macros) function).macroExpand(values.tail(1), env);
            }
        };
        new Primitive("describe", env,
                "Returns a description of a value. Works with procedures, symbols, throwables, and any object.")
        {
            private static final long serialVersionUID = 5164474211503405192L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object obj = values.get(0);
                StringBuilder sb = new StringBuilder();
                if (obj instanceof Procedure)
                {
                    sb.append(((Procedure) obj).describe());
                }
                else if (obj instanceof Symbol)
                {
                    Symbol sym = (Symbol) obj;
                    Object value = env.lookup(sym);
                    // Check for :doc metadata first - show at top if present
                    Symbol docSym = Symbol.intern("doc");
                    Object docMeta = env.getMeta(sym, docSym);
                    if (docMeta != null)
                    {
                        sb.append("Doc: ").append(docMeta).append("\n\n");
                    }
                    if (value instanceof Procedure)
                    {
                        sb.append(((Procedure) value).describe());
                    }
                    else
                    {
                        sb.append("Symbol '").append(sym.getName()).append("'");
                        if (value != null)
                        {
                            sb.append("\nValue: ").append(value);
                        }
                    }
                    // Append remaining metadata (excluding :doc since already shown at top)
                    if (env.hasMeta(sym))
                    {
                        Map<Symbol, Object> meta = env.getAllMeta(sym);
                        boolean hasOtherMeta = meta.entrySet().stream().anyMatch(e -> !e.getKey().equals(docSym));
                        if (hasOtherMeta)
                        {
                            sb.append("\nMetadata:");
                            for (Map.Entry<Symbol, Object> entry : meta.entrySet())
                            {
                                if (!entry.getKey().equals(docSym))
                                {
                                    sb.append("\n  :").append(entry.getKey().getName()).append(" ")
                                            .append(entry.getValue());
                                }
                            }
                        }
                    }
                }
                else if (obj instanceof Throwable)
                {
                    StringWriter sw = new StringWriter();
                    PrintWriter out = new PrintWriter(sw);
                    out.println("Throwable: ");
                    ((Throwable) obj).printStackTrace(out);
                    sb.append(sw.toString());
                }
                else
                {
                    sb.append("Instance of ").append(obj.getClass()).append("\n").append("toString() : ").append(obj);
                }
                return sb.toString();
            }
        };
        new Primitive("exclamation", env,
                "Evaluates and returns the argument. Used for !default syntax in parameter definitions.")
        {
            private static final long serialVersionUID = 1827364509182736450L;

            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                    throw new JlllException("exclamation requires exactly one argument");
                return Evaluator.eval(values.car(), env);
            }
        };
        // ============== Metadata Primitives ==============
        new Primitive("doc", env, "Prints and returns documentation for a symbol or procedure. "
                + "Documentation is stored as :doc metadata on bindings.")
        {
            private static final long serialVersionUID = 9182736450918273645L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                    throw new JlllException("doc requires exactly one argument");
                Object arg = values.get(0);
                Console console = getConsole(env);
                Symbol sym;
                String symName;
                if (arg instanceof Symbol)
                {
                    sym = (Symbol) arg;
                    symName = sym.getName();
                }
                else if (arg instanceof Procedure)
                {
                    // For procedures passed directly, try to get their doc
                    String doc = ((Procedure) arg).getDoc();
                    if (doc != null && !doc.isEmpty())
                    {
                        console.println(doc);
                        console.flush();
                    }
                    return (doc != null && !doc.isEmpty()) ? doc : null;
                }
                else
                {
                    throw new JlllException("doc requires a symbol or procedure");
                }
                // Primary source: metadata on the binding
                Object doc = env.getMeta(sym, Symbol.intern("doc"));
                String docString = null;
                if (doc != null)
                {
                    docString = doc.toString();
                }
                else
                {
                    // Fallback: Procedure.getDoc() for backward compatibility
                    Object value = env.lookup(sym);
                    if (value instanceof Procedure)
                    {
                        String procDoc = ((Procedure) value).getDoc();
                        if (procDoc != null && !procDoc.isEmpty())
                        {
                            docString = procDoc;
                        }
                    }
                }
                // Print formatted output
                Object value = env.lookup(sym);
                console.println();
                if (value == null)
                {
                    console.printError("Symbol '" + symName + "' is not bound.");
                    console.println();
                }
                else
                {
                    console.printColored(symName, Console.Color.CYAN);
                    console.println();
                    console.printFaint("──────────────────────────────────────────────────");
                    console.println();
                    if (docString != null)
                    {
                        console.println(docString);
                    }
                    else
                    {
                        console.printHint("No documentation available.");
                        console.println();
                    }
                    console.println();
                    console.printHint("Type: " + getTypeName(value));
                    console.println();
                }
                console.println();
                console.flush();
                return docString;
            }
        };
        // ============== Documentation Access ==============
        new Primitive("jlll-docs", env,
                "Access JLLL documentation. (jlll-docs) lists topics, (jlll-docs \"topic\") shows documentation.")
        {
            private static final long serialVersionUID = 9182736450918273699L;
            // Topic aliases for convenience
            private final java.util.Map<String, String> ALIASES = java.util.Map.ofEntries(
                    java.util.Map.entry("interop", "java-interop"), java.util.Map.entry("java", "java-interop"),
                    java.util.Map.entry("forms", "special-forms"), java.util.Map.entry("special", "special-forms"),
                    java.util.Map.entry("procs", "procedures"), java.util.Map.entry("funcs", "primitives"),
                    java.util.Map.entry("functions", "primitives"), java.util.Map.entry("lazy", "lazy-sequences"),
                    java.util.Map.entry("prompt", "system-prompt"), java.util.Map.entry("ai", "system-prompt"));
            // Available documentation topics with descriptions
            private final java.util.Map<String, String> TOPICS = new java.util.LinkedHashMap<>();
            {
                TOPICS.put("README", "Overview & Quick Start");
                TOPICS.put("syntax", "Atoms, lists, reader macros, keywords");
                TOPICS.put("special-forms", "define, if, lambda, let, cond");
                TOPICS.put("procedures", "Keyword arguments, defaults, rest args");
                TOPICS.put("primitives", "Built-in functions by library");
                TOPICS.put("macros", "Macro definition and expansion");
                TOPICS.put("java-interop", "Calling Java from JLLL");
                TOPICS.put("lazy-sequences", "Lazy evaluation and streams");
                TOPICS.put("metadata", "Documentation and metadata on bindings");
                TOPICS.put("system-prompt", "AI assistant system prompt");
            }

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Console console = getConsole(env);
                if (values.length() == 0)
                {
                    // List available topics
                    return listTopics(console);
                }
                String topic = values.get(0).toString();
                // Resolve alias
                topic = ALIASES.getOrDefault(topic.toLowerCase(), topic);
                // Load and display documentation
                return showDocumentation(topic, console);
            }

            private Object listTopics(Console console)
            {
                console.println();
                console.printHeader("Available Documentation Topics");
                console.printFaint("──────────────────────────────────────────────────");
                console.println();
                for (java.util.Map.Entry<String, String> entry : TOPICS.entrySet())
                {
                    console.print("  ");
                    console.printKeyword(String.format("%-16s", entry.getKey()));
                    console.printFaint(" - " + entry.getValue());
                    console.println();
                }
                console.println();
                console.printFaint("Aliases: interop, java, forms, special, procs, funcs, functions, lazy, prompt, ai");
                console.println();
                console.println();
                console.printHint("Usage: (jlll-docs \"topic\") to read documentation");
                console.println();
                console.flush();
                return Null.NULL; // Side-effect only, no return value
            }

            private Object showDocumentation(String topic, Console console) throws JlllException
            {
                String resourcePath = "docs/" + topic + ".md";
                try (InputStream is = KernelLib.class.getClassLoader().getResourceAsStream(resourcePath))
                {
                    if (is == null)
                    {
                        throw new JlllException("Documentation topic not found: " + topic
                                + ". Use (jlll-docs) to list available topics.");
                    }
                    String content = new String(is.readAllBytes(), StandardCharsets.UTF_8);
                    // Render with console formatting if supported
                    if (console.supportsColor())
                    {
                        renderMarkdown(content, console);
                    }
                    else
                    {
                        console.println(content);
                    }
                    console.flush();
                    return Null.NULL; // Side-effect only, no return value
                }
                catch (IOException e)
                {
                    throw new JlllException("Error reading documentation: " + e.getMessage(), e);
                }
            }

            private void renderMarkdown(String content, Console console)
            {
                String[] lines = content.split("\n");
                boolean inCodeBlock = false;
                for (String line : lines)
                {
                    // Code block handling
                    if (line.startsWith("```"))
                    {
                        inCodeBlock = !inCodeBlock;
                        if (inCodeBlock)
                        {
                            console.printFaint("┌─────────────────────────────────────────");
                            console.println();
                        }
                        else
                        {
                            console.printFaint("└─────────────────────────────────────────");
                            console.println();
                        }
                        continue;
                    }
                    if (inCodeBlock)
                    {
                        console.printFaint("│ ");
                        console.printKeyword(line);
                        console.println();
                        continue;
                    }
                    // Headers
                    if (line.startsWith("### "))
                    {
                        console.println();
                        console.printBold(line.substring(4));
                        console.println();
                        continue;
                    }
                    if (line.startsWith("## "))
                    {
                        console.println();
                        console.printHeader(line.substring(3));
                        console.printFaint("──────────────────────────────────────────────────");
                        console.println();
                        continue;
                    }
                    if (line.startsWith("# "))
                    {
                        console.println();
                        console.printColored(line.substring(2), Console.Color.CYAN);
                        console.println();
                        console.printFaint("══════════════════════════════════════════════════");
                        console.println();
                        continue;
                    }
                    // Horizontal rule
                    if (line.matches("^-{3,}$") || line.matches("^\\*{3,}$"))
                    {
                        console.printFaint("──────────────────────────────────────────────────");
                        console.println();
                        continue;
                    }
                    // List items
                    if (line.matches("^\\s*[-*]\\s+.*"))
                    {
                        String item = line.replaceFirst("^(\\s*)[-*]\\s+", "$1• ");
                        renderInlineFormatting(item, console);
                        console.println();
                        continue;
                    }
                    // Regular line with inline formatting
                    renderInlineFormatting(line, console);
                    console.println();
                }
            }

            private void renderInlineFormatting(String line, Console console)
            {
                // Simple inline formatting: `code`, **bold**
                int i = 0;
                while (i < line.length())
                {
                    // Inline code
                    if (line.charAt(i) == '`' && i + 1 < line.length())
                    {
                        int end = line.indexOf('`', i + 1);
                        if (end > i)
                        {
                            console.printKeyword(line.substring(i + 1, end));
                            i = end + 1;
                            continue;
                        }
                    }
                    // Bold
                    if (i + 1 < line.length() && line.charAt(i) == '*' && line.charAt(i + 1) == '*')
                    {
                        int end = line.indexOf("**", i + 2);
                        if (end > i)
                        {
                            console.printBold(line.substring(i + 2, end));
                            i = end + 2;
                            continue;
                        }
                    }
                    console.print(String.valueOf(line.charAt(i)));
                    i++;
                }
            }
        };
        new Primitive("meta", env, "Returns metadata for a symbol. "
                + "(meta sym) returns all metadata as alist; (meta sym :key) returns specific value.")
        {
            private static final long serialVersionUID = 9182736450918273646L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() == 1)
                {
                    // (meta sym) - return all metadata as association list
                    Object arg = values.get(0);
                    if (!(arg instanceof Symbol))
                    {
                        throw new JlllException("meta requires a symbol");
                    }
                    Map<Symbol, Object> allMeta = env.getAllMeta((Symbol) arg);
                    if (allMeta.isEmpty())
                    {
                        return Null.NULL;
                    }
                    // Convert to association list ((key . value) ...)
                    List<Object> pairs = new ArrayList<>();
                    for (Map.Entry<Symbol, Object> entry : allMeta.entrySet())
                    {
                        pairs.add(new Cons(Keyword.fromSymbol(entry.getKey()), entry.getValue()));
                    }
                    return ru.ydn.jlll.util.ListUtil.arrayToCons(pairs.toArray());
                }
                else if (values.length() == 2)
                {
                    // (meta sym :key) - return specific metadata
                    Object arg = values.get(0);
                    Object keyArg = values.get(1);
                    if (!(arg instanceof Symbol))
                    {
                        throw new JlllException("meta requires a symbol as first argument");
                    }
                    Symbol sym = (Symbol) arg;
                    Symbol key;
                    if (keyArg instanceof Keyword)
                    {
                        key = ((Keyword) keyArg).toSymbol();
                    }
                    else if (keyArg instanceof Symbol)
                    {
                        key = (Symbol) keyArg;
                    }
                    else
                    {
                        throw new JlllException("meta key must be a keyword or symbol");
                    }
                    return env.getMeta(sym, key);
                }
                else
                {
                    throw new JlllException("meta requires 1 or 2 arguments");
                }
            }
        };
        new Primitive("set-meta!", env, "Sets metadata on a symbol. Usage: (set-meta! sym :key value)")
        {
            private static final long serialVersionUID = 9182736450918273647L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 3)
                {
                    throw new JlllException("set-meta! requires 3 arguments: (set-meta! symbol :key value)");
                }
                Object arg = values.get(0);
                Object keyArg = values.get(1);
                Object value = values.get(2);
                if (!(arg instanceof Symbol))
                {
                    throw new JlllException("set-meta! requires a symbol as first argument");
                }
                Symbol sym = (Symbol) arg;
                // Verify binding exists
                if (env.lookup(sym) == null)
                {
                    throw new JlllException("set-meta!: no binding for symbol " + sym);
                }
                Symbol key;
                if (keyArg instanceof Keyword)
                {
                    key = ((Keyword) keyArg).toSymbol();
                }
                else if (keyArg instanceof Symbol)
                {
                    key = (Symbol) keyArg;
                }
                else
                {
                    throw new JlllException("set-meta! key must be a keyword or symbol");
                }
                env.setMeta(sym, key, value);
                return value;
            }
        };
        new Primitive("define-from", env,
                "Creates a new binding with copied value and metadata from an existing symbol.")
        {
            private static final long serialVersionUID = 9182736450918273648L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 2)
                {
                    throw new JlllException("define-from requires 2 arguments: (define-from new-name source-symbol)");
                }
                Object newNameArg = values.get(0);
                Object sourceArg = values.get(1);
                // First arg is unevaluated symbol (new name)
                if (!(newNameArg instanceof Symbol))
                {
                    throw new JlllException("define-from: first argument must be a symbol");
                }
                Symbol newName = (Symbol) newNameArg;
                // Second arg is evaluated to get the source symbol
                Object evaluatedSource = Evaluator.eval(sourceArg, env);
                if (!(evaluatedSource instanceof Symbol))
                {
                    throw new JlllException("define-from: second argument must evaluate to a symbol");
                }
                Symbol source = (Symbol) evaluatedSource;
                // Get value from source
                Object value = env.lookup(source);
                if (value == null)
                {
                    throw new JlllException("define-from: no binding for source symbol " + source);
                }
                // Get metadata from source
                Map<Symbol, Object> meta = env.getAllMeta(source);
                // Create new binding with same value and metadata
                env.addBindingWithMeta(newName, value, meta.isEmpty() ? null : meta);
                return value;
            }
        };
        // ============== Exception Handling Primitives ==============
        new Primitive("raise", env, "Raises an exception. (raise value) throws a JlllException. "
                + "If value is already a Throwable, it is wrapped; otherwise value is converted to string message.")
        {
            private static final long serialVersionUID = 8273645091827364501L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("raise requires exactly one argument");
                }
                Object err = values.get(0);
                if (err instanceof JlllException)
                {
                    throw (JlllException) err;
                }
                else if (err instanceof Throwable)
                {
                    throw new JlllException((Throwable) err);
                }
                else
                {
                    throw new JlllException(err.toString());
                }
            }
        };
        new Primitive("error", env, "Raises an error with a message. (error msg...) concatenates all arguments "
                + "into a single error message and raises a JlllException.")
        {
            private static final long serialVersionUID = 8273645091827364502L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                StringBuilder sb = new StringBuilder();
                for (Object val : values)
                {
                    sb.append(val);
                }
                throw new JlllException(sb.toString());
            }
        };
        new Primitive("exception?", env,
                "Tests if a value is an exception. (exception? x) returns true if x is a Throwable.")
        {
            private static final long serialVersionUID = 8273645091827364503L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("exception? requires exactly one argument");
                }
                return values.get(0) instanceof Throwable;
            }
        };
        new Primitive("exception-message", env,
                "Returns the message of an exception. (exception-message e) returns the error message string.")
        {
            private static final long serialVersionUID = 8273645091827364504L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("exception-message requires exactly one argument");
                }
                Object err = values.get(0);
                if (err instanceof Throwable)
                {
                    return ((Throwable) err).getMessage();
                }
                throw new JlllException("exception-message requires a Throwable, got: " + err.getClass().getName());
            }
        };
        new Primitive("exception-cause", env,
                "Returns the underlying cause of an exception. (exception-cause e) returns the wrapped Throwable or null.")
        {
            private static final long serialVersionUID = 8273645091827364505L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("exception-cause requires exactly one argument");
                }
                Object err = values.get(0);
                if (err instanceof Throwable)
                {
                    Throwable cause = ((Throwable) err).getCause();
                    return cause != null ? cause : Null.NULL;
                }
                throw new JlllException("exception-cause requires a Throwable, got: " + err.getClass().getName());
            }
        };
        // ============== try/catch/finally Special Form ==============
        new Primitive("try", env,
                "Exception handling. (try body... (catch [type] var handler...) (finally cleanup...)). "
                        + "Executes body, catches exceptions matching type (string class name or predicate), "
                        + "binds to var and runs handler. Finally always runs.")
        {
            private static final long serialVersionUID = 8273645091827364506L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Parse: separate body, catch clauses, and finally clause
                List<Object> bodyForms = new ArrayList<>();
                List<Cons> catchClauses = new ArrayList<>();
                Cons finallyClause = null;
                Symbol catchSym = Symbol.intern("catch");
                Symbol finallySym = Symbol.intern("finally");
                for (Object form : values)
                {
                    if (form instanceof Cons)
                    {
                        Cons c = (Cons) form;
                        if (!c.isNull())
                        {
                            Object car = c.car();
                            if (catchSym.equals(car))
                            {
                                catchClauses.add(c);
                                continue;
                            }
                            else if (finallySym.equals(car))
                            {
                                if (finallyClause != null)
                                {
                                    throw new JlllException("try: multiple finally clauses not allowed");
                                }
                                finallyClause = c;
                                continue;
                            }
                        }
                    }
                    bodyForms.add(form);
                }
                Object result = null;
                JlllException caughtException = null;
                try
                {
                    // Evaluate body forms
                    for (Object form : bodyForms)
                    {
                        result = Evaluator.eval(form, env);
                    }
                }
                catch (JlllException e)
                {
                    // Continuations must propagate to their call/cc - not errors
                    if (e.getSource() instanceof Continuation)
                    {
                        throw e;
                    }
                    caughtException = e;
                    // Try to find a matching catch clause
                    for (Cons catchClause : catchClauses)
                    {
                        // Parse catch clause: (catch [type-spec] var handler...)
                        // Formats:
                        //   (catch var handler...)           - catch all
                        //   (catch "ClassName" var handler...) - match by class name
                        //   (catch predicate var handler...) - match by predicate
                        Cons rest = (Cons) catchClause.cdr();
                        Object first = rest.car();
                        Object typeSpec = null;
                        Symbol var;
                        Cons handler;
                        if (first instanceof Symbol)
                        {
                            // Catch-all: (catch var handler...)
                            var = (Symbol) first;
                            handler = (Cons) rest.cdr();
                        }
                        else
                        {
                            // Type-specific: (catch type-spec var handler...)
                            typeSpec = first;
                            Cons afterType = (Cons) rest.cdr();
                            Object varObj = afterType.car();
                            if (!(varObj instanceof Symbol))
                            {
                                throw new JlllException("catch: variable must be a symbol, got: " + varObj);
                            }
                            var = (Symbol) varObj;
                            handler = (Cons) afterType.cdr();
                        }
                        // Check if this clause matches
                        boolean matches = false;
                        if (typeSpec == null)
                        {
                            // Catch-all always matches
                            matches = true;
                        }
                        else if (typeSpec instanceof String)
                        {
                            // Match by class name
                            String className = (String) typeSpec;
                            try
                            {
                                Class<?> targetClass = Class.forName(className);
                                // Check if the exception or its cause matches
                                matches = targetClass.isAssignableFrom(e.getClass());
                                if (!matches && e.getCause() != null)
                                {
                                    matches = targetClass.isAssignableFrom(e.getCause().getClass());
                                }
                            }
                            catch (ClassNotFoundException cnf)
                            {
                                throw new JlllException("catch: unknown exception class: " + className, cnf);
                            }
                        }
                        else
                        {
                            // Evaluate type-spec as predicate
                            Object predValue = Evaluator.eval(typeSpec, env);
                            if (predValue instanceof Procedure)
                            {
                                Object predResult = ((Procedure) predValue).applyEvaluated(env, e);
                                matches = CommonUtil.getBoolean(predResult);
                            }
                            else
                            {
                                throw new JlllException(
                                        "catch: type specifier must be a string or procedure, got: " + typeSpec);
                            }
                        }
                        if (matches)
                        {
                            // Bind exception to var and evaluate handler
                            Environment catchEnv = new Environment(env);
                            catchEnv.addBinding(var, e);
                            // Evaluate handler body
                            Object handlerBody = new Cons(Symbol.BEGIN, handler);
                            result = Evaluator.eval(handlerBody, catchEnv);
                            caughtException = null; // Exception was handled
                            break;
                        }
                    }
                }
                finally
                {
                    // Execute finally clause if present
                    if (finallyClause != null)
                    {
                        Cons finallyBody = (Cons) finallyClause.cdr();
                        Object finallyExpr = new Cons(Symbol.BEGIN, finallyBody);
                        Evaluator.eval(finallyExpr, env);
                    }
                }
                // Re-throw if not handled
                if (caughtException != null)
                {
                    throw caughtException;
                }
                return result;
            }
        };
        // ============== guard Special Form (Scheme-style) ==============
        new Primitive("guard", env,
                "Scheme-style exception handling. (guard (var (test handler...) ... (else default...)) body...). "
                        + "Evaluates body, on exception binds it to var and tests clauses like cond.")
        {
            private static final long serialVersionUID = 8273645091827364507L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Parse: (guard (var clause...) body...)
                if (values.length() < 2)
                {
                    throw new JlllException("guard requires at least a clause-spec and body");
                }
                Object clauseSpecObj = values.car();
                if (!(clauseSpecObj instanceof Cons))
                {
                    throw new JlllException("guard: first argument must be (var clause...)");
                }
                Cons clauseSpec = (Cons) clauseSpecObj;
                if (clauseSpec.isNull())
                {
                    throw new JlllException("guard: clause-spec cannot be empty");
                }
                Object varObj = clauseSpec.car();
                if (!(varObj instanceof Symbol))
                {
                    throw new JlllException("guard: variable must be a symbol, got: " + varObj);
                }
                Symbol var = (Symbol) varObj;
                Cons clauses = (Cons) clauseSpec.cdr();
                Cons body = (Cons) values.cdr();
                try
                {
                    // Evaluate body
                    Object bodyExpr = new Cons(Symbol.BEGIN, body);
                    return Evaluator.eval(bodyExpr, env);
                }
                catch (JlllException e)
                {
                    // Continuations must propagate to their call/cc - not errors
                    if (e.getSource() instanceof Continuation)
                    {
                        throw e;
                    }
                    // Bind exception to var
                    Environment guardEnv = new Environment(env);
                    guardEnv.addBinding(var, e);
                    // Evaluate clauses like cond
                    for (Object clause : clauses)
                    {
                        if (!(clause instanceof Cons))
                        {
                            throw new JlllException("guard: clause must be a list");
                        }
                        Cons c = (Cons) clause;
                        Object test = c.car();
                        Cons handlerBody = (Cons) c.cdr();
                        boolean matches;
                        if (Symbol.intern("else").equals(test))
                        {
                            matches = true;
                        }
                        else
                        {
                            Object testResult = Evaluator.eval(test, guardEnv);
                            matches = CommonUtil.getBoolean(testResult);
                        }
                        if (matches)
                        {
                            Object handlerExpr = new Cons(Symbol.BEGIN, handlerBody);
                            return Evaluator.eval(handlerExpr, guardEnv);
                        }
                    }
                    // No matching clause - re-raise
                    throw e;
                }
            }
        };
        // ============== with-exception-handler (SRFI-34 style) ==============
        new Primitive("with-exception-handler", env,
                "Installs handler for dynamic extent of thunk. "
                        + "(with-exception-handler handler thunk). Handler is called with exception; "
                        + "exception propagates after handler returns unless handler escapes.")
        {
            private static final long serialVersionUID = 8273645091827364510L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 2)
                {
                    throw new JlllException("with-exception-handler requires exactly 2 arguments");
                }
                Object handlerObj = values.get(0);
                Object thunkObj = values.get(1);
                if (!(handlerObj instanceof Procedure))
                {
                    throw new JlllException("with-exception-handler: handler must be a procedure");
                }
                if (!(thunkObj instanceof Procedure))
                {
                    throw new JlllException("with-exception-handler: thunk must be a procedure");
                }
                Procedure handler = (Procedure) handlerObj;
                Procedure thunk = (Procedure) thunkObj;
                try
                {
                    // Call thunk with no arguments
                    return thunk.applyEvaluated(new Cons(), env);
                }
                catch (JlllException e)
                {
                    // Continuations must propagate - not errors
                    if (e.getSource() instanceof Continuation)
                    {
                        throw e;
                    }
                    // Call handler with exception
                    handler.applyEvaluated(env, e);
                    // Re-raise the exception (SRFI-34 semantics)
                    throw e;
                }
            }
        };
        // ============== Environment Operations ==============
        new Primitive("env", env,
                "Environment operations. Multiple forms: " + "(env) - prints all bindings grouped by type. "
                        + "(env \"prefix\") - filters bindings by name prefix. "
                        + "(env :primitives|:macros|:procedures|:variables) - filters by type. "
                        + "(env :depends '(\"group:artifact:version\" ...) body...) - executes body in child "
                        + "environment with Maven dependencies loaded. Returns result of body. "
                        + "(env :depends '(...)) - without body, returns the environment object for later use. "
                        + "(env :depends '(...) :repos '(\"url\" ...) body...) - with custom Maven repositories.")
        {
            private static final long serialVersionUID = 8273645091827364520L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                // Check for :depends keyword - special form (don't evaluate args yet)
                if (values.length() > 0)
                {
                    Object first = values.get(0);
                    if (first instanceof Keyword && ((Keyword) first).getName().equals("depends"))
                    {
                        return handleEnvDepends(values, env);
                    }
                }
                // Otherwise, evaluate arguments and use introspection mode
                Cons evaluated = Jlll.evalEvery(values, env);
                return applyEvaluated(evaluated, env);
            }

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Console console = getConsole(env);
                Map<Symbol, Object> bindings = env.getAllBindings();
                String prefixFilter = null;
                String typeFilter = null;
                // Parse arguments
                if (values.length() > 0)
                {
                    Object arg = values.get(0);
                    if (arg instanceof String)
                    {
                        prefixFilter = (String) arg;
                    }
                    else if (arg instanceof Keyword)
                    {
                        typeFilter = ((Keyword) arg).getName();
                    }
                    else if (arg instanceof Symbol)
                    {
                        // Allow (env 'primitives) as well as (env :primitives)
                        typeFilter = ((Symbol) arg).getName();
                    }
                    else
                    {
                        throw new JlllException(
                                "env argument must be a string prefix or type keyword (:primitives, :macros, :procedures, :variables)");
                    }
                }
                // Collect bindings by type
                List<Map.Entry<Symbol, Object>> primitivesList = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> macrosList = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> proceduresList = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> variablesList = new ArrayList<>();
                for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
                {
                    String name = entry.getKey().getName();
                    // Apply prefix filter if specified
                    if (prefixFilter != null && !name.contains(prefixFilter))
                    {
                        continue;
                    }
                    Object value = entry.getValue();
                    if (value instanceof Primitive)
                    {
                        primitivesList.add(entry);
                    }
                    else if (value instanceof Macros)
                    {
                        macrosList.add(entry);
                    }
                    else if (value instanceof Procedure)
                    {
                        proceduresList.add(entry);
                    }
                    else
                    {
                        variablesList.add(entry);
                    }
                }
                // Print header
                console.println();
                int total = primitivesList.size() + macrosList.size() + proceduresList.size() + variablesList.size();
                String header;
                if (prefixFilter != null)
                {
                    header = "Bindings matching \"" + prefixFilter + "\" (" + total + "):";
                }
                else if (typeFilter != null)
                {
                    header = "Environment bindings (" + typeFilter + "):";
                }
                else
                {
                    header = "Environment bindings (" + bindings.size() + " total):";
                }
                console.printHeader(header);
                console.println();
                // Print groups based on type filter
                if (typeFilter == null || typeFilter.equals("primitives"))
                {
                    printGroup(console, env, "Primitives", primitivesList, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("macros"))
                {
                    printGroup(console, env, "Macros", macrosList, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("procedures"))
                {
                    printGroup(console, env, "Procedures", proceduresList, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("variables"))
                {
                    printGroup(console, env, "Variables", variablesList, typeFilter != null);
                }
                if (prefixFilter == null && typeFilter == null)
                {
                    console.printHint("Use (env \"prefix\") to filter by name.");
                    console.println();
                }
                console.println();
                console.flush();
                return Null.NULL;
            }

            private void printGroup(Console console, Environment env, String groupName,
                    List<Map.Entry<Symbol, Object>> entries, boolean skipHeader)
            {
                if (entries.isEmpty())
                {
                    return;
                }
                if (!skipHeader)
                {
                    console.printColored(groupName + " (" + entries.size() + "):", Console.Color.CYAN);
                    console.println();
                }
                // Sort alphabetically
                entries.sort((a, b) -> a.getKey().getName().compareTo(b.getKey().getName()));
                for (Map.Entry<Symbol, Object> entry : entries)
                {
                    String name = entry.getKey().getName();
                    String docExcerpt = getDocExcerpt(env, entry.getKey(), entry.getValue());
                    console.print("  ");
                    console.printName(String.format("%-16s", name));
                    console.print(" ");
                    console.println(docExcerpt);
                }
                console.println();
            }

            private String getDocExcerpt(Environment env, Symbol sym, Object value)
            {
                Object doc = env.getMeta(sym, Symbol.intern("doc"));
                if (doc != null && !doc.toString().isEmpty())
                {
                    String text = doc.toString();
                    int newline = text.indexOf('\n');
                    if (newline > 0)
                    {
                        return text.substring(0, newline);
                    }
                    // Truncate long docs
                    if (text.length() > 80)
                    {
                        return text.substring(0, 77) + "...";
                    }
                    return text;
                }
                // Fallback descriptions
                if (value instanceof Primitive)
                {
                    return "";
                }
                else if (value instanceof Macros)
                {
                    return "";
                }
                else if (value instanceof Procedure)
                {
                    return "";
                }
                else if (value instanceof Boolean)
                {
                    return "Boolean: " + value;
                }
                else if (value instanceof Number)
                {
                    return value.getClass().getSimpleName() + ": " + value;
                }
                else if (value instanceof String)
                {
                    String s = (String) value;
                    if (s.length() > 20)
                    {
                        return "String: \"" + s.substring(0, 17) + "...\"";
                    }
                    return "String: \"" + s + "\"";
                }
                else if (value == null || value instanceof Null)
                {
                    return "nil";
                }
                else
                {
                    return value.getClass().getSimpleName();
                }
            }
        };
        // ============== env-switch! - Switch current environment ==============
        new Primitive("env-switch!", env,
                "Switches the current REPL/evaluation environment. "
                        + "(env-switch! new-env) switches to the specified environment. "
                        + "(env-switch!) with no arguments switches to the parent environment. "
                        + "Returns the environment that was switched to.")
        {
            private static final long serialVersionUID = 8273645091827364521L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Environment targetEnv;
                if (values.length() == 0)
                {
                    // No argument: switch to parent
                    targetEnv = env.getParent();
                    if (targetEnv == null)
                    {
                        throw new JlllException("Cannot switch to parent: already at top environment");
                    }
                }
                else
                {
                    Object arg = values.get(0);
                    if (!(arg instanceof Environment))
                    {
                        throw new JlllException("env-switch! argument must be an Environment, got: "
                                + (arg == null ? "null" : arg.getClass().getSimpleName()));
                    }
                    targetEnv = (Environment) arg;
                }
                // Set *current-env* binding to the target environment
                // This allows REPL and other tools to detect the switch
                env.getTopEnvironment().setBinding(Symbol.intern("*current-env*"), targetEnv);
                return targetEnv;
            }
        };
        // ============== env-classpath - Get environment's classpath ==============
        new Primitive("env-classpath", env,
                "Returns the JAR files in an environment's classpath. "
                        + "(env-classpath) returns current environment's JARs. "
                        + "(env-classpath env) returns the specified environment's JARs. "
                        + "Returns a list of file path strings.")
        {
            private static final long serialVersionUID = 8273645091827364522L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Environment targetEnv = env;
                if (values.length() > 0)
                {
                    Object arg = values.get(0);
                    if (!(arg instanceof Environment))
                    {
                        throw new JlllException("env-classpath argument must be an Environment, got: "
                                + (arg == null ? "null" : arg.getClass().getSimpleName()));
                    }
                    targetEnv = (Environment) arg;
                }
                List<File> jars = targetEnv.getAllClasspathJars();
                if (jars.isEmpty())
                {
                    return Null.NULL;
                }
                Object[] paths = new Object[jars.size()];
                for (int i = 0; i < jars.size(); i++)
                {
                    paths[i] = jars.get(i).getAbsolutePath();
                }
                return ListUtil.arrayToCons(paths);
            }
        };
        // ============== env-parent - Get parent environment ==============
        new Primitive("env-parent", env,
                "Returns the parent environment. " + "(env-parent) returns current environment's parent. "
                        + "(env-parent env) returns the specified environment's parent. "
                        + "Returns null if at top environment.")
        {
            private static final long serialVersionUID = 8273645091827364523L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Environment targetEnv = env;
                if (values.length() > 0)
                {
                    Object arg = values.get(0);
                    if (!(arg instanceof Environment))
                    {
                        throw new JlllException("env-parent argument must be an Environment, got: "
                                + (arg == null ? "null" : arg.getClass().getSimpleName()));
                    }
                    targetEnv = (Environment) arg;
                }
                Environment parent = targetEnv.getParent();
                return parent != null ? parent : Null.NULL;
            }
        };
        // ============== env? - Test if value is an Environment ==============
        new Primitive("env?", env,
                "Tests if a value is an Environment object. (env? x) returns true if x is an Environment.")
        {
            private static final long serialVersionUID = 8273645091827364524L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("env? requires exactly one argument");
                }
                return values.get(0) instanceof Environment;
            }
        };
        // ============== call/cc (call-with-current-continuation) ==============
        new Primitive("call/cc", env,
                "Calls procedure with the current continuation. "
                        + "(call/cc (lambda (k) body...)) - k is a procedure that, when called, "
                        + "returns its argument as the result of the call/cc expression. "
                        + "The continuation can be saved and called multiple times.")
        {
            private static final long serialVersionUID = 8273645091827364508L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                {
                    throw new JlllException("call/cc requires exactly one argument (a procedure)");
                }
                // Evaluate the procedure argument
                Object procObj = Evaluator.eval(values.car(), env);
                if (!(procObj instanceof Procedure))
                {
                    throw new JlllException("call/cc argument must be a procedure");
                }
                Procedure proc = (Procedure) procObj;
                // Create the continuation object
                Continuation k = new Continuation(env);
                // Capture the PARENT context (the expression containing call/cc)
                // Current context is (call/cc ...), parent is (+ 1 (call/cc ...))
                Evaluator.EvalContext ctx = Evaluator.getEvalContext();
                if (ctx != null && ctx.parent != null)
                {
                    Evaluator.EvalContext parent = ctx.parent;
                    k.setOuterContext(parent.expression, parent.argumentPosition, parent.evaluatedArguments);
                }
                try
                {
                    // Call (proc k)
                    return proc.applyEvaluated(env, k);
                }
                catch (JlllException exc)
                {
                    // Check if this is OUR continuation being invoked
                    if (exc.getSource() == k)
                    {
                        // Identity check: this exception is from our continuation
                        // Mark as captured for future invocations
                        k.markCaptured();
                        // Return the value passed to the continuation
                        return exc.getValue();
                    }
                    // Not our continuation (or not a continuation at all)
                    // Propagate the exception
                    throw exc;
                }
            }
        };
        // Add alias for call/cc
        env.cloneBinding("call-with-current-continuation", "call/cc");
        Jlll.eval("(load-system-script \"kernel.jlll\")", env);
        String extLib = null;
        extLib = System.getProperty("jlll.extension");
        if (extLib != null)
            Jlll.invokeProcedure("load-lib", env, extLib);
    }
}
