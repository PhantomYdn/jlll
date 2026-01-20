package ru.ydn.jlll.libs;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import ru.ydn.jlll.common.CompaundProcedure;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ParameterParser;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;
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
    public void load(Enviroment env) throws JlllException
    {
        env.addBinding(Symbol.TRUE, Boolean.TRUE);
        //       env.addBinding(Symbol.TRUE2, new Boolean(true));
        env.addBinding(Symbol.FALSE, Boolean.FALSE);
        //       env.addBinding(Symbol.FALSE2, new Boolean(false));
        env.addBinding(Symbol.NULL, Null.NULL);
        new Primitive("define", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -7343798584300580807L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
                    CompaundProcedure procedure = new CompaundProcedure(variables, body, env);
                    env.addBindingWithMeta(Symbol.intern(name), procedure, metadata);
                    return procedure;
                }
                throw new JlllException("define: first argument must be symbol or list, got: " + first);
            }
        };
        new Primitive("apply", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -5122951526914178471L;

            @Override
            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Cons args = (Cons) values.get(1);
                //System.out.println(args);
                return proc.applayEvaluated(args, env);
            }
        };
        new Primitive("set", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 7346093880732180126L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
                    CompaundProcedure procedure = new CompaundProcedure(variables, body);
                    env.setBinding(Symbol.intern(name), procedure);
                    return procedure;
                }
                return null;
            }
        };
        env.addBinding("set!", env.lookup("set"));
        new Primitive("defmacro", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 1953809997016714629L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Object car = values.car();
                if (!(car instanceof Cons))
                {
                    throw new JlllException("Symbol in defmacro is not allowed");
                }
                else
                {
                    Cons def = (Cons) car;
                    String name = def.get(0).toString();
                    Object variables = def.cdr();
                    Object body = values.tail(1);
                    Macros macros = new Macros(variables, body);
                    env.addBinding(Symbol.intern(name), macros);
                    return macros;
                }
            }
        };
        new Primitive("if", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 8411805766481542038L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
        new Primitive("cond", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -83499329550871262L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
        new Primitive("case", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -8203023265311714220L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
        new Primitive("lambda", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -840641725868209645L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Object variables = values.get(0);
                Object body = values.tail(1);
                // Pass env for evaluating !defaults
                CompaundProcedure procedure = new CompaundProcedure(variables, body, env);
                return procedure;
            }
        };
        new Primitive("cons", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -7652629513702176997L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                return new Cons(values.get(0), values.get(1));
            }
        };
        new Primitive("begin", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 2744358405000249124L;

            public Object applay(Cons values, Enviroment env) throws JlllException
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
        new Primitive("quote", env)
        {
            private static final long serialVersionUID = -8365426058879553368L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                if (values.length() > 1)
                    throw new JlllException("So many parameters");
                return values.car();
            }
        };
        new Primitive("car", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 6224440278990904488L;

            public Object applay(Cons vaCons, Enviroment env) throws JlllException
            {
                Object obj = Evaluator.eval(vaCons.car(), env);
                if (!(obj instanceof Cons))
                    throw new JlllException("This is not a list: " + vaCons.car());
                return ((Cons) obj).car();
            }
        };
        new Primitive("cdr", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 3572491558064466506L;

            public Object applay(Cons vaCons, Enviroment env) throws JlllException
            {
                Object obj = Evaluator.eval(vaCons.car(), env);
                if (!(obj instanceof Cons))
                    throw new JlllException("This is not a list: " + vaCons.car());
                return ((Cons) obj).cdr();
            }
        };
        new Primitive("current-environment", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 8816274024795448776L;

            public Object applay(Object vaCons, Enviroment env) throws JlllException
            {
                return ListUtil.toCons(env.getAllBindings().keySet().toArray());
            }
        };
        new Primitive("top-environment", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -2819367499932684513L;

            public Object applay(Object vaCons, Enviroment env) throws JlllException
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
         * public Object applay(Object vaCons, Enviroment env) throws JlllException
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
        new Primitive("load-system-script", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -8036963916533498346L;

            public Object applay(Cons vaCons, Enviroment env) throws JlllException
            {
                Jlll.eval(KernelLib.class.getResourceAsStream(Evaluator.eval(vaCons.get(0), env).toString()));
                return null;
            }
        };
        new Primitive("load-url", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -8736628261895814580L;

            public Object applayEvaluated(Cons vaCons, Enviroment env) throws JlllException
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
        new Primitive("quasiquote", env)
        {
            /**
             *
             */
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

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Object what = values.get(0);
                return evalQQ(what, env).object;
            }

            private RetObject evalQQ(Object what, Enviroment env) throws JlllException
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
        new Primitive("eval", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -8630743653655996673L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                //                System.out.println(values);
                return Evaluator.eval(values.get(0), env);
            }
        };
        new Primitive("time", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 1296484298301550805L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
        new Primitive("sleep", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 5958807729111336106L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Object toEval = values.get(0);
                try
                {
                    Thread.sleep(((Number) toEval).longValue());
                }
                catch (InterruptedException e)
                {
                    //NOP!
                }
                return null;
            }
        };
        new Primitive("concat", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -2789439764629510145L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Iterator<?> it = values.iterator();
                String ret = "";
                while (it.hasNext())
                {
                    Object val = it.next();
                    ret += val;
                }
                return ret;
            }
        };
        new Primitive("map", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -6920908225941152717L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Iterator<?> it = ((Cons) values.get(1)).iterator();
                List<Object> ret = new ArrayList<Object>(values.length() - 1);
                while (it.hasNext())
                {
                    Object next = it.next();
                    ret.add(proc.applayEvaluated(env, next));
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("mapall", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 7289150232747124300L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
                 * ret.add(proc.applay(env, next));
                 *
                 *
                 * }
                 *
                 * return ListUtil.arrayToCons(ret.toArray());
                 */
            }

            private Cons mapAll(Procedure proc, Cons cons, Enviroment env) throws JlllException
            {
                Iterator<?> it = cons.iterator();
                List<Object> ret = new ArrayList<Object>(cons.length());
                while (it.hasNext())
                {
                    Object next = it.next();
                    Object toSet = (next instanceof Cons && !((Cons) next).isNull())
                            ? mapAll(proc, (Cons) next, env)
                            : proc.applay(new Cons(next), env);
                    ret.add(toSet);
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("filter", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 6564713665261707057L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Iterator<?> it = ((Cons) values.get(1)).iterator();
                List<Object> ret = new ArrayList<Object>(values.length() - 1);
                while (it.hasNext())
                {
                    Object next = it.next();
                    if (CommonUtil.getBoolean(proc.applayEvaluated(env, next)))
                        ret.add(next);
                }
                return ListUtil.arrayToCons(ret.toArray());
            }
        };
        new Primitive("jlll-extract-body", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 4782501546001349950L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                Object function = values.get(0);
                if (!(function instanceof CompaundProcedure))
                    throw new JlllException("Function isn't compaund: " + function.getClass());
                return ((CompaundProcedure) function).getBody();
            }
        };
        new Primitive("jlll-macro-expand", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 3835175014722360095L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                Object function = Evaluator.eval(values.get(0), env);
                if (!(function instanceof Macros))
                    throw new JlllException("Function isn't macros: " + function.getClass());
                return ((Macros) function).macroExpand(values.tail(1), env);
            }
        };
        new Primitive("describe", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 5164474211503405192L;

            @Override
            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
                    // Append metadata if present
                    if (env.hasMeta(sym))
                    {
                        Map<Symbol, Object> meta = env.getAllMeta(sym);
                        sb.append("\nMetadata:");
                        for (Map.Entry<Symbol, Object> entry : meta.entrySet())
                        {
                            sb.append("\n  :").append(entry.getKey().getName()).append(" ").append(entry.getValue());
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
        new Primitive("exlamation", env)
        {
            private static final long serialVersionUID = 1827364509182736450L;

            public Object applay(Cons values, Enviroment env) throws JlllException
            {
                if (values.length() != 1)
                    throw new JlllException("exlamation requires exactly one argument");
                return Evaluator.eval(values.car(), env);
            }
        };
        // ============== Metadata Primitives ==============
        new Primitive("doc", env, "Returns documentation for a symbol or procedure. "
                + "Documentation is stored as :doc metadata on bindings.")
        {
            private static final long serialVersionUID = 9182736450918273645L;

            @Override
            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                if (values.length() != 1)
                    throw new JlllException("doc requires exactly one argument");
                Object arg = values.get(0);
                Symbol sym;
                if (arg instanceof Symbol)
                {
                    sym = (Symbol) arg;
                }
                else if (arg instanceof Procedure)
                {
                    // For procedures passed directly, try to get their doc
                    String doc = ((Procedure) arg).getDoc();
                    return (doc != null && !doc.isEmpty()) ? doc : null;
                }
                else
                {
                    throw new JlllException("doc requires a symbol or procedure");
                }
                // Primary source: metadata on the binding
                Object doc = env.getMeta(sym, Symbol.intern("doc"));
                if (doc != null)
                {
                    return doc;
                }
                // Fallback: Procedure.getDoc() for backward compatibility
                Object value = env.lookup(sym);
                if (value instanceof Procedure)
                {
                    String procDoc = ((Procedure) value).getDoc();
                    if (procDoc != null && !procDoc.isEmpty())
                    {
                        return procDoc;
                    }
                }
                return null;
            }
        };
        new Primitive("meta", env, "Returns metadata for a symbol. "
                + "(meta sym) returns all metadata as alist; (meta sym :key) returns specific value.")
        {
            private static final long serialVersionUID = 9182736450918273646L;

            @Override
            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
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
            public Object applay(Cons values, Enviroment env) throws JlllException
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
        Jlll.eval("(load-system-script \"kernel.jlll\")", env);
        String extLib = null;
        extLib = System.getProperty("jlll.extension");
        if (extLib != null)
            Jlll.invokeProcedure("load-lib", env, extLib);
    }
}
