package ru.ydn.jlll.common;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Environment (sory for mistake in Class name) is a data structure
 * to store variables of the calculation environment
 *
 */
public class Enviroment implements Serializable
{
    private static final long serialVersionUID = 5024478830439245581L;
    protected Enviroment parent = null;
    protected Map<Symbol, Object> current = new HashMap<Symbol, Object>();
    /**
     * top Enviroment
     */
    public static Enviroment top = null;
    static
    {
        String topClass = null;
        try
        {
            topClass = System.getProperty("jlll.top-class");
        }
        catch (SecurityException e)
        {
            //NOP
        }
        if (topClass != null && topClass.trim().length() > 0)
        {
            try
            {
                Class<?> clazz = Class.forName(topClass);
                Constructor<?> constructor = clazz.getConstructor(new Class[]
                {Enviroment.class});
                top = (Enviroment) constructor.newInstance(new Object[]
                {null});
            }
            catch (Exception e)
            {
                throw new RuntimeException("Can't initialize enviroment. Check \"jlll.top-class\" property.", e);
            }
        }
        else
        {
            top = new Enviroment(null);
        }
        //        top = new SkyNetEnviroment(null);
        new Primitive("load-lib", top)
        {
            private static final long serialVersionUID = -7181172149106048903L;

            public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
            {
                String path = values.get(0).toString();
                try
                {
                    Object lib = Class.forName(path).newInstance();
                    if (!(lib instanceof Library))
                        throw new JlllException("This is not a library" + path);
                    Library library = (Library) lib;
                    library.load(env);
                }
                catch (JlllException e)
                {
                    throw e;
                }
                catch (Exception e)
                {
                    throw new JlllException("Can't load " + path, e);
                }
                return null;
            }
        };
        try
        {
            Jlll.eval(Evaluator.class.getResourceAsStream("init.jlll"));
        }
        catch (JlllException e)
        {
            e.printStackTrace();
        }
    }

    /**
     * Creates an environment with the specified parent environment
     *
     * @param parent
     *            parent environment
     */
    public Enviroment(Enviroment parent)
    {
        this.parent = parent;
    }

    /**
     * Adds new binding to the environmet
     *
     * @param symbolName
     *            Symbol name to bind to
     * @param obj
     *            Object to bind
     */
    public final void addBinding(String symbolName, Object obj)
    {
        addBinding(Symbol.intern(symbolName), obj);
    }

    /**
     * Adds new binding to the environmet
     *
     * @param sym
     *            Symbol to bind to
     * @param obj
     *            Object to bind
     */
    public void addBinding(Symbol sym, Object obj)
    {
        current.put(sym, obj);
    }

    /**
     * Lookups an object for the symbolName
     *
     * @param symbolName
     *            symbol name of the required objects
     * @return lookuped object or <tt>null</tt>
     */
    public final Object lookup(String symbolName)
    {
        return lookup(Symbol.intern(symbolName));
    }

    /**
     * Lookups an object for the symbol
     *
     * @param sym
     *            symbol of the required objects
     * @return lookuped object or <tt>null</tt>
     */
    public final Object lookup(Symbol sym)
    {
        return lookup(sym, new ArrayList<Enviroment>(5));
    }

    protected Object lookup(Symbol sym, List<Enviroment> trace)
    {
        if (sym == null)
            return null;
        Object ret = current.get(sym);
        if (ret == null && parent != null && !trace.contains(parent))
        {
            trace.add(this);
            ret = parent.lookup(sym, trace);
        }
        return ret;
    }

    /**
     * Returns all bindings with parent environments
     *
     * @return all bindings with parent environments
     */
    public Map<Symbol, Object> getAllBindings()
    {
        Map<Symbol, Object> ret = new HashMap<Symbol, Object>();
        if (parent != null)
        {
            ret.putAll(parent.getAllBindings());
        }
        ret.putAll(current);
        return ret;
    }

    /**
     * Returns top environment
     *
     * @return top environment
     */
    public Enviroment getTopEnvironment()
    {
        if (parent != null)
        {
            return parent.getTopEnvironment();
        }
        else
        {
            return this;
        }
    }

    /**
     * Remove binding from environment where symbol is binded
     *
     * @param sym
     *            symbol name to unbind
     */
    public void removeBinding(String symbolName)
    {
        removeBinding(Symbol.intern(symbolName));
    }

    /**
     * Remove binding from environment where symbol is binded
     *
     * @param sym
     *            symbol name to unbind
     */
    public void removeBinding(Symbol sym)
    {
        if (current.remove(sym) == null && parent != null)
        {
            parent.removeBinding(sym);
        }
    }

    public void clear()
    {
        current.clear();
        if (parent != null)
            parent.clear();
    }

    /**
     * Sets binding in environment where symbol was initialy binded
     * or in top environment
     *
     * @param sym
     *            Symbol to bind to
     * @param obj
     *            Object to bind
     */
    public void setBinding(Symbol sym, Object obj)
    {
        if (current.containsKey(sym))
        {
            addBinding(sym, obj);
        }
        else
        {
            if (parent != null)
            {
                parent.setBinding(sym, obj);
            }
            else
            {
                addBinding(sym, obj);
            }
        }
    }

    /**
     * Returns parent environment
     *
     * @return parent environment
     */
    public Enviroment getParent()
    {
        return parent;
    }

    /**
     * Returns the level of this environment
     *
     * @return the level of this environment
     */
    public int getLevel()
    {
        return parent == null ? 0 : parent.getLevel() + 1;
    }

    /**
     * Check wheter this environment empty
     *
     * @return
     */
    public boolean isEmpty()
    {
        return !(current != null && !current.isEmpty()) && (parent == null ? true : parent.isEmpty());
    }
}
