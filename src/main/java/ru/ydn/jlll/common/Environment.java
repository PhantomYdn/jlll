package ru.ydn.jlll.common;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Environment is a data structure to store variables of the calculation environment.
 */
public class Environment implements Serializable
{
    private static final long serialVersionUID = 5024478830439245582L;
    protected Environment parent = null;
    protected Map<Symbol, Object> current = new HashMap<Symbol, Object>();
    protected Map<Symbol, Map<Symbol, Object>> metadata = new HashMap<Symbol, Map<Symbol, Object>>();
    /**
     * top Environment
     */
    public static Environment top = null;
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
                {Environment.class});
                top = (Environment) constructor.newInstance(new Object[]
                {null});
            }
            catch (Exception e)
            {
                throw new RuntimeException("Can't initialize environment. Check \"jlll.top-class\" property.", e);
            }
        }
        else
        {
            top = new Environment(null);
        }
        //        top = new SkyNetEnvironment(null);
        new Primitive("load-lib", top)
        {
            private static final long serialVersionUID = -7181172149106048903L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String path = values.get(0).toString();
                try
                {
                    Object lib = Class.forName(path).getDeclaredConstructor().newInstance();
                    if (!(lib instanceof Library))
                        throw new JlllException("This is not a library: " + path);
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
    public Environment(Environment parent)
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
     * Adds new binding with metadata to the environment
     *
     * @param sym
     *            Symbol to bind to
     * @param obj
     *            Object to bind
     * @param meta
     *            Metadata map (Symbol keys to values), may be null
     */
    public void addBindingWithMeta(Symbol sym, Object obj, Map<Symbol, Object> meta)
    {
        current.put(sym, obj);
        if (meta != null && !meta.isEmpty())
        {
            metadata.put(sym, new HashMap<>(meta));
        }
    }

    /**
     * Sets a single metadata value on an existing binding.
     * Creates the binding's metadata map if needed.
     *
     * @param sym
     *            Symbol of the binding
     * @param key
     *            Metadata key
     * @param value
     *            Metadata value
     */
    public void setMeta(Symbol sym, Symbol key, Object value)
    {
        Map<Symbol, Object> meta = metadata.get(sym);
        if (meta == null)
        {
            // Check if binding exists in this env or parent
            if (!current.containsKey(sym))
            {
                if (parent != null && parent.lookup(sym) != null)
                {
                    // Binding is in parent, set metadata there
                    parent.setMeta(sym, key, value);
                    return;
                }
            }
            meta = new HashMap<>();
            metadata.put(sym, meta);
        }
        meta.put(key, value);
    }

    /**
     * Gets a metadata value for a binding.
     *
     * @param sym
     *            Symbol of the binding
     * @param key
     *            Metadata key
     * @return the metadata value, or null if not found
     */
    public Object getMeta(Symbol sym, Symbol key)
    {
        Map<Symbol, Object> meta = metadata.get(sym);
        if (meta != null)
        {
            Object value = meta.get(key);
            if (value != null)
            {
                return value;
            }
        }
        // Check parent environment
        if (parent != null)
        {
            return parent.getMeta(sym, key);
        }
        return null;
    }

    /**
     * Gets all metadata for a binding.
     *
     * @param sym
     *            Symbol of the binding
     * @return map of all metadata, or empty map if none
     */
    public Map<Symbol, Object> getAllMeta(Symbol sym)
    {
        Map<Symbol, Object> result = new HashMap<>();
        // Start with parent metadata (will be overwritten by local)
        if (parent != null)
        {
            result.putAll(parent.getAllMeta(sym));
        }
        // Add local metadata
        Map<Symbol, Object> local = metadata.get(sym);
        if (local != null)
        {
            result.putAll(local);
        }
        return result;
    }

    /**
     * Checks if a binding has any metadata.
     *
     * @param sym
     *            Symbol of the binding
     * @return true if metadata exists
     */
    public boolean hasMeta(Symbol sym)
    {
        if (metadata.containsKey(sym) && !metadata.get(sym).isEmpty())
        {
            return true;
        }
        return parent != null && parent.hasMeta(sym);
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
        return lookup(sym, new ArrayList<Environment>(5));
    }

    protected Object lookup(Symbol sym, List<Environment> trace)
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
    public Environment getTopEnvironment()
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

    /**
     * Clears all bindings in this environment and all parent environments.
     * Use with caution as this removes all definitions.
     */
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
    public Environment getParent()
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
