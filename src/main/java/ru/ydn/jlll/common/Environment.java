package ru.ydn.jlll.common;

import java.io.File;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

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
     * Custom classloader for this environment. If null, uses parent's classloader or system
     * classloader.
     */
    protected transient ClassLoader classLoader = null;
    /**
     * List of JAR files in this environment's classpath (for introspection).
     */
    protected transient List<File> classpathJars = null;
    /**
     * top Environment
     */
    public static Environment top = null;
    /**
     * Module registry - maps module names to their environments.
     * Thread-safe for concurrent module loading.
     */
    private static final ConcurrentMap<String, ModuleEnvironment> modules = new ConcurrentHashMap<>();

    /**
     * Registers a module in the global registry.
     *
     * @param name
     *            the module name
     * @param module
     *            the module environment
     */
    public static void registerModule(String name, ModuleEnvironment module)
    {
        modules.put(name, module);
    }

    /**
     * Looks up a module by name.
     *
     * @param name
     *            the module name
     * @return the module environment, or null if not found
     */
    public static ModuleEnvironment getModule(String name)
    {
        return modules.get(name);
    }

    /**
     * Checks if a module is registered.
     *
     * @param name
     *            the module name
     * @return true if the module exists
     */
    public static boolean hasModule(String name)
    {
        return modules.containsKey(name);
    }

    /**
     * Returns all registered module names.
     *
     * @return set of module names
     */
    public static Set<String> getModuleNames()
    {
        return modules.keySet();
    }

    /**
     * Removes a module from the registry.
     * Primarily for testing purposes.
     *
     * @param name
     *            the module name to remove
     * @return the removed module, or null if not found
     */
    public static ModuleEnvironment removeModule(String name)
    {
        return modules.remove(name);
    }

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
        new Primitive("load-lib", top,
                "Loads a Java library class. (load-lib \"com.example.MyLib\") "
                        + "instantiates the class and calls its load() method to register primitives. "
                        + "Uses the current environment's classloader to find the class.")
        {
            private static final long serialVersionUID = -7181172149106048903L;

            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String path = values.get(0).toString();
                try
                {
                    // Use environment's classloader to support dynamic dependencies
                    Class<?> clazz = env.loadClass(path);
                    Object lib = clazz.getDeclaredConstructor().newInstance();
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
     * Clones an existing binding to a new symbol name, copying both value and metadata.
     *
     * @param newName
     *            the new symbol name
     * @param sourceName
     *            the source symbol name to clone from
     */
    public void cloneBinding(String newName, String sourceName)
    {
        cloneBinding(Symbol.intern(newName), Symbol.intern(sourceName));
    }

    /**
     * Clones an existing binding to a new symbol name, copying both value and metadata.
     *
     * @param newSym
     *            the new symbol
     * @param sourceSym
     *            the source symbol to clone from
     */
    public void cloneBinding(Symbol newSym, Symbol sourceSym)
    {
        Object value = lookup(sourceSym);
        Map<Symbol, Object> meta = getAllMeta(sourceSym);
        addBindingWithMeta(newSym, value, meta.isEmpty() ? null : meta);
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
     * Returns whether this environment is transient (temporary scope).
     *
     * <p>
     * Transient environments are temporary scopes created during evaluation,
     * such as procedure call frames. Definitions made in transient environments
     * are lost when the scope exits.
     * </p>
     *
     * <p>
     * Override this method in subclasses that represent transient scopes.
     * </p>
     *
     * @return true if this is a transient environment, false otherwise
     */
    public boolean isTransient()
    {
        return false;
    }

    /**
     * Returns the nearest "user-level" environment by walking up the parent chain
     * and skipping transient environments.
     *
     * <p>
     * This is useful for operations that should affect the user's interactive
     * environment rather than a temporary procedure scope, such as AI tool definitions.
     * </p>
     *
     * @return the nearest non-transient ancestor, or this if already at user level
     */
    public Environment getUserEnvironment()
    {
        Environment current = this;
        while (current.isTransient() && current.getParent() != null)
        {
            current = current.getParent();
        }
        return current;
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
     * Sets the classloader for this environment.
     *
     * @param classLoader
     *            the classloader to use, or null to inherit from parent
     */
    public void setClassLoader(ClassLoader classLoader)
    {
        this.classLoader = classLoader;
    }

    /**
     * Sets the classloader and records the JAR files for introspection.
     *
     * @param classLoader
     *            the classloader to use
     * @param jars
     *            list of JAR files in the classpath
     */
    public void setClassLoader(ClassLoader classLoader, List<File> jars)
    {
        this.classLoader = classLoader;
        this.classpathJars = jars != null ? new ArrayList<>(jars) : null;
    }

    /**
     * Returns the classloader for this environment.
     *
     * <p>
     * If this environment has a custom classloader, it is returned. Otherwise, the parent's
     * classloader is used. If no parent has a custom classloader, returns the thread's context
     * classloader or the system classloader.
     * </p>
     *
     * @return the effective classloader for this environment
     */
    public ClassLoader getClassLoader()
    {
        if (classLoader != null)
        {
            return classLoader;
        }
        if (parent != null)
        {
            return parent.getClassLoader();
        }
        // Fall back to context classloader or system classloader
        ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
        return contextLoader != null ? contextLoader : ClassLoader.getSystemClassLoader();
    }

    /**
     * Returns the JAR files in this environment's classpath.
     *
     * @return list of JAR files, or empty list if no custom classpath
     */
    public List<File> getClasspathJars()
    {
        if (classpathJars != null)
        {
            return Collections.unmodifiableList(classpathJars);
        }
        return Collections.emptyList();
    }

    /**
     * Returns all JAR files in the classpath, including parent environments.
     *
     * @return combined list of JAR files from this environment and all parents
     */
    public List<File> getAllClasspathJars()
    {
        List<File> result = new ArrayList<>();
        if (parent != null)
        {
            result.addAll(parent.getAllClasspathJars());
        }
        if (classpathJars != null)
        {
            result.addAll(classpathJars);
        }
        return result;
    }

    /**
     * Checks if this environment has a custom classloader (not inherited).
     *
     * @return true if this environment has its own classloader
     */
    public boolean hasOwnClassLoader()
    {
        return classLoader != null;
    }

    /**
     * Loads a class using this environment's classloader.
     *
     * @param className
     *            fully qualified class name
     * @return the loaded class
     * @throws ClassNotFoundException
     *             if the class cannot be found
     */
    public Class<?> loadClass(String className) throws ClassNotFoundException
    {
        return Class.forName(className, true, getClassLoader());
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

    /**
     * Creates a snapshot of this environment for continuation capture.
     *
     * <p>
     * The snapshot preserves the current bindings but shares object references.
     * This matches Scheme semantics: continuations see mutations to mutable
     * objects, but don't see new bindings added after capture.
     * </p>
     *
     * @return a snapshot of this environment
     */
    public Environment snapshot()
    {
        Environment snap = new Environment(parent != null ? parent.snapshot() : null);
        snap.current = new HashMap<>(this.current);
        snap.metadata = new HashMap<>(this.metadata);
        // Share classloader reference (immutable from env perspective)
        snap.classLoader = this.classLoader;
        snap.classpathJars = this.classpathJars;
        return snap;
    }
}
