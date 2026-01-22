package ru.ydn.jlll.common;

import java.util.List;
import java.util.Map;

/**
 * An environment that provides positional argument access via $-prefixed symbols.
 *
 * <p>
 * This environment intercepts lookups for symbols like {@code $1}, {@code $2}, etc.
 * and returns the corresponding argument from the args array. The symbol {@code $0}
 * returns all arguments as a list.
 * </p>
 *
 * <p>
 * All other operations (add, set, remove) are delegated to the parent environment.
 * </p>
 */
public class ArgsEnvironment extends Environment
{
    private static final long serialVersionUID = 7866903478037016312L;
    /** The positional arguments accessible via $1, $2, etc. */
    private Object[] args;

    /**
     * Creates an args environment with positional argument bindings.
     *
     * @param parent
     *            the parent environment for non-argument lookups
     * @param args
     *            the arguments accessible via $1, $2, etc.
     */
    public ArgsEnvironment(Environment parent, Object[] args)
    {
        super(parent);
        this.args = args;
    }

    /** {@inheritDoc} Delegates to parent environment. */
    @Override
    public void addBinding(Symbol sym, Object obj)
    {
        getParent().addBinding(sym, obj);
    }

    /** {@inheritDoc} Returns parent's bindings. */
    @Override
    public Map<Symbol, Object> getAllBindings()
    {
        return getParent().getAllBindings();
    }

    /**
     * {@inheritDoc}
     *
     * <p>
     * Intercepts $-prefixed symbols: $0 returns all args as list,
     * $1 returns first arg, $2 returns second, etc.
     * </p>
     */
    @Override
    protected Object lookup(Symbol sym, List<Environment> trace)
    {
        if (sym.getName().startsWith("$"))
        {
            int indx = Integer.parseInt(sym.getName().substring(1));
            if (indx == 0)
                return Cons.list(args);
            else if (indx - 1 > args.length)
                return null;
            else
                return args[indx - 1];
        }
        else
        {
            return super.lookup(sym, trace);
        }
    }

    /** {@inheritDoc} Delegates to parent environment. */
    @Override
    public void removeBinding(Symbol sym)
    {
        getParent().removeBinding(sym);
    }

    /** {@inheritDoc} Delegates to parent environment. */
    @Override
    public void setBinding(Symbol sym, Object obj)
    {
        getParent().setBinding(sym, obj);
    }
}
