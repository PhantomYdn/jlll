package ru.ydn.jlll.common;

import java.util.List;
import java.util.Map;

/**
 * An environment that delegates all binding operations to its parent.
 *
 * <p>
 * Unlike a normal child environment that creates its own local scope,
 * DelegateEnvironment passes all operations (add, set, remove, lookup)
 * directly to the parent environment. This is useful for creating
 * transparent wrappers that don't introduce a new scope level.
 * </p>
 */
public class DelegateEnvironment extends Environment
{
    private static final long serialVersionUID = -8776861114285131541L;

    /**
     * Creates a delegate environment that forwards all operations to the parent.
     *
     * @param parent
     *            the environment to delegate to
     */
    public DelegateEnvironment(Environment parent)
    {
        super(parent);
    }

    /**
     * {@inheritDoc}
     *
     * <p>
     * DelegateEnvironment is transient - it's a transparent wrapper that should be skipped.
     * </p>
     */
    @Override
    public boolean isTransient()
    {
        return true;
    }

    /** {@inheritDoc} Delegates to parent environment. */
    @Override
    public void addBinding(Symbol sym, Object obj)
    {
        parent.addBinding(sym, obj);
    }

    /** {@inheritDoc} Returns parent's bindings. */
    @Override
    public Map<Symbol, Object> getAllBindings()
    {
        return parent.getAllBindings();
    }

    /** {@inheritDoc} Looks up in parent environment. */
    @Override
    protected Object lookup(Symbol sym, List<Environment> trace)
    {
        return parent.lookup(sym, trace);
    }

    /** {@inheritDoc} Removes from parent environment. */
    @Override
    public void removeBinding(Symbol sym)
    {
        parent.removeBinding(sym);
    }

    /** {@inheritDoc} Sets in parent environment. */
    @Override
    public void setBinding(Symbol sym, Object obj)
    {
        parent.setBinding(sym, obj);
    }
}
