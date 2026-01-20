package ru.ydn.jlll.common;

import java.util.List;
import java.util.Map;

public class DelegateEnvironment extends Enviroment
{
    /**
     *
     */
    private static final long serialVersionUID = -8776861114285131541L;

    public DelegateEnvironment(Enviroment parent)
    {
        super(parent);
    }

    @Override
    public void addBinding(Symbol sym, Object obj)
    {
        parent.addBinding(sym, obj);
    }

    @Override
    public Map<Symbol, Object> getAllBindings()
    {
        return parent.getAllBindings();
    }

    @Override
    protected Object lookup(Symbol sym, List<Enviroment> trace)
    {
        return parent.lookup(sym, trace);
    }

    @Override
    public void removeBinding(Symbol sym)
    {
        parent.removeBinding(sym);
    }

    @Override
    public void setBinding(Symbol sym, Object obj)
    {
        parent.setBinding(sym, obj);
    }
}
