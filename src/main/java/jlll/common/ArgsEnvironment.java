package jlll.common;

import java.util.List;
import java.util.Map;

public class ArgsEnvironment extends Enviroment
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 7866903478037016312L;
	private Object[] args;
    public ArgsEnvironment(Enviroment parent, Object[] args)
    {
        super(parent);
        this.args = args;
    }
    @Override
    public void addBinding(Symbol sym, Object obj)
    {
        getParent().addBinding(sym, obj);
    }
    @Override
    public Map<Symbol, Object> getAllBindings()
    {
        return getParent().getAllBindings();
    }
    @Override
    protected Object lookup(Symbol sym, List<Enviroment> trace)
    {
        if(sym.getName().startsWith("$"))
        {
            int indx = Integer.parseInt(sym.getName().substring(1));
            if(indx==0) return Cons.list(args);
            else if(indx-1>args.length) return null;
            else return args[indx-1];
        }
        else
        {
            return super.lookup(sym, trace);
        }
    }
    @Override
    public void removeBinding(Symbol sym)
    {
        getParent().removeBinding(sym);
    }
    @Override
    public void setBinding(Symbol sym, Object obj)
    {
        getParent().setBinding(sym, obj);
    }
    
}
