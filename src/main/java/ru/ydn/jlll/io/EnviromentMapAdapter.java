package ru.ydn.jlll.io;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Symbol;

public class EnviromentMapAdapter implements Map<String, Object>
{
    private final Enviroment env;

    public EnviromentMapAdapter(Enviroment env)
    {
        this.env = env;
    }

    public int size()
    {
        return env.getAllBindings().size();
    }

    public boolean isEmpty()
    {
        return env.isEmpty();
    }

    public boolean containsKey(Object key)
    {
        if (key instanceof String)
        {
            return env.lookup((String) key) != null;
        }
        else if (key instanceof Symbol)
        {
            return env.lookup((Symbol) key) != null;
        }
        else
        {
            return false;
        }
    }

    public boolean containsValue(Object value)
    {
        return env.getAllBindings().containsValue(value);
    }

    public Object get(Object key)
    {
        if (key instanceof String)
        {
            return env.lookup((String) key);
        }
        else if (key instanceof Symbol)
        {
            return env.lookup((Symbol) key);
        }
        else
        {
            return null;
        }
    }

    public Object put(String key, Object value)
    {
        Object prev = env.lookup(key);
        env.addBinding(key, value);
        return prev;
    }

    public Object remove(Object key)
    {
        Object prev = null;
        if (key instanceof String)
        {
            prev = env.lookup((String) key);
            env.removeBinding((String) key);
        }
        else if (key instanceof Symbol)
        {
            prev = env.lookup((Symbol) key);
            env.removeBinding((Symbol) key);
        }
        return prev;
    }

    public void putAll(Map<? extends String, ? extends Object> m)
    {
        for (Entry<? extends String, ? extends Object> entry : m.entrySet())
        {
            env.addBinding(entry.getKey(), entry.getValue());
        }
    }

    public void clear()
    {
        env.clear();
    }

    public Set<String> keySet()
    {
        Set<Symbol> symbolsSet = env.getAllBindings().keySet();
        HashSet<String> ret = new HashSet<String>();
        for (Symbol symbol : symbolsSet)
        {
            ret.add(symbol.toString());
        }
        return ret;
    }

    public Collection<Object> values()
    {
        return env.getAllBindings().values();
    }

    public Set<java.util.Map.Entry<String, Object>> entrySet()
    {
        Map<Symbol, Object> envContent = env.getAllBindings();
        Set<java.util.Map.Entry<String, Object>> ret = new HashSet<Map.Entry<String, Object>>();
        for (Entry<Symbol, Object> entry : envContent.entrySet())
        {
            final String key = entry.getKey().toString();
            final Object value = entry.getValue();
            ret.add(new Entry<String, Object>()
            {
                public Object setValue(Object value)
                {
                    throw new UnsupportedOperationException("Can't change value to Entry");
                }

                public Object getValue()
                {
                    return value;
                }

                public String getKey()
                {
                    return key;
                }
            });
        }
        return ret;
    }
}
