package ru.ydn.jlll.util;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;

/**
 * Created by IntelliJ IDEA.
 * User: Eleas
 * Date: 06.05.2003
 * Time: 22:46:14
 * To change this template use Options | File Templates.
 */
public class ListUtil
{
	public static final Comparator<Object> GENERIC_COMPARATOR = new GenericComparator();
	
	public static class GenericComparator implements Comparator<Object>
	{
		public int compare(Object o1, Object o2)
		{
			if(!(o1 instanceof Comparable)) throw new IllegalArgumentException("Object '"+o1+"' is not comparable");
			return ((Comparable)o1).compareTo(o2);
		}
		
	}

    public static void append(Cons first, Object last) throws JlllException
    {
//        if(obj==null) return;
       if (first.car() == null)
            first.car(last);
        else if (first.cdr() == null || ((first.cdr() instanceof Cons) && ((Cons)first.cdr()).isNull())
        )
        {
            first.cdr(new Cons(last, null));
        }
        else
        {
            if (first.cdr() instanceof Cons)
            {
                append((Cons) first.cdr(), last);
            }
            else
            {
                throw new JlllException("This is dotted cons. Append:" + last);
            }
        }
    }
    
    public static Object[] listVector(Cons cons) throws JlllException
    {
    	return consToList(cons).toArray();
    }
    
    public static List<Object> consToList(Cons cons) throws JlllException
    {
        List<Object> ret = new ArrayList<Object>();
        Iterator<?> it = cons.iterator();
        while(it.hasNext())
        {
            ret.add(it.next());
        }
        return ret;
    }
    
    public static Cons toCons(Object...objects)
    {
        return arrayToCons(objects);
    }
    
    public static Cons arrayToCons(Object[] array)
    {
    	return arrayToCons(array, null);
    }

    public static Cons arrayToCons(Object[] array, Object dotted)
    {
    	if(array==null || array.length==0)
    	{
    		if(Null.NULL.equals(dotted)) return Null.NULL;
    		else return new Cons(null, dotted);
    	}
        Cons ret = null;
        for(int i=array.length-1; i>=0; i--)
        {
            Object prev = array[i];
            if(ret==null) ret = new Cons(prev,dotted);
            else ret = new Cons(prev, ret);
        }
        return ret;
    }


    public static Cons getLastCons(Cons cons)
    {
        while ((cons.cdr() instanceof Cons) && !((Cons)cons.cdr()).isNull())
        {
            cons = (Cons) cons.cdr();
        }
        return cons;
    }

    public static Cons list(Object obj1, Object obj2)
    {
        return new Cons(obj1,new Cons(obj2));
    }
}


