package ru.ydn.jlll.util;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;

/**
 * Utility methods for Cons list construction and manipulation.
 * Provides conversions between Cons lists and Java collections/arrays.
 */
public class ListUtil
{
    /** Generic comparator for sorting lists with Comparable elements */
    public static final Comparator<Object> GENERIC_COMPARATOR = new GenericComparator();

    /**
     * Comparator that delegates to Comparable.compareTo().
     */
    public static class GenericComparator implements Comparator<Object>
    {
        public int compare(Object o1, Object o2)
        {
            if (!(o1 instanceof Comparable))
                throw new IllegalArgumentException("Object '" + o1 + "' is not comparable");
            return ((Comparable) o1).compareTo(o2);
        }
    }

    /**
     * Appends an element to the end of a Cons list (mutating).
     *
     * @param first
     *            the list to append to
     * @param last
     *            the element to append
     * @throws JlllException
     *             if the list is a dotted pair
     */
    public static void append(Cons first, Object last) throws JlllException
    {
        if (first.car() == null)
            first.car(last);
        else if (first.cdr() == null || ((first.cdr() instanceof Cons) && ((Cons) first.cdr()).isNull()))
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

    /**
     * Converts a Cons list to an Object array.
     *
     * @param cons
     *            the list to convert
     * @return array of list elements
     * @throws JlllException
     *             if iteration fails
     */
    public static Object[] listVector(Cons cons) throws JlllException
    {
        return consToList(cons).toArray();
    }

    /**
     * Converts a Cons list to a Java List.
     *
     * @param cons
     *            the Cons list to convert
     * @return a new ArrayList containing the elements
     * @throws JlllException
     *             if iteration fails
     */
    public static List<Object> consToList(Cons cons) throws JlllException
    {
        List<Object> ret = new ArrayList<Object>();
        Iterator<?> it = cons.iterator();
        while (it.hasNext())
        {
            ret.add(it.next());
        }
        return ret;
    }

    /**
     * Creates a Cons list from varargs.
     *
     * @param objects
     *            the elements
     * @return a new Cons list
     */
    public static Cons toCons(Object... objects)
    {
        return arrayToCons(objects);
    }

    /**
     * Converts an array to a Cons list.
     *
     * @param array
     *            the array to convert
     * @return a new Cons list containing the array elements
     */
    public static Cons arrayToCons(Object[] array)
    {
        return arrayToCons(array, null);
    }

    public static Cons arrayToCons(Object[] array, Object dotted)
    {
        if (array == null || array.length == 0)
        {
            if (Null.NULL.equals(dotted))
                return Null.NULL;
            else
                return new Cons(null, dotted);
        }
        Cons ret = null;
        for (int i = array.length - 1; i >= 0; i--)
        {
            Object prev = array[i];
            if (ret == null)
                ret = new Cons(prev, dotted);
            else
                ret = new Cons(prev, ret);
        }
        return ret;
    }

    public static Cons getLastCons(Cons cons)
    {
        while ((cons.cdr() instanceof Cons) && !((Cons) cons.cdr()).isNull())
        {
            cons = (Cons) cons.cdr();
        }
        return cons;
    }

    public static Cons list(Object obj1, Object obj2)
    {
        return new Cons(obj1, new Cons(obj2));
    }
}
