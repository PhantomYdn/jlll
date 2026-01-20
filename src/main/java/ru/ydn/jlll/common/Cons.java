package ru.ydn.jlll.common;

import java.io.Serializable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import ru.ydn.jlll.util.CommonUtil;
import ru.ydn.jlll.util.ListUtil;

/**
 * Implementation of pair of objects: CAR and CDR
 * Cans is the main data structure which is being used in LISP like languages
 *
 */
public class Cons implements Serializable, Cloneable, Iterable<Object>
{
    private static final long serialVersionUID = 3084412198960125583L;
    private Object car = null;
    private Object cdr = null;

    /**
     * Iterator over Cons chain
     *
     */
    public class ConsIterator implements Iterator<Object>
    {
        private Cons next = null;
        private Object dotted = null;

        /**
         * Constructor of ConsIterator
         *
         * @param chain
         *            cons chain which will be used in iterating
         */
        public ConsIterator(Cons chain)
        {
            this.next = chain;
        }

        /**
         * Returns <tt>true</tt> if the iteration has more elements. (In other
         * words, returns <tt>true</tt> if <tt>next</tt> would return an element
         * rather than throwing an exception.)
         *
         * @return <tt>true</tt> if the iterator has more elements.
         */
        public boolean hasNext()
        {
            if (next != null && !next.isNull())
                return true;
            else
                return false;
        }

        /**
         * Returns the next element in the iterated chain of cons. Calling this method
         * repeatedly until the
         * {
         *
         * @link #hasNext()
         *       }
         *       method returns false will
         *       return each element in the underlying collection exactly once.
         *
         * @return the next element in the iteration.
         * @exception NoSuchElementException
         *                iteration has no more elements.
         */
        public Object next()
        {
            if (!hasNext())
                throw new NoSuchElementException("No next element in the list");
            Object ret = next.car();
            if (next.cdr() instanceof Cons)
            {
                next = (Cons) next.cdr();
            }
            else
            {
                dotted = next.cdr();
                next = null;
            }
            return ret;
        }

        /**
         * Returns dotted (cdr of last Cons) object
         *
         * @return dotted (cdr of last Cons) object
         */
        public Object getDotted()
        {
            return dotted;
        }

        /**
         * Returns the tail of the chain
         *
         * @return the tail of the chain
         */
        public Cons getTail()
        {
            Cons ret = next;
            next = null;
            return ret;
            //return ret==null?Null.NULL:ret;
        }

        /**
         * Allways throws UnsupportedOperationException
         */
        public void remove()
        {
            throw new UnsupportedOperationException("Remove for ConsIterator is not supported");
        }
    }

    /**
     * Constructor of Cons - pair of objects CAR and CDR
     *
     * @param car
     *            first element of this Cons
     * @param cdr
     *            second element of this Cons
     */
    public Cons(Object car, Object cdr)
    {
        this.car = car;
        //this.cdr = ((cdr instanceof Cons) && (((Cons)cdr).isNil()))? null : cdr;
        this.cdr = CommonUtil.avoidNull(cdr);
    }

    /**
     * Constructor of the Cons - cretes Cons only with first element. Second is null
     *
     * @param car
     *            first element of this Cons
     */
    public Cons(Object car)
    {
        this.car = car;
    }

    /**
     * Creates nil - empty Cons
     */
    public Cons()
    {
    }

    /**
     * Returns first element of the Cons
     *
     * @return first element of the Cons
     */
    public Object car()
    {
        return car;
    }

    /**
     * Sets first element of the Cons. This is a destructive function
     *
     * @param car
     *            Object that will be set as first element of the Cons
     */
    public void car(Object car)
    {
        this.car = car;
    }

    /**
     * Returns the second element of the cons chain
     *
     * @return second element of the cons chain
     */
    public Object cadr()
    {
        return ((Cons) cdr()).car();
    }

    /**
     * Returns second element of the Cons
     *
     * @return second element of the Cons
     */
    public Object cdr()
    {
        //return cdr;
        return cdr == null ? Null.NULL : cdr;
        //return cdr==null? new Cons():cdr;
    }

    /**
     * Sets second element of the Cons. This is a destructive function
     *
     * @param car
     *            Object that will be set as second element of the Cons
     */
    public void cdr(Object cdr)
    {
        this.cdr = cdr;
    }

    private String objectToString(Object obj)
    {
        if (obj instanceof String)
        {
            String ret = (String) obj;
            ret = ret.replace("\"", "\\\"").replace("\n", "\\n");
            ret.replaceAll("", "");
            return "\"" + ret + "\"";
        }
        else
        {
            return Null.NULL.equals(obj) ? "()" : obj.toString();
        }
    }

    /**
     * Returns the string representation of the cons
     */
    public String toString()
    {
        if (isNull())
            return "()";
        else if (cdr instanceof Cons && !((Cons) cdr).isNull())
        {
            Cons cdrCons = (Cons) cdr;
            if (Symbol.QUOTE.equals(car) && cdrCons.length() == 1)
            {
                return "\'" + objectToString(cdrCons.car);
            }
            else if (Symbol.QUASIQUOTE.equals(car) && cdrCons.length() == 1)
            {
                return "`" + objectToString(cdrCons.car);
            }
            else if (Symbol.UNQUOTE.equals(car) && cdrCons.length() == 1)
            {
                return "," + objectToString(cdrCons.car);
            }
            else if (Symbol.UNQUOTE_SPLICING.equals(car) && cdrCons.length() == 1)
            {
                return ",@" + objectToString(cdrCons.car);
            }
            else if (Symbol.EXLAMATION.equals(car) && cdrCons.length() == 1)
            {
                return "!" + objectToString(cdrCons.car);
            }
            else if (Symbol.SHARP.equals(car) && cdrCons.length() == 1)
            {
                return "#" + objectToString(cdrCons.car);
            }
            return "(" + objectToString(car) + " " + cdrCons.toStringList() + ")";
        }
        else
        {
            if (cdr == null || ((cdr instanceof Cons) && ((Cons) cdr).isNull()))
            {
                return "(" + objectToString(car) + ")";
            }
            else
            {
                return "(" + objectToString(car) + " . " + objectToString(cdr) + ")";
            }
        }
    }

    private String toStringList()
    {
        if (isNull())
            return "()";
        else if (cdr instanceof Cons && !((Cons) cdr).isNull())
        {
            return objectToString(car) + " " + ((Cons) cdr).toStringList();
        }
        else
        {
            if (cdr == null || ((cdr instanceof Cons) && ((Cons) cdr).isNull()))
            {
                return objectToString(car) + "";
            }
            else
            {
                return objectToString(car) + "." + objectToString(cdr);
            }
        }
    }

    /**
     * Returns the iterator over the cons chain
     */
    public ConsIterator iterator()
    {
        return new ConsIterator(this);
    }

    /**
     * Returns the n-th element of the chain
     *
     * @param index
     *            index of the required element
     * @return the n-th element of the chain
     */
    public Object get(int index)
    {
        Iterator<?> it = iterator();
        for (int i = 0; i < index && it.hasNext(); i++)
        {
            it.next();
        }
        return it.next();
    }

    /**
     * Returns the tail after n-th element
     *
     * @param index
     *            index of the first element for the tail
     * @return the tail after n-th element
     */
    public Cons tail(int index)
    {
        ConsIterator it = iterator();
        for (int i = 0; i < index && it.hasNext(); i++)
        {
            it.next();
        }
        return it.getTail();
    }

    /**
     * Returns the length of the cons chain
     *
     * @return the length of the cons chain
     */
    public int length()
    {
        Iterator<?> it = this.iterator();
        int ret = 0;
        while (it.hasNext())
        {
            it.next();
            ret++;
        }
        return ret;
    }

    /**
     * Check wether the chain of cons contains required object
     *
     * @param obj
     *            Object for checking
     * @return <tt>true</tt> if chain contains and <tt>false</tt> if not
     */
    public boolean contain(Object obj)
    {
        if (obj == null)
            return true;
        ConsIterator it = (ConsIterator) iterator();
        while (it.hasNext())
        {
            Object next = it.next();
            if (obj.equals(next))
                return true;
        }
        if (obj.equals(it.getDotted()))
            return true;
        return false;
    }

    /**
     * Whether this cons is nil?
     *
     * @return <tt>true</tt> if chain is nil and <tt>false</tt> if not
     */
    public boolean isNull()
    {
        return car == null && cdr == null;
    }

    /**
     * Check whete this Cons equals to specified object or not
     */
    public boolean equals(Object obj)
    {
        if (isNull())
            return Null.NULL.equals(obj);
        if (!(obj instanceof Cons))
            return false;
        Cons cons = (Cons) obj;
        return equals(car, cons.car) && equals(cdr, cons.cdr);
    }

    private boolean equals(Object obj1, Object obj2)
    {
        if (obj1 == obj2)
            return true;
        if (obj1 == null || obj2 == null)
            return false;
        return obj1.equals(obj2);
    }

    /**
     * Clone this cons with CARs and CDRs
     */
    public Cons clone()
    {
        Object newCar = car instanceof Cons ? ((Cons) car).clone() : car;
        Object newCdr = cdr instanceof Cons ? ((Cons) cdr).clone() : cdr;
        return new Cons(newCar, newCdr);
    }

    /**
     * Create a list (chain of cons) from the specified objects
     *
     * @param objects
     *            array of objects
     * @return list (chain of cons) from the specified objects
     */
    public static Cons list(Object... objects)
    {
        return ListUtil.arrayToCons(objects);
    }
}
