package ru.ydn.jlll.common;

/**
 * Singleton representing the empty list (nil) in JLLL.
 * Used as the list terminator and as a false-like value in conditionals.
 *
 * <p>
 * Access via {@link #NULL} - the constructor is private.
 * Attempting to modify this object throws {@link IllegalAccessError}.
 * </p>
 */
public class Null extends Cons
{
    private static final long serialVersionUID = 6499553304262659518L;
    /** The singleton null/nil instance */
    public static final Null NULL = new Null();

    private Null()
    {
    }

    public String toString()
    {
        return "()";
    }

    @Override
    public boolean isNull()
    {
        return true;
    }

    @Override
    public void car(Object car)
    {
        throw new IllegalAccessError("It's prohibited to modify static Nil object");
    }

    @Override
    public void cdr(Object cdr)
    {
        throw new IllegalAccessError("It's prohibited to modify static Nil object");
    }

    public boolean equals(Object obj)
    {
        if (obj == null)
            return true;
        if (obj instanceof Null)
            return true;
        if ((obj instanceof Cons) && ((Cons) obj).isNull())
            return true;
        return false;
    }
}
