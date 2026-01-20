package ru.ydn.jlll.common;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: May 31, 2004
 * Time: 7:35:58 PM
 * To change this template use Options | File Templates.
 */
public class Null extends Cons //implements Serializable
{
    private static final long serialVersionUID = 6499553304262659518L;
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
