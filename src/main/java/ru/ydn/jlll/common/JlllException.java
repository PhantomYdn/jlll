package ru.ydn.jlll.common;

import java.util.ArrayList;
import java.util.List;

/**
 * Exception thrown during JLLL parsing or evaluation.
 * Wraps interpreter errors with optional JLLL stack trace information showing
 * the Lisp expressions that led to the error.
 */
public class JlllException extends Exception
{
    private static final long serialVersionUID = 8810568891407802852L;
    private List<String> jlllCouse = null;

    /**
     * Creates an exception with the given message.
     *
     * @param s
     *            the error message
     */
    public JlllException(String s)
    {
        super(s);
    }

    /**
     * Creates an exception wrapping another throwable.
     *
     * @param couse
     *            the underlying cause
     */
    public JlllException(Throwable couse)
    {
        this(couse.getMessage(), couse);
    }

    /**
     * Creates an exception with message and cause.
     *
     * @param s
     *            the error message
     * @param couse
     *            the underlying cause
     */
    public JlllException(String s, Throwable couse)
    {
        super(s, couse);
    }

    /**
     * Creates an exception with message and JLLL context.
     *
     * @param s
     *            the error message
     * @param jlllCouse
     *            the JLLL expression that caused the error
     */
    public JlllException(String s, Object jlllCouse)
    {
        super(s);
        addJlllCouse(jlllCouse);
    }

    /**
     * Creates an exception with message, JLLL context, and Java cause.
     *
     * @param s
     *            the error message
     * @param jlllCouse
     *            the JLLL expression that caused the error
     * @param couse
     *            the underlying Java exception
     */
    public JlllException(String s, Object jlllCouse, Throwable couse)
    {
        super(s, couse);
        addJlllCouse(jlllCouse);
    }

    /**
     * Adds a JLLL expression to the error context stack.
     * Called during stack unwinding to build up the JLLL trace.
     *
     * @param couse
     *            the JLLL expression to add to the trace
     */
    public void addJlllCouse(Object couse)
    {
        if (couse != null)
        {
            if (jlllCouse == null)
                jlllCouse = new ArrayList<String>();
            jlllCouse.add(couse.toString());
        }
    }

    /**
     * Returns the JLLL stack trace as a string.
     *
     * @return the formatted JLLL trace
     */
    public String jlllCause()
    {
        StringBuffer sb = new StringBuffer(super.toString());
        if (jlllCouse != null && jlllCouse.size() > 0)
        {
            sb.append("\njlll:\n");
            for (String nextCouse : jlllCouse)
            {
                sb.append("\tat ").append(nextCouse).append("\n");
            }
        }
        return sb.toString();
    }

    public String toString()
    {
        StringBuffer sb = new StringBuffer(super.toString());
        if (jlllCouse != null && jlllCouse.size() > 0)
        {
            sb.append("\njlll:\n");
            for (String nextCouse : jlllCouse)
            {
                sb.append("\tat ").append(nextCouse).append("\n");
            }
            sb.append("java:");
        }
        return sb.toString();
    }
}
