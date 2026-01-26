package ru.ydn.jlll.common;

import java.io.Serializable;

/**
 * Singleton representing end-of-file.
 * Returned by input operations (read, read-line, read-char, peek-char) when the end of the input
 * stream is reached.
 *
 * <p>
 * Test for EOF using {@code eof-object?} predicate:
 * </p>
 *
 * <pre>
 * (define line (read-line))
 * (if (eof-object? line)
 *     (println "End of input")
 *     (println "Read: " line))
 * </pre>
 */
public class Eof implements Serializable
{
    private static final long serialVersionUID = 1L;
    /** The singleton EOF instance. */
    public static final Eof EOF = new Eof();

    private Eof()
    {
    }

    /**
     * Returns the string representation of EOF.
     *
     * @return "#&lt;eof&gt;"
     */
    @Override
    public String toString()
    {
        return "#<eof>";
    }

    /**
     * Ensures deserialization returns the singleton instance.
     *
     * @return the singleton EOF instance
     */
    private Object readResolve()
    {
        return EOF;
    }
}
