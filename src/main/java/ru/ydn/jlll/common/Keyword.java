package ru.ydn.jlll.common;

import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * Interned keyword representing a self-evaluating identifier.
 * Keywords are distinct from symbols: they start with a colon and evaluate to themselves.
 *
 * <p>
 * Keywords are unique: two keywords with the same name are guaranteed to be the same object,
 * enabling fast identity comparison with {@code ==}.
 * </p>
 *
 * <p>
 * Use {@link #intern(String)} to obtain keywords - the constructor is private.
 * </p>
 *
 * <p>
 * Example usage in JLLL:
 * </p>
 *
 * <pre>
 * :foo              ; keyword, evaluates to itself
 * (eq? :foo :foo)   ; => true
 * (eq? :foo 'foo)   ; => false (keyword != symbol)
 * </pre>
 */
public class Keyword implements Serializable
{
    private static final long serialVersionUID = 7823456789012345678L;
    private static final ConcurrentMap<String, Keyword> intern = new ConcurrentHashMap<>();
    private final String name;

    private Keyword(String name)
    {
        this.name = name;
    }

    /**
     * Returns the interned keyword for the given name.
     * Creates a new keyword if one doesn't exist, otherwise returns the existing one.
     * This method is thread-safe.
     *
     * @param name
     *            the keyword name (without the leading colon)
     * @return the unique Keyword instance for this name
     * @throws JlllException
     *             if the name is invalid (empty or starts with colon)
     */
    public static Keyword intern(String name) throws JlllException
    {
        if (name == null || name.isEmpty())
        {
            throw new JlllException("Keyword name cannot be empty");
        }
        if (name.startsWith(":"))
        {
            throw new JlllException("Keyword name cannot start with colon: " + name);
        }
        return intern.computeIfAbsent(name, Keyword::new);
    }

    /**
     * Returns the keyword's name (without the leading colon).
     *
     * @return the keyword name
     */
    public String getName()
    {
        return name;
    }

    /**
     * Converts this keyword to its corresponding symbol.
     *
     * @return the Symbol with the same name as this keyword
     */
    public Symbol toSymbol()
    {
        return Symbol.intern(name);
    }

    /**
     * Creates a keyword from a symbol.
     *
     * @param symbol
     *            the symbol to convert
     * @return the Keyword with the same name as the symbol
     * @throws JlllException
     *             if conversion fails
     */
    public static Keyword fromSymbol(Symbol symbol) throws JlllException
    {
        return intern(symbol.getName());
    }

    /**
     * Returns the keyword's display representation with leading colon.
     *
     * @return the keyword formatted as ":name"
     */
    @Override
    public String toString()
    {
        return ":" + name;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null || !(obj instanceof Keyword))
        {
            return false;
        }
        return getName().equals(((Keyword) obj).getName());
    }

    @Override
    public int hashCode()
    {
        return getName().hashCode();
    }
}
