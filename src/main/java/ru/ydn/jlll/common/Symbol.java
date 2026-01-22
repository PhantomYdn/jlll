package ru.ydn.jlll.common;

import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import ru.ydn.jlll.io.JlllSymbolNaming;

/**
 * Interned symbol representing a Lisp identifier.
 * Symbols are unique: two symbols with the same name are guaranteed to be the same object,
 * enabling fast identity comparison with {@code ==}.
 *
 * <p>
 * Use {@link #intern(String)} to obtain symbols - the constructor is private.
 * </p>
 */
public class Symbol implements Serializable
{
    private static final long serialVersionUID = 3176952970569428659L;
    private static final ConcurrentMap<String, Symbol> intern = new ConcurrentHashMap<>();
    /** Dot symbol used for dotted pairs */
    public static final Symbol DOT = Symbol.intern(".");
    /** Boolean true symbol */
    public static final Symbol TRUE = Symbol.intern("true");
    /** Scheme-style boolean true (#t) */
    public static final Symbol TRUE2 = Symbol.intern("#t");
    /** Boolean false symbol */
    public static final Symbol FALSE = Symbol.intern("false");
    /** Scheme-style boolean false (#f) */
    public static final Symbol FALSE2 = Symbol.intern("#f");
    /** Null/nil symbol */
    public static final Symbol NULL = Symbol.intern("null");
    /** Quote special form */
    public static final Symbol QUOTE = Symbol.intern("quote");
    /** Begin special form for sequencing */
    public static final Symbol BEGIN = Symbol.intern("begin");
    /** Quasiquote for template expressions */
    public static final Symbol QUASIQUOTE = Symbol.intern("quasiquote");
    /** Unquote-splicing for list interpolation */
    public static final Symbol UNQUOTE_SPLICING = Symbol.intern("unquote_splicing");
    /** Unquote for value interpolation */
    public static final Symbol UNQUOTE = Symbol.intern("unquote");
    /** Exclamation mark reader macro */
    public static final Symbol EXCLAMATION = Symbol.intern("exclamation");
    /** Sharp/hash reader macro */
    public static final Symbol SHARP = Symbol.intern("sharp");
    /** Standard input stream symbol */
    public static final Symbol STDIN = Symbol.intern("stdin");
    /** Standard output stream symbol */
    public static final Symbol STDOUT = Symbol.intern("stdout");
    private final String name;

    private Symbol(String name)
    {
        this.name = name;
    }

    /**
     * Returns the interned symbol for the given name.
     * Creates a new symbol if one doesn't exist, otherwise returns the existing one.
     * This method is thread-safe.
     *
     * @param name
     *            the symbol name
     * @return the unique Symbol instance for this name
     */
    public static Symbol intern(String name)
    {
        return intern.computeIfAbsent(name, Symbol::new);
    }

    /**
     * Returns the symbol's name.
     *
     * @return the symbol name
     */
    public String getName()
    {
        return name;
    }

    /**
     * Returns the symbol's display representation.
     *
     * @return the symbol name formatted for output
     */
    @Override
    public String toString()
    {
        return JlllSymbolNaming.convertFromSymbolNameToOut(name);
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null || !(obj instanceof Symbol))
            return false;
        return getName().equals(((Symbol) obj).getName());
    }

    @Override
    public int hashCode()
    {
        return getName().hashCode();
    }
}
