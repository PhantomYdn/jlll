package ru.ydn.jlll.common;

import java.io.Serializable;
import java.util.Map;
import java.util.WeakHashMap;
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
    private transient static Map<String, Symbol> intern = new WeakHashMap<String, Symbol>();
    /** Dot symbol used for dotted pairs */
    /** Dot symbol used for dotted pairs */
    public static Symbol DOT = Symbol.intern(".");
    /** Boolean true symbol */
    public static Symbol TRUE = Symbol.intern("true");
    /** Scheme-style boolean true (#t) */
    public static Symbol TRUE2 = Symbol.intern("#t");
    /** Boolean false symbol */
    public static Symbol FALSE = Symbol.intern("false");
    /** Scheme-style boolean false (#f) */
    public static Symbol FALSE2 = Symbol.intern("#f");
    /** Null/nil symbol */
    public static Symbol NULL = Symbol.intern("null");
    /** Quote special form */
    public static Symbol QUOTE = Symbol.intern("quote");
    /** Begin special form for sequencing */
    public static Symbol BEGIN = Symbol.intern("begin");
    /** Quasiquote for template expressions */
    public static Symbol QUASIQUOTE = Symbol.intern("quasiquote");
    /** Unquote-splicing for list interpolation */
    public static Symbol UNQUOTE_SPLICING = Symbol.intern("unquote_splicing");
    /** Unquote for value interpolation */
    public static Symbol UNQUOTE = Symbol.intern("unquote");
    /** Exclamation mark reader macro */
    public static Symbol EXLAMATION = Symbol.intern("exlamation");
    /** Sharp/hash reader macro */
    public static Symbol SHARP = Symbol.intern("sharp");
    /** Standard input stream symbol */
    public static Symbol STDIN = Symbol.intern("stdin");
    /** Standard output stream symbol */
    public static Symbol STDOUT = Symbol.intern("stdout");
    private String name = null;

    private Symbol(String name)
    {
        this.name = name;
    }

    /**
     * Returns the interned symbol for the given name.
     * Creates a new symbol if one doesn't exist, otherwise returns the existing one.
     *
     * @param name
     *            the symbol name
     * @return the unique Symbol instance for this name
     */
    public static Symbol intern(String name)
    {
        Symbol ret = intern.get(name);
        if (ret == null)
        {
            ret = new Symbol(name);
            intern.put(name, ret);
        }
        return ret;
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
    public String toString()
    {
        return JlllSymbolNaming.convertFromSymbolNameToOut(name);
    }

    public boolean equals(Object obj)
    {
        if (obj == null || !(obj instanceof Symbol))
            return false;
        return getName().equals(((Symbol) obj).getName());
        //return this == obj;
    }

    @Override
    public int hashCode()
    {
        return getName().hashCode();
    }
}
