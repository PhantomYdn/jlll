package ru.ydn.jlll.common;

import java.util.HashMap;
import java.util.Map;
import com.github.therapi.runtimejavadoc.ClassJavadoc;
import com.github.therapi.runtimejavadoc.CommentFormatter;
import com.github.therapi.runtimejavadoc.RuntimeJavadoc;

/**
 * Base class for built-in JLLL functions (primitives).
 * Primitives are automatically bound to the environment upon construction.
 *
 * <p>
 * Subclasses must implement {@link Procedure#apply(Cons, Environment)} or
 * {@link Procedure#applyEvaluated(Cons, Environment)} to define the primitive's behavior.
 * </p>
 *
 * <p>
 * Documentation can be provided in several ways:
 * </p>
 * <ul>
 * <li>For named subclasses: JavaDoc comments are automatically captured at compile time</li>
 * <li>For inline/anonymous primitives: Use the constructor with doc parameter</li>
 * <li>For full control: Use the constructor with metadata map</li>
 * </ul>
 */
public class Primitive extends Procedure
{
    private static final long serialVersionUID = -9190619708410061311L;
    /** Shared formatter for converting JavaDoc comments to plain text */
    private static final CommentFormatter FORMATTER = new CommentFormatter();
    /** Symbol for documentation metadata key */
    private static final Symbol DOC_KEY = Symbol.intern("doc");
    /** Symbol for Java class metadata key */
    private static final Symbol JAVA_CLASS_KEY = Symbol.intern("java-class");
    /** The original name used when binding this primitive */
    protected final String originalName;

    /**
     * Creates a primitive and binds it to the given environment.
     * For named subclasses, JavaDoc comments are automatically captured as documentation.
     * For anonymous classes, no documentation is set (use the doc parameter constructor instead).
     *
     * @param name
     *            the primitive name (used for binding and error messages)
     * @param env
     *            the environment to bind this primitive in
     */
    public Primitive(String name, Environment env)
    {
        this(name, env, (String) null);
    }

    /**
     * Creates a primitive with explicit documentation and binds it to the given environment.
     * Use this constructor for anonymous/inline primitives where JavaDoc cannot be captured.
     *
     * @param name
     *            the primitive name (used for binding and error messages)
     * @param env
     *            the environment to bind this primitive in
     * @param doc
     *            the documentation string, or null to attempt JavaDoc extraction
     */
    public Primitive(String name, Environment env, String doc)
    {
        Symbol sym = Symbol.intern(name);
        env.addBinding(sym, this);
        originalName = name;
        // Set java-class metadata
        env.setMeta(sym, JAVA_CLASS_KEY, this.getClass().getName());
        // Set documentation: explicit doc takes precedence, then try therapi for named classes
        String docToSet = doc;
        if (docToSet == null)
        {
            docToSet = extractJavadoc();
        }
        if (docToSet != null && !docToSet.isEmpty())
        {
            env.setMeta(sym, DOC_KEY, docToSet);
        }
    }

    /**
     * Creates a primitive with full metadata control and binds it to the given environment.
     * The metadata map can contain any key-value pairs; common keys include:
     * <ul>
     * <li>{@code doc} - documentation string</li>
     * <li>{@code category} - functional category</li>
     * <li>{@code since} - version when introduced</li>
     * </ul>
     *
     * @param name
     *            the primitive name (used for binding and error messages)
     * @param env
     *            the environment to bind this primitive in
     * @param metadata
     *            map of metadata keys (Symbols) to values, may be null
     */
    public Primitive(String name, Environment env, Map<Symbol, Object> metadata)
    {
        Symbol sym = Symbol.intern(name);
        env.addBinding(sym, this);
        originalName = name;
        // Always set java-class
        env.setMeta(sym, JAVA_CLASS_KEY, this.getClass().getName());
        // Set provided metadata
        if (metadata != null)
        {
            for (Map.Entry<Symbol, Object> entry : metadata.entrySet())
            {
                env.setMeta(sym, entry.getKey(), entry.getValue());
            }
        }
        // If no doc in metadata, try to extract from JavaDoc
        if (metadata == null || !metadata.containsKey(DOC_KEY))
        {
            String javadoc = extractJavadoc();
            if (javadoc != null && !javadoc.isEmpty())
            {
                env.setMeta(sym, DOC_KEY, javadoc);
            }
        }
    }

    /**
     * Extracts JavaDoc comment for this class using therapi-runtime-javadoc.
     * Only works for named classes compiled with the annotation processor.
     *
     * @return the JavaDoc comment as plain text, or null if not available
     */
    private String extractJavadoc()
    {
        Class<?> clazz = this.getClass();
        // Anonymous classes won't have JavaDoc captured
        if (clazz.isAnonymousClass())
        {
            return null;
        }
        ClassJavadoc classDoc = RuntimeJavadoc.getJavadoc(clazz);
        if (classDoc.isEmpty())
        {
            return null;
        }
        String formatted = FORMATTER.format(classDoc.getComment());
        return formatted.isEmpty() ? null : formatted;
    }

    /**
     * Returns a description of this primitive for debugging.
     *
     * @return a string containing the primitive name and documentation
     */
    public String describe()
    {
        return "JLLL primitive with original name: " + originalName + "\n" + "Doc: " + getDoc();
    }

    /**
     * Returns the documentation string.
     * This method is kept for backward compatibility but documentation
     * should primarily be accessed via metadata using {@code (doc 'name)} or {@code (meta 'name :doc)}.
     *
     * @return the documentation string, or empty string if not documented
     */
    @Override
    public String getDoc()
    {
        // For backward compatibility, try to extract JavaDoc
        String javadoc = extractJavadoc();
        return javadoc != null ? javadoc : "";
    }

    /**
     * Creates a metadata map with a single documentation entry.
     * Convenience method for creating primitives with just documentation.
     *
     * @param doc
     *            the documentation string
     * @return a map containing the doc entry
     */
    public static Map<Symbol, Object> docMeta(String doc)
    {
        Map<Symbol, Object> meta = new HashMap<>();
        meta.put(DOC_KEY, doc);
        return meta;
    }
}
