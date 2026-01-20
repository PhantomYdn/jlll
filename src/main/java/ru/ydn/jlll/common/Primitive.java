package ru.ydn.jlll.common;

import ru.ydn.jlll.common.annotation.JlllDoc;

/**
 * Base class for built-in JLLL functions (primitives).
 * Primitives are automatically bound to the environment upon construction.
 *
 * <p>
 * Subclasses must implement {@link Procedure#applay(Cons, Enviroment)} or
 * {@link Procedure#applayEvaluated(Cons, Enviroment)} to define the primitive's behavior.
 * </p>
 */
public class Primitive extends Procedure
{
    private static final long serialVersionUID = -9190619708410061311L;
    /** The original name used when binding this primitive */
    protected final String originalName;

    /**
     * Creates a primitive and binds it to the given environment.
     *
     * @param name
     *            the primitive name (used for binding and error messages)
     * @param env
     *            the environment to bind this primitive in
     */
    public Primitive(String name, Enviroment env)
    {
        env.addBinding(Symbol.intern(name), this);
        originalName = name;
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
     * Returns the documentation string from the {@link JlllDoc} annotation, if present.
     *
     * @return the documentation string, or empty string if not documented
     */
    @Override
    public String getDoc()
    {
        Class<?> clazz = this.getClass();
        JlllDoc doc = clazz.getAnnotation(JlllDoc.class);
        return doc == null ? "" : doc.value();
    }
}
