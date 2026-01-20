package ru.ydn.jlll.common.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to provide documentation for JLLL primitives.
 * The documentation is accessible at runtime via the (doc) function.
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * &#64;JlllName("my-function")
 * &#64;JlllDoc("Computes something useful. Args: (x y)")
 * public Object myFunction(Enviroment env, Object x, Object y) { ... }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(
{ElementType.METHOD, ElementType.TYPE})
public @interface JlllDoc
{
    /**
     * The documentation string for this primitive.
     *
     * @return the documentation text
     */
    public String value() default "";
}
