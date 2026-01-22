package ru.ydn.jlll.common.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to bind a Java method to a JLLL primitive name.
 * Used with {@link ru.ydn.jlll.common.ReflectionLibrary} to expose Java methods as JLLL functions.
 *
 * <p>
 * Example:
 * </p>
 *
 * <pre>
 * &#64;JlllName("my-function")
 * public Object myFunction(Environment env, Object arg) { ... }
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface JlllName
{
    /**
     * The JLLL primitive name for this method.
     *
     * @return the primitive name
     */
    String value();

    /**
     * Whether arguments should be evaluated before passing to the method.
     * Set to false for special forms that need unevaluated arguments.
     *
     * @return true to evaluate arguments (default), false to pass unevaluated
     */
    boolean useEvaluated() default true;
}
