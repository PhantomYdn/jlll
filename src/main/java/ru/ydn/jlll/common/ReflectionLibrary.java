package ru.ydn.jlll.common;

import java.lang.reflect.Method;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Base class for libraries that expose Java methods as JLLL primitives via reflection.
 * Methods annotated with {@link JlllName} are automatically registered as primitives.
 *
 * <p>
 * Example library:
 * </p>
 *
 * <pre>
 * public class MyLib extends ReflectionLibrary
 * {
 *     &#64;JlllName("greet")
 *     public String greet(Environment env, String name)
 *     {
 *         return "Hello, " + name;
 *     }
 * }
 * </pre>
 */
public abstract class ReflectionLibrary implements Library
{
    /**
     * Loads this library by scanning for {@link JlllName} annotated methods.
     *
     * @param env
     *            the environment to load into
     * @throws JlllException
     *             if loading fails
     */
    public void load(Environment env) throws JlllException
    {
        loadMethods(this, env);
    }

    /**
     * Scans an object for {@link JlllName} annotated methods and registers them as primitives.
     *
     * @param obj
     *            the object containing annotated methods
     * @param env
     *            the environment to register primitives in
     * @throws JlllException
     *             if registration fails
     */
    public static void loadMethods(Object obj, Environment env) throws JlllException
    {
        Method[] methods = obj.getClass().getMethods();
        for (int i = 0; i < methods.length; i++)
        {
            Method method = methods[i];
            if (method.isAnnotationPresent(JlllName.class))
            {
                ReflectionPrimitive.createReflectionPrimitive(env, obj, method);
            }
        }
    }
}
