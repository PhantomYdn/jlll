package ru.ydn.jlll.util;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.dynamic.loading.ClassLoadingStrategy;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.bind.annotation.AllArguments;
import net.bytebuddy.implementation.bind.annotation.RuntimeType;
import net.bytebuddy.matcher.ElementMatchers;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Procedure;

/**
 * Converts JLLL procedures to Java functional interface implementations using ByteBuddy.
 *
 * <p>
 * This class enables transparent interoperability between JLLL lambdas/procedures and Java's
 * functional interfaces (SAM - Single Abstract Method interfaces). When a Java method expects
 * a functional interface like {@code Runnable}, {@code Comparator}, or {@code ActionListener},
 * a JLLL procedure can be automatically converted.
 * </p>
 *
 * <p>
 * Example usage in JLLL:
 * </p>
 *
 * <pre>
 * ;; Automatically converts lambda to ActionListener
 * (invoke button "addActionListener"
 *   (lambda (event) (println "Clicked!")))
 *
 * ;; Automatically converts lambda to Runnable
 * (invoke executor "submit"
 *   (lambda () (println "Running!")))
 *
 * ;; Automatically converts lambda to Comparator
 * (invoke-static 'java.util.Collections "sort" my-list
 *   (lambda (a b) (- a b)))
 * </pre>
 *
 * <p>
 * Generated wrapper classes are cached for performance - the same interface will reuse
 * its generated implementation class.
 * </p>
 */
public class SamConverter
{
    /** Cache of generated wrapper classes by interface type. */
    private static final Map<Class<?>, Class<?>> wrapperClassCache = new ConcurrentHashMap<>();
    /** ByteBuddy instance for class generation. */
    private static final ByteBuddy byteBuddy = new ByteBuddy();

    /**
     * Checks if a class is a functional interface (SAM - Single Abstract Method).
     *
     * <p>
     * A functional interface has exactly one abstract method. This includes interfaces
     * annotated with {@code @FunctionalInterface} as well as any interface that happens
     * to have a single abstract method.
     * </p>
     *
     * @param clazz
     *            the class to check
     * @return true if clazz is a functional interface
     */
    public static boolean isFunctionalInterface(Class<?> clazz)
    {
        if (!clazz.isInterface())
        {
            return false;
        }
        int abstractMethodCount = 0;
        for (Method method : clazz.getMethods())
        {
            // Skip default methods and static methods
            if (method.isDefault() || Modifier.isStatic(method.getModifiers()))
            {
                continue;
            }
            // Skip methods inherited from Object (equals, hashCode, toString)
            if (isObjectMethod(method))
            {
                continue;
            }
            if (Modifier.isAbstract(method.getModifiers()))
            {
                abstractMethodCount++;
                if (abstractMethodCount > 1)
                {
                    return false;
                }
            }
        }
        return abstractMethodCount == 1;
    }

    /**
     * Gets the single abstract method of a functional interface.
     *
     * @param functionalInterface
     *            the functional interface class
     * @return the single abstract method
     * @throws IllegalArgumentException
     *             if not a functional interface
     */
    public static Method getSamMethod(Class<?> functionalInterface)
    {
        if (!functionalInterface.isInterface())
        {
            throw new IllegalArgumentException("Not an interface: " + functionalInterface.getName());
        }
        for (Method method : functionalInterface.getMethods())
        {
            if (method.isDefault() || Modifier.isStatic(method.getModifiers()))
            {
                continue;
            }
            if (isObjectMethod(method))
            {
                continue;
            }
            if (Modifier.isAbstract(method.getModifiers()))
            {
                return method;
            }
        }
        throw new IllegalArgumentException("No abstract method found in: " + functionalInterface.getName());
    }

    /**
     * Checks if a method is one of the public methods from Object class
     * (equals, hashCode, toString) that interfaces may declare.
     */
    private static boolean isObjectMethod(Method method)
    {
        String name = method.getName();
        Class<?>[] params = method.getParameterTypes();
        if (name.equals("equals") && params.length == 1 && params[0] == Object.class)
        {
            return true;
        }
        if (name.equals("hashCode") && params.length == 0)
        {
            return true;
        }
        if (name.equals("toString") && params.length == 0)
        {
            return true;
        }
        return false;
    }

    /**
     * Converts a JLLL procedure to an implementation of the specified functional interface.
     *
     * <p>
     * The generated implementation will call the JLLL procedure when the interface's
     * single abstract method is invoked, passing all arguments as a JLLL list.
     * </p>
     *
     * @param <T>
     *            the functional interface type
     * @param functionalInterface
     *            the target functional interface class
     * @param procedure
     *            the JLLL procedure to wrap
     * @param environment
     *            the environment for procedure evaluation
     * @return an instance of the functional interface that delegates to the procedure
     * @throws JlllException
     *             if conversion fails
     */
    @SuppressWarnings("unchecked")
    public static <T> T convert(Class<T> functionalInterface, Procedure procedure, Environment environment)
            throws JlllException
    {
        if (!isFunctionalInterface(functionalInterface))
        {
            throw new JlllException(
                    "Cannot convert procedure to non-functional interface: " + functionalInterface.getName());
        }
        try
        {
            Class<?> wrapperClass = getOrCreateWrapperClass(functionalInterface, environment.getClassLoader());
            // Create instance with procedure and environment
            Object instance = wrapperClass.getDeclaredConstructor(Procedure.class, Environment.class)
                    .newInstance(procedure, environment);
            return (T) instance;
        }
        catch (JlllException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new JlllException("Failed to convert procedure to " + functionalInterface.getName(), e);
        }
    }

    /**
     * Gets or creates a wrapper class for the given functional interface.
     * Results are cached for performance.
     */
    private static Class<?> getOrCreateWrapperClass(Class<?> functionalInterface, ClassLoader classLoader)
            throws Exception
    {
        return wrapperClassCache.computeIfAbsent(functionalInterface, iface ->
        {
            try
            {
                return createWrapperClass(iface, classLoader);
            }
            catch (Exception e)
            {
                throw new RuntimeException("Failed to create wrapper for " + iface.getName(), e);
            }
        });
    }

    /**
     * Creates a new wrapper class for the functional interface using ByteBuddy.
     */
    private static Class<?> createWrapperClass(Class<?> functionalInterface, ClassLoader classLoader) throws Exception
    {
        String wrapperClassName = "ru.ydn.jlll.generated.Sam$" + functionalInterface.getSimpleName() + "$Wrapper";
        DynamicType.Builder<?> builder = byteBuddy.subclass(Object.class).name(wrapperClassName)
                .implement(functionalInterface)
                // Add fields for procedure and environment
                .defineField("procedure", Procedure.class, Visibility.PRIVATE)
                .defineField("environment", Environment.class, Visibility.PRIVATE)
                // Add constructor that properly calls super() and sets fields
                .defineConstructor(Visibility.PUBLIC).withParameters(Procedure.class, Environment.class)
                .intercept(net.bytebuddy.implementation.MethodCall.invoke(Object.class.getConstructor())
                        .andThen(FieldAccessor.ofField("procedure").setsArgumentAt(0))
                        .andThen(FieldAccessor.ofField("environment").setsArgumentAt(1)))
                // Intercept the SAM method
                .method(ElementMatchers.isAbstract()).intercept(MethodDelegation.to(SamMethodInterceptor.class));
        DynamicType.Unloaded<?> unloaded = builder.make();
        return unloaded.load(classLoader, ClassLoadingStrategy.Default.INJECTION).getLoaded();
    }

    /**
     * Interceptor for SAM method calls - delegates to the JLLL procedure.
     */
    public static class SamMethodInterceptor
    {
        /**
         * Called when the functional interface method is invoked.
         * Delegates to the JLLL procedure.
         *
         * @param procedure
         *            the JLLL procedure (injected from field)
         * @param environment
         *            the environment (injected from field)
         * @param args
         *            the method arguments
         * @return the result of the procedure call
         */
        @RuntimeType
        public static Object intercept(
                @net.bytebuddy.implementation.bind.annotation.FieldValue("procedure") Procedure procedure,
                @net.bytebuddy.implementation.bind.annotation.FieldValue("environment") Environment environment,
                @AllArguments Object[] args) throws Exception
        {
            // Convert arguments to JLLL cons list
            Cons argList = ListUtil.toCons(args);
            // Call the procedure
            Object result = procedure.applyEvaluated(argList, environment);
            // Convert null result for void methods
            return CommonUtil.avoidNull(result);
        }
    }

    /**
     * Checks if a JLLL value (expected to be a Procedure) can be converted to the target type.
     *
     * @param targetType
     *            the expected parameter type
     * @param value
     *            the JLLL value
     * @return true if value is a Procedure and targetType is a functional interface
     */
    public static boolean canConvert(Class<?> targetType, Object value)
    {
        return value instanceof Procedure && isFunctionalInterface(targetType);
    }

    /**
     * Converts a JLLL value to the target functional interface type if possible.
     *
     * @param targetType
     *            the expected parameter type (must be a functional interface)
     * @param value
     *            the JLLL value (must be a Procedure)
     * @param environment
     *            the environment for procedure evaluation
     * @return the converted value, or the original value if conversion not applicable
     * @throws JlllException
     *             if conversion fails
     */
    public static Object convertIfNeeded(Class<?> targetType, Object value, Environment environment)
            throws JlllException
    {
        if (canConvert(targetType, value))
        {
            return convert(targetType, (Procedure) value, environment);
        }
        return value;
    }
}
