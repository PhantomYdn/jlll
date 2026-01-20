package ru.ydn.jlll.common;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import com.github.therapi.runtimejavadoc.CommentFormatter;
import com.github.therapi.runtimejavadoc.MethodJavadoc;
import com.github.therapi.runtimejavadoc.RuntimeJavadoc;
import ru.ydn.jlll.common.Cons.ConsIterator;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * A primitive that wraps a Java method, exposing it as a JLLL function.
 * Methods are discovered via the {@link JlllName} annotation.
 *
 * <p>
 * Documentation is extracted from the method's JavaDoc comment at compile time
 * using therapi-runtime-javadoc and stored as metadata on the binding.
 * </p>
 */
public class ReflectionPrimitive extends Primitive
{
    private static final long serialVersionUID = -7400033818353226326L;
    /** Shared formatter for converting JavaDoc comments to plain text */
    private static final CommentFormatter FORMATTER = new CommentFormatter();
    /** Symbol for documentation metadata key */
    private static final Symbol DOC_KEY = Symbol.intern("doc");
    /** Symbol for Java class metadata key */
    private static final Symbol JAVA_CLASS_KEY = Symbol.intern("java-class");
    /** Symbol for Java method metadata key */
    private static final Symbol JAVA_METHOD_KEY = Symbol.intern("java-method");
    protected final Object obj;
    protected final Method method;
    protected boolean useEvaluated = true;

    private ReflectionPrimitive(String name, Enviroment env, Object obj, Method method, boolean useEvaluated)
    {
        // Use the doc-less super constructor to avoid double metadata setting
        super(name, env, (String) null);
        this.obj = obj;
        this.method = method;
        this.useEvaluated = useEvaluated;
        // Now set method-specific metadata
        Symbol sym = Symbol.intern(name);
        // Override java-class with the actual containing class (not the anonymous ReflectionPrimitive)
        env.setMeta(sym, JAVA_CLASS_KEY, obj.getClass().getName());
        // Add java-method metadata
        env.setMeta(sym, JAVA_METHOD_KEY, method.getName());
        // Extract and set documentation from method JavaDoc
        String doc = extractMethodJavadoc();
        if (doc != null && !doc.isEmpty())
        {
            env.setMeta(sym, DOC_KEY, doc);
        }
    }

    /**
     * Creates a ReflectionPrimitive from a method annotated with {@link JlllName}.
     *
     * @param env
     *            the environment to bind the primitive in
     * @param obj
     *            the object instance containing the method
     * @param method
     *            the method to wrap
     * @return the created ReflectionPrimitive
     * @throws JlllException
     *             if the method is not properly annotated
     */
    public static ReflectionPrimitive createReflectionPrimitive(Enviroment env, Object obj, Method method)
            throws JlllException
    {
        JlllName jlllName = method.getAnnotation(JlllName.class);
        if (jlllName == null)
            throw new JlllException("Method must be annotated by JlllName");
        if (jlllName.value() == null)
            throw new JlllException("Name is not specified");
        String name = jlllName.value();
        boolean useEvaluated = jlllName.useEvaluated();
        return createReflectionPrimitive(env, obj, method, name, useEvaluated);
    }

    /**
     * Creates a ReflectionPrimitive with explicit name and evaluation settings.
     *
     * @param env
     *            the environment to bind the primitive in
     * @param obj
     *            the object instance containing the method
     * @param method
     *            the method to wrap
     * @param name
     *            the JLLL name for the primitive
     * @param useEvaluated
     *            whether arguments should be evaluated before passing to the method
     * @return the created ReflectionPrimitive
     * @throws JlllException
     *             if creation fails
     */
    public static ReflectionPrimitive createReflectionPrimitive(Enviroment env, Object obj, Method method, String name,
            boolean useEvaluated) throws JlllException
    {
        ReflectionPrimitive primitive = new ReflectionPrimitive(name, env, obj, method, useEvaluated);
        return primitive;
    }

    /**
     * Extracts JavaDoc comment for the wrapped method using therapi-runtime-javadoc.
     *
     * @return the JavaDoc comment as plain text, or null if not available
     */
    private String extractMethodJavadoc()
    {
        MethodJavadoc methodDoc = RuntimeJavadoc.getJavadoc(method);
        if (methodDoc.isEmpty())
        {
            return null;
        }
        String formatted = FORMATTER.format(methodDoc.getComment());
        return formatted.isEmpty() ? null : formatted;
    }

    @Override
    public Object applay(Cons values, Enviroment env) throws JlllException
    {
        return useEvaluated ? super.applay(values, env) : invoke(values, env);
    }

    @Override
    public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
    {
        return useEvaluated ? invoke(values, env) : super.applayEvaluated(values, env);
    }

    protected Object invoke(Cons values, Enviroment env) throws JlllException
    {
        Class<?>[] params = method.getParameterTypes();
        Object[] vals = new Object[params.length];
        boolean setEnvironment = params != null && params.length > 0 && Enviroment.class.isAssignableFrom(params[0]);
        int i = 0;
        if (setEnvironment)
        {
            vals[0] = env;
            i = 1;
        }
        ConsIterator it = values.iterator();
        for (; i < params.length; i++)
        {
            Class<?> paramType = params[i];
            boolean isLast = (i == params.length - 1);
            if (isLast && paramType.isArray())
            {
                Class<?> arrayType = paramType.getComponentType();
                Cons tail = it.getTail();
                Object array = Array.newInstance(arrayType, tail.length());
                int j = 0;
                for (Object object : tail)
                {
                    Object toSet = convert(object, arrayType, env);
                    if (toSet != null && !arrayType.isAssignableFrom(toSet.getClass()))
                        throw new JlllException(
                                "Incorrect type for tail. Required " + arrayType + " but " + object.getClass());
                    Array.set(array, j++, toSet);
                }
                //Object[] lastValue = ListUtil.listVector(tail);
                vals[i] = array;
            }
            else
            {
                if (it.hasNext())
                {
                    Object next = it.next();
                    Object toSet = convert(next, paramType, env);
                    if (toSet != null && !paramType.isAssignableFrom(toSet.getClass()))
                        throw new JlllException("Argument #" + (setEnvironment ? i - 1 : i) + " should be " + paramType
                                + " but " + next.getClass());
                    vals[i] = toSet;
                }
                else
                    throw new JlllException("No enought arguments. Required #" + (setEnvironment ? i - 1 : i)
                            + " of type " + paramType);
            }
        }
        if (it.hasNext())
            throw new JlllException("So many arguments");
        try
        {
            return method.invoke(obj, vals);
        }
        catch (InvocationTargetException e)
        {
            if (e.getTargetException() instanceof JlllException)
                throw (JlllException) e.getTargetException();
            else
                throw new JlllException(e);
        }
        catch (Exception e)
        {
            throw new JlllException(e);
        }
    }

    private Object convert(Object value, Class<?> requiredClass, Enviroment env)
    {
        if (value instanceof Null)
            return null;
        else if (requiredClass.equals(Object.class))
            return value;
        else if (value instanceof Symbol && requiredClass.equals(String.class))
            return ((Symbol) value).getName();
        else if (value instanceof Integer && requiredClass.equals(BigInteger.class))
            return new BigInteger(value.toString());
        else
        {
            Object convertors = env.lookup("jlll-convertors");
            if (convertors != null)
            {
                for (Object convertor : (Cons) convertors)
                {
                    Convertor conv = (Convertor) convertor;
                    value = conv.convert(value, requiredClass, env);
                    if (requiredClass.isAssignableFrom(value.getClass()))
                    {
                        return value;
                    }
                }
            }
            return value;
        }
    }

    @Override
    public String getDoc()
    {
        String doc = extractMethodJavadoc();
        return doc != null ? doc : "";
    }

    @Override
    public String describe()
    {
        StringWriter sw = new StringWriter();
        PrintWriter out = new PrintWriter(sw);
        out.println("ReflectionPrimitive wraps class: '" + (obj == null ? "NULL" : obj.getClass().getName())
                + "' method: " + method.getName());
        out.println("Arguments: ");
        Class<?>[] paramsClasses = method.getParameterTypes();
        for (int i = 0; i < paramsClasses.length; i++)
        {
            Class<?> class1 = paramsClasses[i];
            out.println("Class of argument #" + i + ":" + class1.getName());
        }
        out.println("Doc: " + getDoc());
        return sw.toString();
    }
}
