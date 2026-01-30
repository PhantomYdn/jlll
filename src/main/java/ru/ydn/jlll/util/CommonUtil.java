package ru.ydn.jlll.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Procedure;

/**
 * Common utility methods for Java reflection and type handling.
 * Used internally by JLLL for Java interoperability operations.
 */
public class CommonUtil
{
    /**
     * Converts null to Null.NULL for JLLL compatibility.
     *
     * @param ret
     *            the value to prepare
     * @return the value, or Null.NULL if null
     */
    public static Object prepareReturn(Object ret)
    {
        return ret == null ? Null.NULL : ret;
    }

    /**
     * Converts Null.NULL to Java null.
     *
     * @param obj
     *            the value to convert
     * @return null if Null.NULL, otherwise the original value
     */
    public static Object avoidNull(Object obj)
    {
        if (Null.NULL.equals(obj))
            return null;
        return obj;
    }

    /**
     * Creates an object using reflection, finding a matching constructor.
     *
     * @param clss
     *            the class to instantiate
     * @param constr
     *            constructor arguments
     * @return the new instance
     * @throws JlllException
     *             if no matching constructor or instantiation fails
     */
    public static Object constactObject(Class<?> clss, Object[] constr) throws JlllException
    {
        return constactObject(clss, constr, null);
    }

    /**
     * Creates an object using reflection, finding a matching constructor.
     * Supports SAM (functional interface) conversion when environment is provided.
     *
     * @param clss
     *            the class to instantiate
     * @param constr
     *            constructor arguments
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return the new instance
     * @throws JlllException
     *             if no matching constructor or instantiation fails
     */
    public static Object constactObject(Class<?> clss, Object[] constr, Environment env) throws JlllException
    {
        try
        {
            if (constr != null)
            {
                Constructor<?>[] cnst = clss.getConstructors();
                for (int i = 0; i < cnst.length; i++)
                {
                    Class<?>[] classes = cnst[i].getParameterTypes();
                    if (matchClasses(classes, constr, env))
                    {
                        Object[] convertedArgs = convertArgs(classes, constr, env);
                        return cnst[i].newInstance(convertedArgs);
                    }
                }
                //Creating exception
                String message = "No such constructor for: ";
                for (int i = 0; i < constr.length; i++)
                {
                    Object obj = constr[i];
                    message += (i > 0 ? ", " : "") + (obj == null ? "NULL" : obj.getClass().getName());
                }
                throw new JlllException(message);
            }
            else
            {
                return clss.getDeclaredConstructor().newInstance();
            }
        }
        catch (JlllException e)
        {
            throw e;
        }
        catch (InstantiationException e)
        {
            throw new JlllException("Exception in creating object from class " + clss.getName(), e);
        }
        catch (IllegalAccessException e)
        {
            throw new JlllException("Exception in creating object from class " + clss.getName(), e);
        }
        catch (InvocationTargetException e)
        {
            throw new JlllException("Exception in creating object from class " + clss.getName(),
                    e.getTargetException());
        }
        catch (NoSuchMethodException e)
        {
            throw new JlllException("No default constructor for class " + clss.getName(), e);
        }
    }

    /**
     * Invokes a method on an object using reflection.
     *
     * @param object
     *            the target object
     * @param methodS
     *            the method name
     * @param params
     *            method arguments
     * @return the method result
     * @throws JlllException
     *             if method not found or invocation fails
     */
    public static Object invoke(Object object, String methodS, Object[] params) throws JlllException
    {
        return invoke(object, methodS, params, null);
    }

    /**
     * Invokes a method on an object using reflection, with SAM conversion support.
     *
     * <p>
     * If environment is provided and a parameter is a JLLL Procedure where the Java method
     * expects a functional interface, the procedure will be automatically converted.
     * </p>
     *
     * @param object
     *            the target object
     * @param methodS
     *            the method name
     * @param params
     *            method arguments
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return the method result
     * @throws JlllException
     *             if method not found or invocation fails
     */
    public static Object invoke(Object object, String methodS, Object[] params, Environment env) throws JlllException
    {
        if (object == null)
            throw new JlllException("Object for invoke is null");
        if (methodS == null)
            throw new JlllException("Method for invoke is null");
        try
        {
            Method[] methods = object.getClass().getMethods();
            for (int i = 0; i < methods.length; i++)
            {
                Class<?>[] classes = methods[i].getParameterTypes();
                if (methodS.equals(methods[i].getName()) && matchClasses(classes, params, env))
                {
                    Object[] convertedParams = convertArgs(classes, params, env);
                    return methods[i].invoke(object, convertedParams);
                }
            }
        }
        catch (IllegalAccessException e)
        {
            throw new JlllException("Exception in invoke " + object.getClass().getName() + "." + methodS, e);
        }
        catch (InvocationTargetException e)
        {
            throw new JlllException("Exception in invoke " + object.getClass().getName() + "." + methodS + " "
                    + e.getTargetException().getMessage(), e.getTargetException());
        }
        throw new JlllException("No such method: " + object.getClass().getName() + "." + methodS + "()");
    }

    /**
     * Invokes a static method using reflection.
     *
     * @param clss
     *            the class containing the method
     * @param methodS
     *            the method name
     * @param params
     *            method arguments
     * @return the method result
     * @throws JlllException
     *             if method not found or invocation fails
     */
    public static Object invokeStatic(Class<?> clss, String methodS, Object[] params) throws JlllException
    {
        return invokeStatic(clss, methodS, params, null);
    }

    /**
     * Invokes a static method using reflection, with SAM conversion support.
     *
     * @param clss
     *            the class containing the method
     * @param methodS
     *            the method name
     * @param params
     *            method arguments
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return the method result
     * @throws JlllException
     *             if method not found or invocation fails
     */
    public static Object invokeStatic(Class<?> clss, String methodS, Object[] params, Environment env)
            throws JlllException
    {
        if (clss == null)
            throw new JlllException("Object for invoke is null");
        if (methodS == null)
            throw new JlllException("Method for invoke is null");
        try
        {
            Method[] methods = clss.getMethods();
            for (int i = 0; i < methods.length; i++)
            {
                Class<?>[] classes = methods[i].getParameterTypes();
                if (methodS.equals(methods[i].getName()) && matchClasses(classes, params, env))
                {
                    Object[] convertedParams = convertArgs(classes, params, env);
                    return methods[i].invoke(null, convertedParams);
                }
            }
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in invoke " + clss.getName() + "." + methodS, e);
        }
        throw new JlllException("No such method: " + clss.getName() + "." + methodS + "()");
    }

    /**
     * Gets an instance field value using reflection.
     *
     * @param object
     *            the target object
     * @param field
     *            the field name
     * @return the field value
     * @throws JlllException
     *             if field access fails
     */
    public static Object peek(Object object, String field) throws JlllException
    {
        if (object == null)
            throw new JlllException("Object for invoke is null");
        if (field == null)
            throw new JlllException("Method for invoke is null");
        try
        {
            return object.getClass().getField(field).get(object);
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in peek", e);
        }
    }

    /**
     * Sets an instance field value using reflection.
     *
     * @param object
     *            the target object
     * @param field
     *            the field name
     * @param value
     *            the value to set
     * @return the value that was set
     * @throws JlllException
     *             if field access fails
     */
    public static Object poke(Object object, String field, Object value) throws JlllException
    {
        if (object == null)
            throw new JlllException("Object for invoke is null");
        if (field == null)
            throw new JlllException("Method for invoke is null");
        try
        {
            object.getClass().getField(field).set(object, value);
            return value;
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in poke", e);
        }
    }

    /**
     * Gets a static field value using reflection.
     *
     * @param clss
     *            the class containing the field
     * @param field
     *            the field name
     * @return the field value
     * @throws JlllException
     *             if field access fails
     */
    public static Object peekStatic(Class<?> clss, String field) throws JlllException
    {
        if (clss == null)
            throw new JlllException("Class for peekStatic is null");
        if (field == null)
            throw new JlllException("Field for peekStatic is null");
        try
        {
            return clss.getField(field).get(null);
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in peek-static", e);
        }
    }

    /**
     * Sets a static field value using reflection.
     *
     * @param clss
     *            the class containing the field
     * @param field
     *            the field name
     * @param value
     *            the value to set
     * @return the value that was set
     * @throws JlllException
     *             if field access fails
     */
    public static Object pokeStatic(Class<?> clss, String field, Object value) throws JlllException
    {
        if (clss == null)
            throw new JlllException("Class for pokeStatic is null");
        if (field == null)
            throw new JlllException("Field for pokeStatic is null");
        try
        {
            clss.getField(field).set(null, value);
            return value;
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in peek", e);
        }
    }

    /**
     * Extracts Class objects from an array of objects.
     *
     * @param objects
     *            the objects
     * @return array of their Class types
     */
    public static Class<?>[] objectsToClasses(Object[] objects)
    {
        Class<?>[] classes = new Class[objects.length];
        for (int i = 0; i < objects.length; i++)
        {
            classes[i] = objects[i].getClass();
        }
        return classes;
    }

    /**
     * Checks if objects can be passed as parameters to a method with given parameter types.
     *
     * @param classes
     *            the method parameter types
     * @param last
     *            the actual argument objects
     * @return true if arguments match parameters
     */
    public static boolean matchClasses(Class<?>[] classes, Object[] last)
    {
        return matchClasses(classes, last, null);
    }

    /**
     * Checks if objects can be passed as parameters to a method with given parameter types.
     * Supports SAM (functional interface) conversion when environment is provided.
     *
     * @param classes
     *            the method parameter types
     * @param last
     *            the actual argument objects
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return true if arguments match parameters
     */
    public static boolean matchClasses(Class<?>[] classes, Object[] last, Environment env)
    {
        if (classes == null || classes.length == 0)
            return last == null || last.length == 0;
        else if (last == null || last.length != classes.length)
            return false;
        for (int i = 0; i < classes.length; i++)
        {
            if (!isInstance(classes[i], last[i], env))
                return false;
        }
        return true;
    }

    /**
     * Checks if an object is assignable to a class (null-safe, handles primitives).
     * Supports Java primitive widening conversions (JLS §5.1.2) for numeric types.
     *
     * @param clss
     *            the target class
     * @param object
     *            the object to check
     * @return true if object can be assigned to clss
     */
    public static boolean isInstance(Class<?> clss, Object object)
    {
        return isInstance(clss, object, null);
    }

    /**
     * Checks if an object is assignable to a class (null-safe, handles primitives).
     * Supports Java primitive widening conversions (JLS §5.1.2) for numeric types.
     * Also supports SAM (functional interface) conversion when environment is provided.
     *
     * @param clss
     *            the target class
     * @param object
     *            the object to check
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return true if object can be assigned to clss
     */
    public static boolean isInstance(Class<?> clss, Object object, Environment env)
    {
        if (object == null || Null.NULL.equals(object))
            return true;
        if (clss.isAssignableFrom(object.getClass()))
            return true;
        // Check for numeric primitive widening
        if (object instanceof Number && canWidenNumeric(clss, object.getClass()))
            return true;
        // Check for SAM (functional interface) conversion: Procedure -> functional interface
        if (env != null && object instanceof Procedure && SamConverter.isFunctionalInterface(clss))
            return true;
        try
        {
            return object.getClass().getField("TYPE").get(object).equals(clss);
        }
        catch (Exception e)
        {
            return false;
        }
    }

    /**
     * Checks if a numeric type can be widened to another type.
     * Follows Java Language Specification §5.1.2 for primitive widening.
     *
     * @param targetType
     *            the target primitive or wrapper type
     * @param sourceType
     *            the source object's type
     * @return true if source can be widened to target
     */
    private static boolean canWidenNumeric(Class<?> targetType, Class<?> sourceType)
    {
        // double accepts any numeric type
        if (targetType == double.class || targetType == Double.class)
        {
            return Number.class.isAssignableFrom(sourceType);
        }
        // float accepts int, long, short, byte, float
        if (targetType == float.class || targetType == Float.class)
        {
            return sourceType == Integer.class || sourceType == int.class || sourceType == Long.class
                    || sourceType == long.class || sourceType == Short.class || sourceType == short.class
                    || sourceType == Byte.class || sourceType == byte.class || sourceType == Float.class
                    || sourceType == float.class;
        }
        // long accepts int, short, byte, long
        if (targetType == long.class || targetType == Long.class)
        {
            return sourceType == Integer.class || sourceType == int.class || sourceType == Short.class
                    || sourceType == short.class || sourceType == Byte.class || sourceType == byte.class
                    || sourceType == Long.class || sourceType == long.class;
        }
        // int accepts short, byte, int
        if (targetType == int.class || targetType == Integer.class)
        {
            return sourceType == Short.class || sourceType == short.class || sourceType == Byte.class
                    || sourceType == byte.class || sourceType == Integer.class || sourceType == int.class;
        }
        // short accepts byte, short
        if (targetType == short.class || targetType == Short.class)
        {
            return sourceType == Byte.class || sourceType == byte.class || sourceType == Short.class
                    || sourceType == short.class;
        }
        return false;
    }

    /**
     * Converts a numeric argument to the target type if widening is needed.
     *
     * @param targetType
     *            the expected parameter type
     * @param arg
     *            the argument value
     * @return the converted value, or original if no conversion needed
     */
    private static Object convertNumericArg(Class<?> targetType, Object arg)
    {
        if (arg == null || !(arg instanceof Number))
            return arg;
        Number num = (Number) arg;
        if (targetType == double.class || targetType == Double.class)
            return num.doubleValue();
        if (targetType == float.class || targetType == Float.class)
            return num.floatValue();
        if (targetType == long.class || targetType == Long.class)
            return num.longValue();
        if (targetType == int.class || targetType == Integer.class)
            return num.intValue();
        if (targetType == short.class || targetType == Short.class)
            return num.shortValue();
        if (targetType == byte.class || targetType == Byte.class)
            return num.byteValue();
        return arg;
    }

    /**
     * Converts an array of arguments to match the expected parameter types.
     * Handles numeric widening conversions.
     *
     * @param paramTypes
     *            the expected parameter types
     * @param args
     *            the argument values
     * @return array with converted arguments
     */
    private static Object[] convertArgs(Class<?>[] paramTypes, Object[] args)
    {
        try
        {
            return convertArgs(paramTypes, args, null);
        }
        catch (JlllException e)
        {
            // Should never happen without environment (no SAM conversion)
            throw new RuntimeException("Unexpected error in convertArgs", e);
        }
    }

    /**
     * Converts an array of arguments to match the expected parameter types.
     * Handles numeric widening conversions and SAM (functional interface) conversion.
     *
     * @param paramTypes
     *            the expected parameter types
     * @param args
     *            the argument values
     * @param env
     *            the environment for SAM conversion (may be null)
     * @return array with converted arguments
     * @throws JlllException
     *             if SAM conversion fails
     */
    private static Object[] convertArgs(Class<?>[] paramTypes, Object[] args, Environment env) throws JlllException
    {
        if (args == null || paramTypes == null)
            return args;
        Object[] result = new Object[args.length];
        for (int i = 0; i < args.length; i++)
        {
            Object arg = args[i];
            Class<?> targetType = paramTypes[i];
            // First try SAM conversion if applicable
            if (env != null && arg instanceof Procedure && SamConverter.isFunctionalInterface(targetType))
            {
                result[i] = SamConverter.convert(targetType, (Procedure) arg, env);
            }
            else
            {
                // Fall back to numeric conversion
                result[i] = convertNumericArg(targetType, arg);
            }
        }
        return result;
    }

    /**
     * Finds a method by name in a class (returns first match).
     *
     * @param clazz
     *            the class to search
     * @param name
     *            the method name
     * @return the Method, or null if not found
     */
    public static Method getMethodByName(Class<?> clazz, String name)
    {
        Method[] methods = clazz.getMethods();
        for (int i = 0; i < methods.length; i++)
        {
            Method method = methods[i];
            if (method.getName().equals(name))
                return method;
        }
        return null;
    }

    /**
     * Creates a Number instance of the specified type from a double value.
     *
     * @param ret
     *            the double value
     * @param clazz
     *            the desired Number class (Integer, Double, etc.)
     * @return a Number instance of the specified type
     * @throws JlllException
     *             if instantiation fails
     */
    public static Object getNumber(double ret, Class<?> clazz) throws JlllException
    {
        try
        {
            return clazz.getConstructor(new Class[]
            {String.class}).newInstance(new Object[]
            {"" + ret});
        }
        catch (InvocationTargetException e)
        {
            if (e.getCause() instanceof NumberFormatException)
            {
                try
                {
                    return clazz.getConstructor(new Class[]
                    {String.class}).newInstance(new Object[]
                    {"" + (int) ret});
                }
                catch (Exception e1)
                {
                    throw new JlllException("Can't instanciate result");
                }
            }
            else
            {
                throw new JlllException("Can't instanciate result");
            }
        }
        catch (Exception e)
        {
            throw new JlllException("Can't instanciate result");
        }
    }

    /**
     * Converts a JLLL value to a boolean.
     * null, Null.NULL, and empty Cons are false; everything else is true.
     *
     * @param obj
     *            the value to convert
     * @return the boolean interpretation
     */
    public static boolean getBoolean(Object obj)
    {
        boolean decide = true;
        if (obj instanceof Boolean)
        {
            decide = ((Boolean) obj).booleanValue();
        }
        else if (obj instanceof Null)
        {
            decide = false;
        }
        else if (obj instanceof Cons)
        {
            decide = !((Cons) obj).isNull();
        }
        return decide;
    }
}
