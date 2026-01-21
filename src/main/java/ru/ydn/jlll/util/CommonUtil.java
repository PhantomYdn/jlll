package ru.ydn.jlll.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;

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
        try
        {
            if (constr != null)
            {
                Constructor<?>[] cnst = clss.getConstructors();
                for (int i = 0; i < cnst.length; i++)
                {
                    Class<?>[] classes = cnst[i].getParameterTypes();
                    if (matchClasses(classes, constr))
                        return cnst[i].newInstance(constr);
                }
                //Creating exception
                String message = "Now such constructor for: ";
                for (int i = 0; i < constr.length; i++)
                {
                    Object obj = constr[i];
                    message += (i > 0 ? ", " : "") + (obj == null ? "NULL" : obj.getClass().getName());
                }
                throw new JlllException(message);
            }
            else
            {
                return clss.newInstance();
            }
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
                if (methodS.equals(methods[i].getName()) && matchClasses(classes, params))
                {
                    return methods[i].invoke(object, params);
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
        throw new JlllException("Now such method: " + object.getClass().getName() + "." + methodS + "()");
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
                if (methodS.equals(methods[i].getName()) && matchClasses(classes, params))
                    return methods[i].invoke(null, params);
            }
        }
        catch (Exception e)
        {
            throw new JlllException("Exception in invoke " + clss.getName() + "." + methodS + "  ", e);
        }
        throw new JlllException("Now such method: " + clss.getName() + "." + methodS + "()");
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
        if (classes == null || classes.length == 0)
            return last == null || last.length == 0;
        else if (last == null || last.length != classes.length)
            return false;
        for (int i = 0; i < classes.length; i++)
        {
            if (!isInstance(classes[i], last[i]))
                return false;
        }
        return true;
    }

    /**
     * Checks if an object is assignable to a class (null-safe, handles primitives).
     *
     * @param clss
     *            the target class
     * @param object
     *            the object to check
     * @return true if object can be assigned to clss
     */
    public static boolean isInstance(Class<?> clss, Object object)
    {
        if (object == null || Null.NULL.equals(object))
            return true;
        if (clss.isAssignableFrom(object.getClass()))
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
