package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.CommonUtil;

/**
 * Java reflection primitives for interoperability.
 *
 * <p>
 * Provides Java interop operations:
 * </p>
 * <ul>
 * <li><b>new:</b> creates Java objects - {@code (new "java.util.ArrayList")}</li>
 * <li><b>invoke:</b> calls instance methods - {@code (invoke list "add" item)}</li>
 * <li><b>invoke-static:</b> calls static methods - {@code (invoke-static "Math" "sqrt" 2)}</li>
 * <li><b>peek/poke:</b> gets/sets instance fields</li>
 * <li><b>peek-static/poke-static:</b> gets/sets static fields</li>
 * <li><b>instanceof?:</b> type checking - {@code (instanceof? obj "java.util.List")}</li>
 * <li><b>class:</b> converts string to Class object</li>
 * </ul>
 */
public class ReflectLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        Jlll.invokeProcedure("load-system-script", env, "reflect.jlll");
    }

    /**
     * Creates a new Java object. {@code (new "java.util.ArrayList")} or {@code (new "java.util.ArrayList" 10)}.
     *
     * @param clazz
     *            class name string or Class object
     * @param params
     *            constructor arguments
     * @return the new instance
     * @throws JlllException
     *             if instantiation fails
     */
    @JlllName("new")
    public Object createObject(Object clazz, Object... params) throws JlllException
    {
        return CommonUtil.constactObject(toClass(clazz), params);
    }

    /**
     * Invokes an instance method. {@code (invoke obj "methodName" arg1 arg2)}.
     *
     * @param obj
     *            the target object
     * @param method
     *            the method name
     * @param params
     *            method arguments
     * @return the method result
     * @throws JlllException
     *             if invocation fails
     */
    @JlllName("invoke")
    public Object invokeMethod(Object obj, String method, Object... params) throws JlllException
    {
        return CommonUtil.invoke(obj, method, params);
    }

    /**
     * Invokes a static method. {@code (invoke-static "Math" "sqrt" 2.0)}.
     *
     * @param clazz
     *            class name string or Class object
     * @param method
     *            the static method name
     * @param params
     *            method arguments
     * @return the method result
     * @throws JlllException
     *             if invocation fails
     */
    @JlllName("invoke-static")
    public Object invokeStatic(Object clazz, String method, Object... params) throws JlllException
    {
        return CommonUtil.invokeStatic(toClass(clazz), method, params);
    }

    /**
     * Gets an instance field value. {@code (peek obj "fieldName")}.
     *
     * @param obj
     *            the target object
     * @param field
     *            the field name
     * @return the field value
     * @throws JlllException
     *             if field access fails
     */
    @JlllName("peek")
    public Object peek(Object obj, String field) throws JlllException
    {
        return CommonUtil.peek(obj, field);
    }

    /**
     * Sets an instance field value. {@code (poke obj "fieldName" value)}.
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
    @JlllName("poke")
    public Object poke(Object object, String field, Object value) throws JlllException
    {
        return CommonUtil.poke(object, field, value);
    }

    /**
     * Gets a static field value. {@code (peek-static "Integer" "MAX_VALUE")}.
     *
     * @param clazz
     *            class name string or Class object
     * @param field
     *            the static field name
     * @return the field value
     * @throws JlllException
     *             if field access fails
     */
    @JlllName("peek-static")
    public Object peekStatic(Object clazz, String field) throws JlllException
    {
        return CommonUtil.peekStatic(toClass(clazz), field);
    }

    /**
     * Sets a static field value. {@code (poke-static "MyClass" "counter" 0)}.
     *
     * @param clazz
     *            class name string or Class object
     * @param field
     *            the static field name
     * @param value
     *            the value to set
     * @return the value that was set
     * @throws JlllException
     *             if field access fails
     */
    @JlllName("poke-static")
    public Object pokeStatic(Object clazz, String field, Object value) throws JlllException
    {
        return CommonUtil.pokeStatic(toClass(clazz), field, value);
    }

    /**
     * Tests if an object is an instance of a class. {@code (instanceof? obj "java.util.List")}.
     *
     * @param obj
     *            the object to test
     * @param clazz
     *            class name string or Class object
     * @return true if obj is an instance of clazz
     * @throws JlllException
     *             if class not found
     */
    @JlllName("instanceof?")
    public boolean isInstanceOf(Object obj, Object clazz) throws JlllException
    {
        return toClass(clazz).isInstance(obj);
    }

    /**
     * Converts a class name to a Class object. {@code (class "java.util.ArrayList")}.
     *
     * @param clazz
     *            class name string or existing Class object
     * @return the Class object
     * @throws JlllException
     *             if class not found
     */
    @JlllName("class")
    public Class<?> toClass(Object clazz) throws JlllException
    {
        try
        {
            return clazz instanceof Class ? (Class<?>) clazz : Class.forName(clazz.toString());
        }
        catch (ClassNotFoundException e)
        {
            throw new JlllException(clazz + " not found", e);
        }
    }
}
