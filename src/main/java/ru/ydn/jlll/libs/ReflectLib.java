package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Enviroment;
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
    public void load(Enviroment env) throws JlllException
    {
        super.load(env);
        Jlll.invokeProcedure("load-system-script", env, "reflect.jlll");
    }

    @JlllName("new")
    public Object createObject(Object clazz, Object... params) throws JlllException
    {
        return CommonUtil.constactObject(toClass(clazz), params);
    }

    @JlllName("invoke")
    public Object invokeMethod(Object obj, String method, Object... params) throws JlllException
    {
        return CommonUtil.invoke(obj, method, params);
    }

    @JlllName("invoke-static")
    public Object invokeStatic(Object clazz, String method, Object... params) throws JlllException
    {
        return CommonUtil.invokeStatic(toClass(clazz), method, params);
    }

    @JlllName("peek")
    public Object peek(Object obj, String field) throws JlllException
    {
        return CommonUtil.peek(obj, field);
    }

    @JlllName("poke")
    public Object poke(Object object, String field, Object value) throws JlllException
    {
        return CommonUtil.poke(object, field, value);
    }

    @JlllName("peek-static")
    public Object peekStatic(Object clazz, String field) throws JlllException
    {
        return CommonUtil.peekStatic(toClass(clazz), field);
    }

    @JlllName("poke-static")
    public Object pokeStatic(Object clazz, String field, Object value) throws JlllException
    {
        return CommonUtil.pokeStatic(toClass(clazz), field, value);
    }

    @JlllName("instanceof?")
    public boolean isInstanceOf(Object obj, Object clazz) throws JlllException
    {
        return toClass(clazz).isInstance(obj);
    }

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
