package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.CommonUtil;

/**
 * Created by IntelliJ IDEA.
 * User: Eleas
 * Date: 18.05.2003
 * Time: 18:26:18
 * To change this template use Options | File Templates.
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
