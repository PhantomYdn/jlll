package ru.ydn.jlll.common;

import ru.ydn.jlll.common.annotation.JlllDoc;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: May 14, 2003
 * Time: 6:24:38 PM
 * To change this template use Options | File Templates.
 */
public class Primitive extends Procedure
{
    private static final long serialVersionUID = -9190619708410061311L;
    protected final String originalName;

    public Primitive(String name, Enviroment env)
    {
        env.addBinding(Symbol.intern(name), this);
        originalName = name;
    }

    public String describe()
    {
        return "JLLL primitive with original name: " + originalName + "\n" + "Doc: " + getDoc();
    }

    @Override
    public String getDoc()
    {
        Class<?> clazz = this.getClass();
        JlllDoc doc = clazz.getAnnotation(JlllDoc.class);
        return doc == null ? "" : doc.value();
    }
}
