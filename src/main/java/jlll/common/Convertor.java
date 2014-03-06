package jlll.common;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: 06.09.2007
 * Time: 14:08:07
 * To change this template use File | Settings | File Templates.
 */
public interface Convertor
{
    public Object convert(Object value, Class<?> requiredClass, Enviroment env);
}
