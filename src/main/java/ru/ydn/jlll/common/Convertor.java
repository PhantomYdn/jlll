package ru.ydn.jlll.common;

/**
 * Functional interface for type conversion when calling Java methods from JLLL.
 * Implementations convert JLLL values to the required Java types.
 */
public interface Convertor
{
    /**
     * Converts a JLLL value to the required Java type.
     *
     * @param value
     *            the JLLL value to convert
     * @param requiredClass
     *            the target Java class
     * @param env
     *            the current environment (for context)
     * @return the converted value
     */
    public Object convert(Object value, Class<?> requiredClass, Environment env);
}
