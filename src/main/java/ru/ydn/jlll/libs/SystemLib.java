package ru.ydn.jlll.libs;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.LinkedHashMap;
import java.util.Map;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * System and environment primitives.
 *
 * <p>
 * Provides access to environment variables, system properties, and system information.
 * </p>
 * <ul>
 * <li><b>Environment:</b> getenv, getenv-all</li>
 * <li><b>Properties:</b> get-property, set-property!</li>
 * <li><b>System Info:</b> hostname</li>
 * <li><b>Memory:</b> gc, memory-used, memory-free, memory-total, memory-max</li>
 * <li><b>CPU:</b> available-processors</li>
 * </ul>
 *
 * <p>
 * Simple wrappers like user-name, user-home, os-name, os-arch, java-version are
 * implemented in system.jlll using get-property.
 * </p>
 */
public class SystemLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // getenv with optional default
        new Primitive("getenv", env, "Gets environment variable value. (getenv \"HOME\") returns the value or null. "
                + "(getenv \"MISSING\" \"default\") returns \"default\" if not set.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String name = (String) values.get(0);
                String value = System.getenv(name);
                if (value == null && values.length() > 1)
                {
                    return values.get(1);
                }
                return value;
            }
        };
        // get-property with optional default
        new Primitive("get-property", env,
                "Gets Java system property. (get-property \"java.version\") returns the value or null. "
                        + "(get-property \"missing\" \"default\") returns \"default\" if not set.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String name = (String) values.get(0);
                String value = System.getProperty(name);
                if (value == null && values.length() > 1)
                {
                    Object defaultValue = values.get(1);
                    return defaultValue instanceof String ? defaultValue : defaultValue.toString();
                }
                return value;
            }
        };
        // Load JLLL wrappers
        Jlll.eval("(load-system-script \"system.jlll\")", env);
    }
    // ========== Simple methods using @JlllName ==========

    /**
     * Gets all environment variables as a hash map.
     * ({@code (getenv-all)}) returns a map of all environment variables.
     *
     * @return a LinkedHashMap of all environment variables
     */
    @JlllName("getenv-all")
    public Map<Object, Object> getenvAll()
    {
        Map<Object, Object> result = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : System.getenv().entrySet())
        {
            result.put(entry.getKey(), entry.getValue());
        }
        return result;
    }

    /**
     * Sets a Java system property.
     * ({@code (set-property! "my.prop" "value")}) sets the property and returns the previous value.
     *
     * @param name
     *            the property name
     * @param value
     *            the property value
     * @return the previous value, or null if none
     */
    @JlllName("set-property!")
    public String setProperty(String name, String value)
    {
        return System.setProperty(name, value);
    }

    /**
     * Gets the machine hostname.
     * ({@code (hostname)}) returns the local host name.
     *
     * @return the hostname, or "unknown" if it cannot be determined
     */
    @JlllName("hostname")
    public String hostname()
    {
        try
        {
            return InetAddress.getLocalHost().getHostName();
        }
        catch (UnknownHostException e)
        {
            return "unknown";
        }
    }

    /**
     * Triggers garbage collection.
     * ({@code (gc)}) requests the JVM to run garbage collection and returns null.
     *
     * @return null
     */
    @JlllName("gc")
    public Object gc()
    {
        System.gc();
        return null;
    }

    /**
     * Gets the amount of used memory in bytes.
     * ({@code (memory-used)}) returns total minus free memory.
     *
     * @return used memory in bytes
     */
    @JlllName("memory-used")
    public Long memoryUsed()
    {
        Runtime rt = Runtime.getRuntime();
        return rt.totalMemory() - rt.freeMemory();
    }

    /**
     * Gets the amount of free memory in bytes.
     * ({@code (memory-free)}) returns the free memory available to the JVM.
     *
     * @return free memory in bytes
     */
    @JlllName("memory-free")
    public Long memoryFree()
    {
        return Runtime.getRuntime().freeMemory();
    }

    /**
     * Gets the total heap size in bytes.
     * ({@code (memory-total)}) returns the total memory currently available to the JVM.
     *
     * @return total memory in bytes
     */
    @JlllName("memory-total")
    public Long memoryTotal()
    {
        return Runtime.getRuntime().totalMemory();
    }

    /**
     * Gets the maximum heap size in bytes.
     * ({@code (memory-max)}) returns the maximum memory the JVM will attempt to use.
     *
     * @return maximum memory in bytes
     */
    @JlllName("memory-max")
    public Long memoryMax()
    {
        return Runtime.getRuntime().maxMemory();
    }

    /**
     * Gets the number of available processors.
     * ({@code (available-processors)}) returns the number of processors available to the JVM.
     *
     * @return the number of processors
     */
    @JlllName("available-processors")
    public Integer availableProcessors()
    {
        return Runtime.getRuntime().availableProcessors();
    }
}
