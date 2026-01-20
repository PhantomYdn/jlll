package ru.ydn.jlll.common;

/**
 * Interface for loadable JLLL libraries.
 * Libraries register primitives, macros, and bindings when loaded into an environment.
 *
 * <p>
 * Implement this interface to create custom libraries that can be loaded with
 * {@code (load-lib "com.example.MyLib")}.
 * </p>
 */
public interface Library
{
    /**
     * Loads this library into the given environment.
     * Implementations should register all primitives and bindings here.
     *
     * @param env
     *            the environment to load into
     * @throws JlllException
     *             if loading fails
     */
    public void load(Enviroment env) throws JlllException;
}
