package ru.ydn.jlll.common;

import java.io.PrintWriter;

/**
 * Thread-local context for deep tracing of JLLL evaluation.
 *
 * <p>
 * When tracing is enabled, the Evaluator outputs trace information for all procedure calls,
 * showing entry (with arguments) and exit (with result). Indentation reflects the call depth
 * based on the environment's parent chain.
 * </p>
 *
 * <p>
 * Usage in JLLL:
 * </p>
 * <ul>
 * <li>{@code (trace)} - Enable deep tracing</li>
 * <li>{@code (untrace)} - Disable deep tracing</li>
 * <li>{@code (traced?)} - Check if tracing is enabled</li>
 * </ul>
 *
 * <p>
 * Trace output goes to the {@code *console*} from the environment, making it visible to AI
 * tools that use CapturingConsole. Falls back to System.err if no console is available.
 * </p>
 */
public class TraceContext
{
    /** Thread-local flag indicating whether tracing is enabled */
    private static final ThreadLocal<Boolean> enabled = ThreadLocal.withInitial(() -> false);

    /**
     * Checks if tracing is currently enabled for this thread.
     *
     * @return true if tracing is enabled
     */
    public static boolean isEnabled()
    {
        return enabled.get();
    }

    /**
     * Enables tracing for this thread.
     */
    public static void enable()
    {
        enabled.set(true);
    }

    /**
     * Disables tracing for this thread.
     */
    public static void disable()
    {
        enabled.set(false);
    }

    /**
     * Gets the console from the environment, falling back to a PlainConsole wrapping System.err.
     *
     * @param env
     *            the environment to look up *console*
     * @return the Console for trace output
     */
    private static Console getConsole(Environment env)
    {
        if (env != null)
        {
            Object console = env.lookup(Symbol.CONSOLE);
            if (console instanceof Console)
            {
                return (Console) console;
            }
        }
        // Fallback: PlainConsole wrapping System.err for trace visibility
        return new PlainConsole(new PrintWriter(System.err, true), null);
    }

    /**
     * Prints a trace entry message (procedure call).
     *
     * @param depth
     *            the call depth for indentation
     * @param form
     *            the expression being evaluated
     * @param env
     *            the environment (for console lookup)
     */
    public static void traceEntry(int depth, Object form, Environment env)
    {
        if (isEnabled())
        {
            Console console = getConsole(env);
            String indent = "  ".repeat(depth);
            console.println(indent + "TRACE: " + form);
            console.flush();
        }
    }

    /**
     * Prints a trace exit message (procedure return).
     *
     * @param depth
     *            the call depth for indentation
     * @param name
     *            the procedure name (or form description)
     * @param result
     *            the result value
     * @param env
     *            the environment (for console lookup)
     */
    public static void traceExit(int depth, Object name, Object result, Environment env)
    {
        if (isEnabled())
        {
            Console console = getConsole(env);
            String indent = "  ".repeat(depth);
            String resultStr = result == null ? "null" : result.toString();
            // Truncate very long results
            if (resultStr.length() > 100)
            {
                resultStr = resultStr.substring(0, 97) + "...";
            }
            console.println(indent + "TRACE: " + name + " => " + resultStr);
            console.flush();
        }
    }
}
