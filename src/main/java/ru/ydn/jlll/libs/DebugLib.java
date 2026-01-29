package ru.ydn.jlll.libs;

import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.stream.Collectors;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.PlainConsole;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.TraceContext;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Debugging and development utilities.
 *
 * <p>
 * Provides tools for debugging, testing, and functional programming:
 * </p>
 * <ul>
 * <li><b>constantly:</b> Returns a function that always returns the given value (defined in debug.jlll)</li>
 * <li><b>complement:</b> Returns the logical negation of a predicate (defined in debug.jlll)</li>
 * <li><b>identity:</b> Returns its argument unchanged (defined in debug.jlll)</li>
 * <li><b>tap:</b> Prints and returns a value for debugging pipelines (defined in debug.jlll)</li>
 * <li><b>type-of:</b> Returns the type name of a value</li>
 * <li><b>trace:</b> Enables deep tracing of all procedure calls</li>
 * <li><b>untrace:</b> Disables deep tracing</li>
 * <li><b>traced?:</b> Checks if tracing is currently enabled</li>
 * </ul>
 *
 * <p>
 * The functional utilities (constantly, complement) leverage JLLL's lexical closures feature.
 * </p>
 *
 * <p>
 * Deep tracing outputs to stderr, showing all procedure calls with indentation reflecting
 * the call depth. This is useful for debugging and understanding evaluation flow.
 * </p>
 */
public class DebugLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // Register assert as a Primitive (needs unevaluated args for lazy message evaluation)
        new Primitive("assert", env,
                "Throws JlllException if test evaluates to false/nil. "
                        + "Optional message parts are concatenated only if assertion fails. "
                        + "Examples: (assert (> x 0)) or (assert (> x 0) \"x must be positive, got: \" x)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("assert requires at least one argument (the test)");
                }
                // Evaluate the test condition
                Object test = Evaluator.eval(values.car(), env);
                boolean passed = test != null && !Boolean.FALSE.equals(test) && !(test instanceof Null);
                if (!passed)
                {
                    // Build error message from remaining args (evaluate lazily)
                    StringBuilder msg = new StringBuilder("Assertion failed");
                    Object rest = values.cdr();
                    if (rest instanceof Cons && !((Cons) rest).isNull())
                    {
                        msg.append(": ");
                        Cons msgParts = (Cons) rest;
                        while (!msgParts.isNull())
                        {
                            Object part = Evaluator.eval(msgParts.car(), env);
                            msg.append(part != null ? part.toString() : "null");
                            Object cdr = msgParts.cdr();
                            if (cdr instanceof Cons)
                            {
                                msgParts = (Cons) cdr;
                            }
                            else
                            {
                                break;
                            }
                        }
                    }
                    throw new JlllException(msg.toString());
                }
                return true;
            }
        };
        Jlll.invokeProcedure("load-system-script", env, "debug.jlll");
    }

    /**
     * Returns the type name of a value.
     * ({@code (type-of 42)}) returns "Number".
     * ({@code (type-of "hello")}) returns "String".
     *
     * @param value
     *            the value to check
     * @return the type name as a string
     */
    @JlllName("type-of")
    public String typeOf(Object value)
    {
        if (value == null || value instanceof Null)
        {
            return "Nil";
        }
        else if (value instanceof Number)
        {
            return "Number";
        }
        else if (value instanceof String)
        {
            return "String";
        }
        else if (value instanceof Boolean)
        {
            return "Boolean";
        }
        else if (value instanceof Keyword)
        {
            return "Keyword";
        }
        else if (value instanceof Symbol)
        {
            return "Symbol";
        }
        else if (value instanceof Cons)
        {
            return "List";
        }
        else if (value instanceof Macros)
        {
            return "Macro";
        }
        else if (value instanceof Primitive)
        {
            return "Primitive";
        }
        else if (value instanceof Procedure)
        {
            return "Procedure";
        }
        else
        {
            return "Java:" + value.getClass().getName();
        }
    }

    /**
     * Enables deep tracing of all procedure calls.
     * When tracing is enabled, every procedure call is logged to stderr with:
     * <ul>
     * <li>Entry: {@code TRACE: (proc args...)}</li>
     * <li>Exit: {@code TRACE: proc => result}</li>
     * </ul>
     * Indentation reflects the call depth based on the environment chain.
     *
     * @return true to indicate tracing was enabled
     */
    @JlllName("trace")
    public boolean trace()
    {
        TraceContext.enable();
        return true;
    }

    /**
     * Disables deep tracing.
     *
     * @return false to indicate tracing was disabled
     */
    @JlllName("untrace")
    public boolean untrace()
    {
        TraceContext.disable();
        return false;
    }

    /**
     * Checks if deep tracing is currently enabled.
     *
     * @return true if tracing is enabled, false otherwise
     */
    @JlllName("traced?")
    public boolean isTraced()
    {
        return TraceContext.isEnabled();
    }

    /**
     * Inspects a value in detail, printing type and structure information to the console.
     * Returns the value unchanged, so it can be used in pipelines like {@code tap}.
     *
     * <p>
     * For JLLL types: shows type, value, and structure (e.g., list length).
     * For procedures: shows description and documentation.
     * For Java objects: shows class name, fields, and method names.
     * </p>
     *
     * @param env
     *            the environment (for console lookup)
     * @param value
     *            the value to inspect
     * @return the value unchanged
     */
    @JlllName("inspect")
    public Object inspect(Environment env, Object value)
    {
        Console console = getConsole(env);
        console.println();
        console.printColored("=== Inspect ===", Console.Color.CYAN);
        console.println();
        console.print("Type: ");
        console.println(typeOf(value));
        if (value == null || value instanceof Null)
        {
            console.println("Value: null");
        }
        else if (value instanceof Cons)
        {
            Cons list = (Cons) value;
            console.print("Length: ");
            console.println(String.valueOf(list.length()));
            console.print("Value: ");
            console.println(list.toString());
        }
        else if (value instanceof Procedure)
        {
            Procedure proc = (Procedure) value;
            console.print("Description: ");
            console.println(proc.describe());
            String doc = proc.getDoc();
            if (doc != null && !doc.isEmpty())
            {
                console.print("Documentation: ");
                console.println(doc);
            }
        }
        else if (isJavaObject(value))
        {
            // Java object - show class, fields, methods
            Class<?> clazz = value.getClass();
            console.print("Java Class: ");
            console.println(clazz.getName());
            // Show public fields
            String fields = getPublicFieldNames(clazz);
            if (!fields.isEmpty())
            {
                console.print("Fields: ");
                console.println(fields);
            }
            // Show public methods (excluding Object methods)
            String methods = getPublicMethodNames(clazz);
            console.print("Methods: ");
            console.println(methods);
        }
        else
        {
            console.print("Value: ");
            console.println(value.toString());
        }
        console.println();
        console.flush();
        return value;
    }

    /**
     * Gets console from environment, falling back to PlainConsole wrapping System.out.
     */
    private Console getConsole(Environment env)
    {
        Object console = env.lookup(Symbol.CONSOLE);
        if (console instanceof Console)
        {
            return (Console) console;
        }
        return new PlainConsole(new PrintWriter(System.out, true), null);
    }

    /**
     * Checks if a value is a Java object (not a JLLL built-in type).
     */
    private boolean isJavaObject(Object value)
    {
        return !(value instanceof Cons) && !(value instanceof Symbol) && !(value instanceof Keyword)
                && !(value instanceof Procedure) && !(value instanceof Number) && !(value instanceof String)
                && !(value instanceof Boolean) && !(value instanceof Null);
    }

    /**
     * Gets public field names of a class.
     */
    private String getPublicFieldNames(Class<?> clazz)
    {
        return Arrays.stream(clazz.getFields()).filter(f -> Modifier.isPublic(f.getModifiers())).map(Field::getName)
                .collect(Collectors.joining(", "));
    }

    /**
     * Gets public method names of a class, excluding common Object methods.
     */
    private String getPublicMethodNames(Class<?> clazz)
    {
        return Arrays.stream(clazz.getMethods()).filter(m -> Modifier.isPublic(m.getModifiers()))
                .filter(m -> !isObjectMethod(m)).map(Method::getName).distinct().sorted()
                .collect(Collectors.joining(", "));
    }

    /**
     * Checks if a method is a common Object method that should be excluded from inspect output.
     */
    private boolean isObjectMethod(Method m)
    {
        String name = m.getName();
        return name.equals("getClass") || name.equals("hashCode") || name.equals("equals") || name.equals("toString")
                || name.equals("notify") || name.equals("notifyAll") || name.equals("wait");
    }
}
