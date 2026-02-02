package ru.ydn.jlll.libs;

import java.io.IOException;
import java.io.Writer;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.Symbol;

/**
 * Formatted output primitives (SRFI-28 / Common Lisp style).
 *
 * <p>
 * Provides formatted output with directives:
 * </p>
 * <ul>
 * <li><b>format:</b> Format string with ~a, ~s, ~d, ~f, ~%, ~~ directives</li>
 * <li><b>display:</b> Write for humans (no quotes on strings)</li>
 * <li><b>write:</b> Write for machine (readable format with quotes)</li>
 * </ul>
 *
 * <p>
 * Simple wrappers printf and fprintf are implemented in format.jlll.
 * </p>
 */
public class FormatLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // format - Format string with directives
        new Primitive("format", env,
                "Formats a string using directives. (format \"Hello ~a!\" \"World\") returns \"Hello World!\". "
                        + "Directives: ~a (aesthetic), ~s (standard/quoted), ~d (decimal), ~f (float), ~% (newline), ~~ (tilde).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("format: requires at least a format string");
                }
                String formatStr = (String) values.get(0);
                // Collect remaining arguments
                Object[] args = new Object[values.length() - 1];
                for (int i = 1; i < values.length(); i++)
                {
                    args[i - 1] = values.get(i);
                }
                return formatString(formatStr, args);
            }
        };
        // display - Write for humans (no quotes on strings)
        new Primitive("display", env, "Writes a value for human reading (strings without quotes). "
                + "(display \"hello\") prints: hello. " + "(display \"hello\" port) writes to the specified port.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object value = values.get(0);
                Writer port = values.length() > 1 ? (Writer) values.get(1) : null;
                String output = displayValue(value);
                if (port != null)
                {
                    try
                    {
                        port.write(output);
                        port.flush();
                    }
                    catch (IOException e)
                    {
                        throw new JlllException("display: I/O error", e);
                    }
                }
                else
                {
                    Console console = KernelLib.getConsole(env);
                    console.print(output);
                    console.flush();
                }
                return Null.NULL;
            }
        };
        // write - Write for machine (readable format)
        new Primitive("write", env, "Writes a value in machine-readable format (strings with quotes). "
                + "(write \"hello\") prints: \"hello\". " + "(write \"hello\" port) writes to the specified port.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object value = values.get(0);
                Writer port = values.length() > 1 ? (Writer) values.get(1) : null;
                String output = writeValue(value);
                if (port != null)
                {
                    try
                    {
                        port.write(output);
                        port.flush();
                    }
                    catch (IOException e)
                    {
                        throw new JlllException("write: I/O error", e);
                    }
                }
                else
                {
                    Console console = KernelLib.getConsole(env);
                    console.print(output);
                    console.flush();
                }
                return Null.NULL;
            }
        };
        // Load JLLL wrappers
        Jlll.eval("(load-system-script \"format.jlll\")", env);
    }

    /**
     * Formats a string using SRFI-28 / Common Lisp style directives.
     *
     * @param formatStr
     *            the format string
     * @param args
     *            the arguments to substitute
     * @return the formatted string
     * @throws JlllException
     *             if format string is invalid or args mismatch
     */
    private static String formatString(String formatStr, Object[] args) throws JlllException
    {
        StringBuilder result = new StringBuilder();
        int argIndex = 0;
        int i = 0;
        while (i < formatStr.length())
        {
            char c = formatStr.charAt(i);
            if (c == '~' && i + 1 < formatStr.length())
            {
                char directive = formatStr.charAt(i + 1);
                switch (directive)
                {
                    case 'a' :
                    case 'A' :
                        // Aesthetic - human readable
                        if (argIndex >= args.length)
                        {
                            throw new JlllException("format: not enough arguments for ~a");
                        }
                        result.append(displayValue(args[argIndex++]));
                        i += 2;
                        break;
                    case 's' :
                    case 'S' :
                        // Standard - machine readable
                        if (argIndex >= args.length)
                        {
                            throw new JlllException("format: not enough arguments for ~s");
                        }
                        result.append(writeValue(args[argIndex++]));
                        i += 2;
                        break;
                    case 'd' :
                    case 'D' :
                        // Decimal integer
                        if (argIndex >= args.length)
                        {
                            throw new JlllException("format: not enough arguments for ~d");
                        }
                        Object dArg = args[argIndex++];
                        if (dArg instanceof Number)
                        {
                            result.append(((Number) dArg).longValue());
                        }
                        else
                        {
                            result.append(dArg);
                        }
                        i += 2;
                        break;
                    case 'f' :
                    case 'F' :
                        // Floating point
                        if (argIndex >= args.length)
                        {
                            throw new JlllException("format: not enough arguments for ~f");
                        }
                        Object fArg = args[argIndex++];
                        if (fArg instanceof Number)
                        {
                            result.append(((Number) fArg).doubleValue());
                        }
                        else
                        {
                            result.append(fArg);
                        }
                        i += 2;
                        break;
                    case '%' :
                        // Newline
                        result.append('\n');
                        i += 2;
                        break;
                    case '~' :
                        // Literal tilde
                        result.append('~');
                        i += 2;
                        break;
                    default :
                        // Unknown directive - just output as-is
                        result.append(c);
                        i++;
                        break;
                }
            }
            else
            {
                result.append(c);
                i++;
            }
        }
        return result.toString();
    }

    /**
     * Converts a value to display format (human readable, no quotes on strings).
     *
     * @param value
     *            the value to convert
     * @return the display string
     */
    private static String displayValue(Object value)
    {
        if (value == null || Null.NULL.equals(value))
        {
            return "()";
        }
        if (value instanceof String)
        {
            return (String) value; // No quotes
        }
        if (value instanceof Cons)
        {
            return consToDisplayString((Cons) value);
        }
        if (value instanceof Symbol)
        {
            return ((Symbol) value).getName();
        }
        if (value instanceof Keyword)
        {
            return ":" + ((Keyword) value).getName();
        }
        return String.valueOf(value);
    }

    /**
     * Converts a value to write format (machine readable, with quotes on strings).
     *
     * @param value
     *            the value to convert
     * @return the write string
     */
    private static String writeValue(Object value)
    {
        if (value == null || Null.NULL.equals(value))
        {
            return "()";
        }
        if (value instanceof String)
        {
            // Escape special characters and quote
            return "\"" + escapeString((String) value) + "\"";
        }
        if (value instanceof Cons)
        {
            return consToWriteString((Cons) value);
        }
        if (value instanceof Symbol)
        {
            return ((Symbol) value).getName();
        }
        if (value instanceof Keyword)
        {
            return ":" + ((Keyword) value).getName();
        }
        if (value instanceof Boolean)
        {
            return (Boolean) value ? "true" : "false";
        }
        return String.valueOf(value);
    }

    /**
     * Escapes special characters in a string for write format.
     */
    private static String escapeString(String s)
    {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++)
        {
            char c = s.charAt(i);
            switch (c)
            {
                case '"' :
                    sb.append("\\\"");
                    break;
                case '\\' :
                    sb.append("\\\\");
                    break;
                case '\n' :
                    sb.append("\\n");
                    break;
                case '\r' :
                    sb.append("\\r");
                    break;
                case '\t' :
                    sb.append("\\t");
                    break;
                default :
                    sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * Converts a Cons list to display format (elements without string quotes).
     */
    private static String consToDisplayString(Cons cons)
    {
        if (cons.isNull())
        {
            return "()";
        }
        StringBuilder sb = new StringBuilder("(");
        boolean first = true;
        Cons current = cons;
        while (current != null && !current.isNull())
        {
            if (!first)
            {
                sb.append(" ");
            }
            first = false;
            sb.append(displayValue(current.car()));
            Object cdr = current.cdr();
            if (cdr == null || Null.NULL.equals(cdr))
            {
                break;
            }
            else if (cdr instanceof Cons)
            {
                current = (Cons) cdr;
                if (current.isNull())
                {
                    break;
                }
            }
            else
            {
                // Dotted pair
                sb.append(" . ");
                sb.append(displayValue(cdr));
                break;
            }
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * Converts a Cons list to write format (elements with string quotes).
     */
    private static String consToWriteString(Cons cons)
    {
        if (cons.isNull())
        {
            return "()";
        }
        StringBuilder sb = new StringBuilder("(");
        boolean first = true;
        Cons current = cons;
        while (current != null && !current.isNull())
        {
            if (!first)
            {
                sb.append(" ");
            }
            first = false;
            sb.append(writeValue(current.car()));
            Object cdr = current.cdr();
            if (cdr == null || Null.NULL.equals(cdr))
            {
                break;
            }
            else if (cdr instanceof Cons)
            {
                current = (Cons) cdr;
                if (current.isNull())
                {
                    break;
                }
            }
            else
            {
                // Dotted pair
                sb.append(" . ");
                sb.append(writeValue(cdr));
                break;
            }
        }
        sb.append(")");
        return sb.toString();
    }
}
