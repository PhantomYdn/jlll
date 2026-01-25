package ru.ydn.jlll.cli;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.jline.reader.EndOfFileException;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.libs.KernelLib;

/**
 * REPL-specific library providing terminal-aware versions of introspection
 * commands and REPL control functions.
 *
 * <p>
 * This library is loaded only by the REPL and overrides the plain-text
 * versions of {@code env} and {@code doc} from KernelLib with colored,
 * formatted versions. It also provides REPL-specific commands:
 * </p>
 * <ul>
 * <li><b>env:</b> Prints environment bindings with colors and grouping</li>
 * <li><b>doc:</b> Prints documentation with formatting</li>
 * <li><b>help:</b> Shows REPL keyboard shortcuts and commands</li>
 * <li><b>quit/exit:</b> Exits the REPL</li>
 * </ul>
 *
 * <p>
 * Requires these bindings to be set before loading:
 * </p>
 * <ul>
 * <li>{@code *terminal*} - JLine Terminal object</li>
 * <li>{@code *color-enabled*} - Boolean for color output</li>
 * </ul>
 */
public class ReplLib implements Library
{
    private static final Symbol TERMINAL = Symbol.intern("*terminal*");
    private static final Symbol COLOR_ENABLED = Symbol.intern("*color-enabled*");

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        // Override env with colored version
        new Primitive("env", env,
                "Prints environment bindings with colors and grouping. (env) prints all bindings. "
                        + "(env \"prefix\") filters by name prefix. "
                        + "(env :primitives), (env :macros), (env :procedures), (env :variables) " + "filters by type.")
        {
            private static final long serialVersionUID = 8273645091827364521L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                PrintWriter out = KernelLib.getStdout(env);
                Terminal terminal = getTerminal(env);
                boolean colorEnabled = isColorEnabled(env);
                Map<Symbol, Object> bindings = env.getAllBindings();
                String prefixFilter = null;
                String typeFilter = null;
                // Parse arguments
                if (values.length() > 0)
                {
                    Object arg = values.get(0);
                    if (arg instanceof String)
                    {
                        prefixFilter = (String) arg;
                    }
                    else if (arg instanceof Keyword)
                    {
                        typeFilter = ((Keyword) arg).getName();
                    }
                    else if (arg instanceof Symbol)
                    {
                        typeFilter = ((Symbol) arg).getName();
                    }
                    else
                    {
                        throw new JlllException(
                                "env argument must be a string prefix or type keyword (:primitives, :macros, :procedures, :variables)");
                    }
                }
                // Collect bindings by type
                List<Map.Entry<Symbol, Object>> primitives = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> macros = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> procedures = new ArrayList<>();
                List<Map.Entry<Symbol, Object>> variables = new ArrayList<>();
                for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
                {
                    String name = entry.getKey().getName();
                    if (prefixFilter != null && !name.contains(prefixFilter))
                    {
                        continue;
                    }
                    Object value = entry.getValue();
                    if (value instanceof Primitive)
                    {
                        primitives.add(entry);
                    }
                    else if (value instanceof Macros)
                    {
                        macros.add(entry);
                    }
                    else if (value instanceof Procedure)
                    {
                        procedures.add(entry);
                    }
                    else
                    {
                        variables.add(entry);
                    }
                }
                // Print header
                out.println();
                int total = primitives.size() + macros.size() + procedures.size() + variables.size();
                String header;
                if (prefixFilter != null)
                {
                    header = "Bindings matching \"" + prefixFilter + "\" (" + total + "):";
                }
                else if (typeFilter != null)
                {
                    header = "Environment bindings (" + typeFilter + "):";
                }
                else
                {
                    header = "Environment bindings (" + bindings.size() + " total):";
                }
                printColored(out, terminal, colorEnabled, header, AttributedStyle.BOLD);
                out.println();
                // Print groups based on type filter
                if (typeFilter == null || typeFilter.equals("primitives"))
                {
                    printGroup(out, terminal, colorEnabled, env, "Primitives", primitives, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("macros"))
                {
                    printGroup(out, terminal, colorEnabled, env, "Macros", macros, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("procedures"))
                {
                    printGroup(out, terminal, colorEnabled, env, "Procedures", procedures, typeFilter != null);
                }
                if (typeFilter == null || typeFilter.equals("variables"))
                {
                    printGroup(out, terminal, colorEnabled, env, "Variables", variables, typeFilter != null);
                }
                if (prefixFilter == null && typeFilter == null)
                {
                    printColored(out, terminal, colorEnabled, "Use (env \"prefix\") to filter by name.",
                            AttributedStyle.DEFAULT.faint());
                }
                out.println();
                out.flush();
                return Null.NULL;
            }

            private void printGroup(PrintWriter out, Terminal terminal, boolean colorEnabled, Environment env,
                    String groupName, List<Map.Entry<Symbol, Object>> entries, boolean skipHeader)
            {
                if (entries.isEmpty())
                {
                    return;
                }
                if (!skipHeader)
                {
                    if (colorEnabled && terminal != null)
                    {
                        out.println(new AttributedString(groupName + " (" + entries.size() + "):",
                                AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN)).toAnsi(terminal));
                    }
                    else
                    {
                        out.println(groupName + " (" + entries.size() + "):");
                    }
                }
                // Sort alphabetically
                entries.sort((a, b) -> a.getKey().getName().compareTo(b.getKey().getName()));
                for (Map.Entry<Symbol, Object> entry : entries)
                {
                    printBinding(out, terminal, colorEnabled, env, entry.getKey(), entry.getValue());
                }
                out.println();
            }

            private void printBinding(PrintWriter out, Terminal terminal, boolean colorEnabled, Environment env,
                    Symbol sym, Object value)
            {
                String name = sym.getName();
                String doc = getDocExcerpt(env, sym, value);
                if (colorEnabled && terminal != null)
                {
                    AttributedStringBuilder asb = new AttributedStringBuilder();
                    asb.append("  ");
                    asb.styled(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW),
                            String.format("%-16s", name));
                    asb.append(" ");
                    asb.append(doc);
                    out.println(asb.toAnsi(terminal));
                }
                else
                {
                    out.println(String.format("  %-16s %s", name, doc));
                }
            }

            private String getDocExcerpt(Environment env, Symbol sym, Object value)
            {
                Object doc = env.getMeta(sym, Symbol.intern("doc"));
                if (doc != null && !doc.toString().isEmpty())
                {
                    String text = doc.toString();
                    int newline = text.indexOf('\n');
                    if (newline > 0)
                    {
                        return text.substring(0, newline);
                    }
                    return text;
                }
                if (value instanceof Primitive)
                {
                    return "primitive";
                }
                else if (value instanceof Macros)
                {
                    return "macro";
                }
                else if (value instanceof Procedure)
                {
                    return "procedure";
                }
                else if (value instanceof Boolean)
                {
                    return "Boolean: " + value;
                }
                else if (value instanceof Number)
                {
                    return value.getClass().getSimpleName() + ": " + value;
                }
                else if (value instanceof String)
                {
                    String s = (String) value;
                    if (s.length() > 20)
                    {
                        return "String: \"" + s.substring(0, 17) + "...\"";
                    }
                    return "String: \"" + s + "\"";
                }
                else if (value == null || value instanceof Null)
                {
                    return "nil";
                }
                else
                {
                    return value.getClass().getSimpleName();
                }
            }
        };
        // Override doc with colored version
        new Primitive("doc", env, "Prints documentation for a symbol with colors and formatting. "
                + "Returns the documentation string or nil if not available.")
        {
            private static final long serialVersionUID = 8273645091827364522L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() != 1)
                    throw new JlllException("doc requires exactly one argument");
                PrintWriter out = KernelLib.getStdout(env);
                Terminal terminal = getTerminal(env);
                boolean colorEnabled = isColorEnabled(env);
                Object arg = values.get(0);
                Symbol sym;
                String symName;
                if (arg instanceof Symbol)
                {
                    sym = (Symbol) arg;
                    symName = sym.getName();
                }
                else if (arg instanceof Procedure)
                {
                    String doc = ((Procedure) arg).getDoc();
                    if (doc != null && !doc.isEmpty())
                    {
                        out.println(doc);
                        out.flush();
                    }
                    return (doc != null && !doc.isEmpty()) ? doc : null;
                }
                else
                {
                    throw new JlllException("doc requires a symbol or procedure");
                }
                Object value = env.lookup(sym);
                if (value == null)
                {
                    printColored(out, terminal, colorEnabled, "Symbol '" + symName + "' is not bound.",
                            AttributedStyle.DEFAULT.foreground(AttributedStyle.RED));
                    out.println();
                    out.flush();
                    return null;
                }
                // Get documentation
                Object docObj = env.getMeta(sym, Symbol.intern("doc"));
                String docString = null;
                if (docObj != null)
                {
                    docString = docObj.toString();
                }
                else if (value instanceof Procedure)
                {
                    String procDoc = ((Procedure) value).getDoc();
                    if (procDoc != null && !procDoc.isEmpty())
                    {
                        docString = procDoc;
                    }
                }
                // Print formatted output
                out.println();
                if (colorEnabled && terminal != null)
                {
                    out.println(new AttributedString(symName, AttributedStyle.BOLD.foreground(AttributedStyle.CYAN))
                            .toAnsi(terminal));
                    out.println(new AttributedString("─".repeat(50), AttributedStyle.DEFAULT.faint()).toAnsi(terminal));
                }
                else
                {
                    out.println(symName);
                    out.println("──────────────────────────────────────────────────");
                }
                if (docString != null)
                {
                    out.println(docString);
                }
                else
                {
                    printColored(out, terminal, colorEnabled, "No documentation available.",
                            AttributedStyle.DEFAULT.faint());
                }
                // Print Java source info if available
                Object javaClass = env.getMeta(sym, Symbol.intern("java-class"));
                Object javaMethod = env.getMeta(sym, Symbol.intern("java-method"));
                if (javaClass != null)
                {
                    out.println();
                    String javaInfo = "Java: " + javaClass;
                    if (javaMethod != null)
                    {
                        javaInfo += "." + javaMethod;
                    }
                    printColored(out, terminal, colorEnabled, javaInfo, AttributedStyle.DEFAULT.faint());
                }
                // Print type info
                out.println();
                String typeInfo = "Type: " + KernelLib.getTypeName(value);
                printColored(out, terminal, colorEnabled, typeInfo, AttributedStyle.DEFAULT.faint());
                out.println();
                out.flush();
                return docString;
            }
        };
        // REPL-specific: help command
        new Primitive("help", env, "Shows REPL help message with available commands and keyboard shortcuts.")
        {
            private static final long serialVersionUID = 8273645091827364523L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                PrintWriter out = KernelLib.getStdout(env);
                Terminal terminal = getTerminal(env);
                boolean colorEnabled = isColorEnabled(env);
                out.println();
                printColored(out, terminal, colorEnabled, "REPL Commands:", AttributedStyle.BOLD);
                out.println("  (help)            Show this help message");
                out.println("  (env)             Show all environment bindings");
                out.println("  (env \"prefix\")    Show bindings matching prefix");
                out.println("  (env :primitives) Show only primitives");
                out.println("  (env :macros)     Show only macros");
                out.println("  (env :procedures) Show only procedures");
                out.println("  (env :variables)  Show only variables");
                out.println("  (doc 'symbol)     Show documentation for symbol");
                out.println("  (quit)            Exit the REPL");
                out.println("  (exit)            Exit the REPL");
                out.println();
                printColored(out, terminal, colorEnabled, "Keyboard Shortcuts:", AttributedStyle.BOLD);
                out.println("  Ctrl+D            Exit the REPL");
                out.println("  Ctrl+C            Cancel current input");
                out.println("  Ctrl+R            Search command history");
                out.println("  Tab               Auto-complete symbols");
                out.println("  Up/Down           Navigate command history");
                out.println();
                out.flush();
                return Null.NULL;
            }
        };
        // REPL-specific: quit command
        new Primitive("quit", env, "Exits the REPL.")
        {
            private static final long serialVersionUID = 8273645091827364524L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                throw new JlllException(new EndOfFileException("quit"));
            }
        };
        // REPL-specific: exit command (alias for quit)
        new Primitive("exit", env, "Exits the REPL (alias for quit).")
        {
            private static final long serialVersionUID = 8273645091827364525L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                throw new JlllException(new EndOfFileException("exit"));
            }
        };
    }

    /**
     * Gets the JLine Terminal from the environment, if available.
     */
    private static Terminal getTerminal(Environment env)
    {
        Object terminal = env.lookup(TERMINAL);
        if (terminal instanceof Terminal)
        {
            return (Terminal) terminal;
        }
        return null;
    }

    /**
     * Checks if color output is enabled.
     */
    private static boolean isColorEnabled(Environment env)
    {
        Object colorEnabled = env.lookup(COLOR_ENABLED);
        if (colorEnabled instanceof Boolean)
        {
            return (Boolean) colorEnabled;
        }
        return false;
    }

    /**
     * Helper method for colored output with fallback.
     */
    private static void printColored(PrintWriter out, Terminal terminal, boolean colorEnabled, String text,
            AttributedStyle style)
    {
        if (colorEnabled && terminal != null)
        {
            out.println(new AttributedString(text, style).toAnsi(terminal));
        }
        else
        {
            out.println(text);
        }
    }
}
