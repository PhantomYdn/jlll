package ru.ydn.jlll.cli;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.jline.reader.Buffer;
import org.jline.reader.EndOfFileException;
import org.jline.reader.History;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.Reference;
import org.jline.reader.UserInterruptException;
import org.jline.reader.Widget;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;

/**
 * Interactive REPL (Read-Eval-Print Loop) for JLLL using JLine 3.
 * Provides readline-like editing, command history, and tab completion.
 */
public class JlllRepl
{
    private static final String HISTORY_FILE = ".jlll_history";
    private static final String PRIMARY_PROMPT = "jlll> ";
    private static final String CONTINUATION_PROMPT = "....> ";
    private final Enviroment env;
    private boolean colorEnabled = true;
    private boolean quiet = false;
    private Terminal terminal;
    private LineReader reader;
    private PrintWriter out;

    /**
     * Creates a REPL for the given environment.
     *
     * @param env
     *            the environment for evaluating expressions
     */
    public JlllRepl(Enviroment env)
    {
        this.env = env;
    }

    /**
     * Returns the current environment for this REPL session.
     * Used by highlighter and completer for context-aware features.
     *
     * @return the current environment
     */
    public Enviroment getEnvironment()
    {
        return env;
    }

    /**
     * Enables or disables ANSI color output.
     *
     * @param enabled
     *            true to enable colors
     */
    public void setColorEnabled(boolean enabled)
    {
        this.colorEnabled = enabled;
    }

    /**
     * Enables or disables the startup banner.
     *
     * @param quiet
     *            true to suppress the banner
     */
    public void setQuiet(boolean quiet)
    {
        this.quiet = quiet;
    }

    /**
     * Runs the REPL loop.
     *
     * @return exit code (0 for normal exit)
     */
    public int run()
    {
        try
        {
            initTerminal();
            printBanner();
            replLoop();
            return 0;
        }
        catch (IOException e)
        {
            System.err.println("Error initializing terminal: " + e.getMessage());
            return 1;
        }
        finally
        {
            cleanup();
        }
    }

    private void initTerminal() throws IOException
    {
        terminal = TerminalBuilder.builder().system(true).build();
        out = terminal.writer();
        Path historyPath = Paths.get(System.getProperty("user.home"), HISTORY_FILE);
        History history = new DefaultHistory();
        reader = LineReaderBuilder.builder().terminal(terminal).history(history)
                .variable(LineReader.HISTORY_FILE, historyPath).parser(new JlllParser())
                .completer(new JlllCompleter(env)).highlighter(colorEnabled ? new JlllHighlighter(this) : null)
                .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true).build();
        // Bind ) to a widget that removes trailing space before inserting
        // This makes "(+ xvar )" become "(+ xvar)" after tab completion
        Widget closeParenWidget = () ->
        {
            Buffer buf = reader.getBuffer();
            if (buf.cursor() > 0 && buf.atChar(buf.cursor() - 1) == ' ')
            {
                buf.backspace();
            }
            buf.write(')');
            return true;
        };
        reader.getWidgets().put("close-paren", closeParenWidget);
        reader.getKeyMaps().get(LineReader.MAIN).bind(new Reference("close-paren"), ")");
    }

    private void printBanner()
    {
        if (quiet)
        {
            return;
        }
        if (colorEnabled)
        {
            out.println(new AttributedString("JLLL - Java Lisp Like Language",
                    AttributedStyle.BOLD.foreground(AttributedStyle.CYAN)).toAnsi(terminal));
            out.println(new AttributedString("Type (help) for commands, Ctrl+D to exit",
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT)).toAnsi(terminal));
        }
        else
        {
            out.println("JLLL - Java Lisp Like Language");
            out.println("Type (help) for commands, Ctrl+D to exit");
        }
        out.println();
        out.flush();
    }

    private void replLoop()
    {
        StringBuilder buffer = new StringBuilder();
        while (true)
        {
            String prompt = buffer.length() == 0 ? PRIMARY_PROMPT : CONTINUATION_PROMPT;
            try
            {
                String line = reader.readLine(prompt);
                if (line == null)
                {
                    // EOF (Ctrl+D)
                    break;
                }
                buffer.append(line).append("\n");
                // Check if expression is complete (balanced parentheses)
                if (isExpressionComplete(buffer.toString()))
                {
                    String code = buffer.toString().trim();
                    buffer.setLength(0);
                    if (code.isEmpty())
                    {
                        continue;
                    }
                    // Handle special REPL commands
                    if (handleReplCommand(code))
                    {
                        continue;
                    }
                    // Evaluate the expression
                    evaluate(code);
                }
            }
            catch (UserInterruptException e)
            {
                // Ctrl+C - clear current input
                buffer.setLength(0);
                out.println();
                out.flush();
            }
            catch (EndOfFileException e)
            {
                // Ctrl+D
                break;
            }
        }
        out.println();
        out.println("Goodbye!");
        out.flush();
    }

    private boolean isExpressionComplete(String code)
    {
        int parenCount = 0;
        int bracketCount = 0;
        boolean inString = false;
        boolean escape = false;
        for (char c : code.toCharArray())
        {
            if (escape)
            {
                escape = false;
                continue;
            }
            if (c == '\\')
            {
                escape = true;
                continue;
            }
            if (c == '"')
            {
                inString = !inString;
                continue;
            }
            if (inString)
            {
                continue;
            }
            switch (c)
            {
                case '(' :
                    parenCount++;
                    break;
                case ')' :
                    parenCount--;
                    break;
                case '[' :
                    bracketCount++;
                    break;
                case ']' :
                    bracketCount--;
                    break;
            }
        }
        // Expression is complete if all parens/brackets are balanced
        // and we're not in the middle of a string
        return parenCount <= 0 && bracketCount <= 0 && !inString;
    }

    private boolean handleReplCommand(String code)
    {
        String trimmed = code.trim();
        // Handle quit/exit commands
        if (trimmed.equals("(quit)") || trimmed.equals("(exit)"))
        {
            throw new EndOfFileException();
        }
        // Handle help command
        if (trimmed.equals("(help)"))
        {
            printHelp();
            return true;
        }
        // Handle env command - show current environment bindings
        if (trimmed.equals("(env)"))
        {
            printEnvironment(null);
            return true;
        }
        // Handle env with filter: (env "prefix") or (env 'prefix) or (env prefix)
        if (trimmed.startsWith("(env ") && trimmed.endsWith(")"))
        {
            String filter = extractArgument(trimmed.substring(5, trimmed.length() - 1));
            printEnvironment(filter);
            return true;
        }
        // Handle doc command: (doc 'symbol) or (doc symbol)
        if (trimmed.startsWith("(doc ") && trimmed.endsWith(")"))
        {
            String symbolName = extractArgument(trimmed.substring(5, trimmed.length() - 1));
            if (symbolName != null && !symbolName.isEmpty())
            {
                printDocumentation(symbolName);
                return true;
            }
        }
        return false;
    }

    /**
     * Extracts an argument from REPL command, handling quotes and quote marks.
     * Examples: "foo" -> foo, 'foo -> foo, foo -> foo
     */
    private String extractArgument(String arg)
    {
        String inner = arg.trim();
        if (inner.startsWith("\"") && inner.endsWith("\"") && inner.length() > 1)
        {
            return inner.substring(1, inner.length() - 1);
        }
        if (inner.startsWith("'"))
        {
            return inner.substring(1);
        }
        return inner;
    }

    private void printHelp()
    {
        out.println();
        printColored("REPL Commands:", AttributedStyle.BOLD);
        out.println("  (help)            Show this help message");
        out.println("  (env)             Show all environment bindings");
        out.println("  (env \"prefix\")    Show bindings matching prefix");
        out.println("  (doc 'symbol)     Show documentation for symbol");
        out.println("  (quit)            Exit the REPL");
        out.println("  (exit)            Exit the REPL");
        out.println();
        printColored("Keyboard Shortcuts:", AttributedStyle.BOLD);
        out.println("  Ctrl+D            Exit the REPL");
        out.println("  Ctrl+C            Cancel current input");
        out.println("  Ctrl+R            Search command history");
        out.println("  Tab               Auto-complete symbols");
        out.println("  Up/Down           Navigate command history");
        out.println();
        out.flush();
    }

    /**
     * Prints environment bindings, optionally filtered by name.
     *
     * @param filter
     *            if not null, only show bindings containing this string
     */
    private void printEnvironment(String filter)
    {
        Map<Symbol, Object> bindings = env.getAllBindings();
        // Group bindings by type
        Map<String, List<Map.Entry<Symbol, Object>>> groups = new LinkedHashMap<>();
        groups.put("Primitives", new ArrayList<>());
        groups.put("Procedures", new ArrayList<>());
        groups.put("Macros", new ArrayList<>());
        groups.put("Variables", new ArrayList<>());
        int total = 0;
        for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
        {
            String name = entry.getKey().getName();
            // Apply filter if specified
            if (filter != null && !name.contains(filter))
            {
                continue;
            }
            total++;
            Object value = entry.getValue();
            if (value instanceof Primitive)
            {
                groups.get("Primitives").add(entry);
            }
            else if (value instanceof Macros)
            {
                groups.get("Macros").add(entry);
            }
            else if (value instanceof Procedure)
            {
                groups.get("Procedures").add(entry);
            }
            else
            {
                groups.get("Variables").add(entry);
            }
        }
        out.println();
        if (filter != null)
        {
            printColored("Bindings matching \"" + filter + "\" (" + total + "):", AttributedStyle.BOLD);
        }
        else
        {
            printColored("Environment bindings (" + bindings.size() + " total):", AttributedStyle.BOLD);
        }
        out.println();
        // Print each non-empty group
        for (Map.Entry<String, List<Map.Entry<Symbol, Object>>> group : groups.entrySet())
        {
            List<Map.Entry<Symbol, Object>> entries = group.getValue();
            if (!entries.isEmpty())
            {
                printGroupHeader(group.getKey(), entries.size());
                // Sort entries alphabetically
                entries.sort((a, b) -> a.getKey().getName().compareTo(b.getKey().getName()));
                for (Map.Entry<Symbol, Object> entry : entries)
                {
                    printBinding(entry.getKey(), entry.getValue());
                }
                out.println();
            }
        }
        if (filter == null)
        {
            printColored("Use (env \"prefix\") to filter by name.", AttributedStyle.DEFAULT.faint());
        }
        out.println();
        out.flush();
    }

    /**
     * Prints a group header for environment listing.
     */
    private void printGroupHeader(String name, int count)
    {
        if (colorEnabled)
        {
            out.println(new AttributedString(name + " (" + count + "):",
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN)).toAnsi(terminal));
        }
        else
        {
            out.println(name + " (" + count + "):");
        }
    }

    /**
     * Prints a single binding with its documentation excerpt.
     */
    private void printBinding(Symbol sym, Object value)
    {
        String name = sym.getName();
        String doc = getDocExcerpt(sym, value, 50);
        if (colorEnabled)
        {
            AttributedStringBuilder asb = new AttributedStringBuilder();
            asb.append("  ");
            asb.styled(AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW), String.format("%-16s", name));
            asb.append(" ");
            asb.append(doc);
            out.println(asb.toAnsi(terminal));
        }
        else
        {
            out.println(String.format("  %-16s %s", name, doc));
        }
    }

    /**
     * Gets a documentation excerpt for display in listings.
     * Returns the first line of documentation if available.
     */
    private String getDocExcerpt(Symbol sym, Object value, int maxLen)
    {
        // Try metadata doc first - show full first line
        Object doc = env.getMeta(sym, Symbol.intern("doc"));
        if (doc != null && !doc.toString().isEmpty())
        {
            return getFirstLine(doc.toString());
        }
        // Fall back to type + value info
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

    /**
     * Truncates documentation to fit in a given length.
     * Prefers ending at sentence boundary.
     */
    private String truncateDoc(String doc, int maxLen)
    {
        if (doc == null || doc.isEmpty())
        {
            return "";
        }
        // Try to end at first sentence
        int period = doc.indexOf('.');
        if (period > 0 && period < maxLen)
        {
            return doc.substring(0, period + 1);
        }
        // Otherwise truncate at word boundary
        if (doc.length() <= maxLen)
        {
            return doc;
        }
        int end = doc.lastIndexOf(' ', maxLen - 3);
        if (end < maxLen / 2)
        {
            end = maxLen - 3;
        }
        return doc.substring(0, end) + "...";
    }

    /**
     * Extracts the first line of text (up to newline or end of string).
     */
    private String getFirstLine(String text)
    {
        if (text == null || text.isEmpty())
        {
            return "";
        }
        int newline = text.indexOf('\n');
        if (newline > 0)
        {
            return text.substring(0, newline);
        }
        return text;
    }

    /**
     * Prints full documentation for a symbol with nice formatting.
     */
    private void printDocumentation(String symbolName)
    {
        Symbol sym = Symbol.intern(symbolName);
        Object value = env.lookup(sym);
        if (value == null)
        {
            printColored("Symbol '" + symbolName + "' is not bound.",
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.RED));
            out.println();
            out.flush();
            return;
        }
        out.println();
        // Print symbol name as header
        if (colorEnabled)
        {
            out.println(new AttributedString(symbolName, AttributedStyle.BOLD.foreground(AttributedStyle.CYAN))
                    .toAnsi(terminal));
            out.println(new AttributedString("─".repeat(50), AttributedStyle.DEFAULT.faint()).toAnsi(terminal));
        }
        else
        {
            out.println(symbolName);
            out.println("─".repeat(50));
        }
        // Print documentation
        Object doc = env.getMeta(sym, Symbol.intern("doc"));
        if (doc != null && !doc.toString().isEmpty())
        {
            out.println(doc.toString());
        }
        else if (value instanceof Procedure)
        {
            // Try Procedure.getDoc() as fallback
            String procDoc = ((Procedure) value).getDoc();
            if (procDoc != null && !procDoc.isEmpty())
            {
                out.println(procDoc);
            }
            else
            {
                printColored("No documentation available.", AttributedStyle.DEFAULT.faint());
            }
        }
        else
        {
            printColored("No documentation available.", AttributedStyle.DEFAULT.faint());
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
            printColored(javaInfo, AttributedStyle.DEFAULT.faint());
        }
        // Print type info
        out.println();
        String typeInfo = "Type: ";
        if (value instanceof Primitive)
        {
            typeInfo += "primitive";
        }
        else if (value instanceof Macros)
        {
            typeInfo += "macro";
        }
        else if (value instanceof Procedure)
        {
            typeInfo += "procedure";
        }
        else
        {
            typeInfo += value.getClass().getSimpleName();
        }
        printColored(typeInfo, AttributedStyle.DEFAULT.faint());
        out.println();
        out.flush();
    }

    /**
     * Helper method for colored output.
     */
    private void printColored(String text, AttributedStyle style)
    {
        if (colorEnabled)
        {
            out.println(new AttributedString(text, style).toAnsi(terminal));
        }
        else
        {
            out.println(text);
        }
    }

    private void evaluate(String code)
    {
        try
        {
            Object result = Jlll.eval(new StringReader(code), env);
            printResult(result);
        }
        catch (JlllException e)
        {
            printError(e);
        }
        catch (Exception e)
        {
            printError(e);
        }
    }

    private void printResult(Object result)
    {
        if (result == null || result instanceof Null)
        {
            if (colorEnabled)
            {
                out.println(new AttributedString("nil", AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT))
                        .toAnsi(terminal));
            }
            else
            {
                out.println("nil");
            }
        }
        else
        {
            if (colorEnabled)
            {
                out.println(new AttributedString(result.toString(),
                        AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN)).toAnsi(terminal));
            }
            else
            {
                out.println(result);
            }
        }
        out.flush();
    }

    private void printError(Exception e)
    {
        String message = e.getMessage();
        if (message == null)
        {
            message = e.getClass().getSimpleName();
        }
        if (colorEnabled)
        {
            out.println(
                    new AttributedString("Error: " + message, AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
                            .toAnsi(terminal));
        }
        else
        {
            out.println("Error: " + message);
        }
        if (e.getCause() != null)
        {
            String causeMsg = e.getCause().getMessage();
            if (causeMsg != null)
            {
                if (colorEnabled)
                {
                    out.println(new AttributedString("  Caused by: " + causeMsg,
                            AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)).toAnsi(terminal));
                }
                else
                {
                    out.println("  Caused by: " + causeMsg);
                }
            }
        }
        out.flush();
    }

    private void cleanup()
    {
        if (terminal != null)
        {
            try
            {
                terminal.close();
            }
            catch (IOException e)
            {
                // Ignore cleanup errors
            }
        }
    }
}
