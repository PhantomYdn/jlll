package ru.ydn.jlll.cli;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import org.jline.utils.AttributedStyle;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;

/**
 * Interactive REPL (Read-Eval-Print Loop) for JLLL using JLine 3.
 * Provides readline-like editing, command history, and tab completion.
 */
public class JlllRepl
{
    private static final String HISTORY_FILE = ".jlll_history";
    private static final String PRIMARY_PROMPT = "jlll> ";
    private static final String CONTINUATION_PROMPT = "....> ";
    private final Environment env;
    private boolean colorEnabled = true;
    private boolean quiet = false;
    private Terminal terminal;
    private LineReader reader;
    private PrintWriter out;
    private StringBuilder currentBuffer;

    /**
     * Creates a REPL for the given environment.
     *
     * @param env
     *            the environment for evaluating expressions
     */
    public JlllRepl(Environment env)
    {
        this.env = env;
    }

    /**
     * Returns the current environment for this REPL session.
     * Used by highlighter and completer for context-aware features.
     *
     * @return the current environment
     */
    public Environment getEnvironment()
    {
        return env;
    }

    /**
     * Returns the current accumulated input buffer.
     * Used by highlighter for context-aware bracket matching across lines.
     *
     * @return the current input buffer contents, or empty string if none
     */
    public String getBuffer()
    {
        return currentBuffer != null ? currentBuffer.toString() : "";
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
        // Set up REPL-specific bindings and load ReplLib
        env.addBinding("*terminal*", terminal);
        env.addBinding("*color-enabled*", colorEnabled);
        try
        {
            new ReplLib().load(env);
        }
        catch (JlllException e)
        {
            System.err.println("Warning: Failed to load ReplLib: " + e.getMessage());
        }
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
        currentBuffer = new StringBuilder();
        while (true)
        {
            String prompt = currentBuffer.length() == 0
                    ? PRIMARY_PROMPT
                    : CONTINUATION_PROMPT + calculateIndentation(currentBuffer.toString());
            try
            {
                String line = reader.readLine(prompt);
                if (line == null)
                {
                    // EOF (Ctrl+D)
                    break;
                }
                currentBuffer.append(line).append("\n");
                // Check if expression is complete (balanced parentheses)
                if (isExpressionComplete(currentBuffer.toString()))
                {
                    String code = currentBuffer.toString().trim();
                    currentBuffer.setLength(0);
                    if (code.isEmpty())
                    {
                        continue;
                    }
                    // Evaluate the expression - quit/exit are handled as procedures
                    if (!evaluate(code))
                    {
                        // evaluate returns false when quit/exit was called
                        break;
                    }
                }
            }
            catch (UserInterruptException e)
            {
                // Ctrl+C - clear current input
                currentBuffer.setLength(0);
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

    /**
     * Calculates indentation spaces based on unclosed brackets in the buffer.
     * Uses 2 spaces per nesting level.
     *
     * @param code
     *            the accumulated code buffer
     * @return indentation string (spaces)
     */
    private String calculateIndentation(String code)
    {
        int depth = countUnclosedBrackets(code);
        if (depth <= 0)
        {
            return "";
        }
        return " ".repeat(depth * 2);
    }

    /**
     * Counts the number of unclosed brackets (parentheses and square brackets) in the code.
     * Properly handles strings and escape sequences.
     *
     * @param code
     *            the code to analyze
     * @return the number of unclosed brackets (can be negative if over-closed)
     */
    int countUnclosedBrackets(String code)
    {
        int depth = 0;
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
                case '[' :
                    depth++;
                    break;
                case ')' :
                case ']' :
                    depth--;
                    break;
            }
        }
        return depth;
    }

    /**
     * Evaluates code and prints the result.
     *
     * @param code
     *            the code to evaluate
     * @return true to continue REPL, false to exit (quit/exit was called)
     */
    private boolean evaluate(String code)
    {
        try
        {
            Object result = Jlll.eval(new StringReader(code), env);
            printResult(result);
            return true;
        }
        catch (JlllException e)
        {
            // Check if this is quit/exit signaling via EndOfFileException
            if (e.getCause() instanceof EndOfFileException)
            {
                return false;
            }
            printError(e);
            return true;
        }
        catch (Exception e)
        {
            printError(e);
            return true;
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
