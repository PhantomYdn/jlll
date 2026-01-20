package ru.ydn.jlll.cli;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.jline.reader.EndOfFileException;
import org.jline.reader.History;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;

import ru.ydn.jlll.common.Enviroment;
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

    private final Enviroment env;
    private boolean colorEnabled = true;
    private boolean quiet = false;

    private Terminal terminal;
    private LineReader reader;
    private PrintWriter out;

    public JlllRepl(Enviroment env)
    {
        this.env = env;
    }

    public void setColorEnabled(boolean enabled)
    {
        this.colorEnabled = enabled;
    }

    public void setQuiet(boolean quiet)
    {
        this.quiet = quiet;
    }

    /**
     * Runs the REPL loop.
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
        terminal = TerminalBuilder.builder()
            .system(true)
            .build();

        out = terminal.writer();

        Path historyPath = Paths.get(System.getProperty("user.home"), HISTORY_FILE);
        History history = new DefaultHistory();

        reader = LineReaderBuilder.builder()
            .terminal(terminal)
            .history(history)
            .variable(LineReader.HISTORY_FILE, historyPath)
            .completer(new JlllCompleter(env))
            .highlighter(colorEnabled ? new JlllHighlighter() : null)
            .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
            .build();
    }

    private void printBanner()
    {
        if (quiet)
        {
            return;
        }

        if (colorEnabled)
        {
            out.println(new AttributedString(
                "JLLL - Java Lisp Like Language",
                AttributedStyle.BOLD.foreground(AttributedStyle.CYAN)
            ).toAnsi(terminal));
            out.println(new AttributedString(
                "Type (help) for commands, Ctrl+D to exit",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT)
            ).toAnsi(terminal));
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
                case '(':
                    parenCount++;
                    break;
                case ')':
                    parenCount--;
                    break;
                case '[':
                    bracketCount++;
                    break;
                case ']':
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
            printEnvironment();
            return true;
        }

        return false;
    }

    private void printHelp()
    {
        out.println();
        out.println("REPL Commands:");
        out.println("  (help)    - Show this help message");
        out.println("  (env)     - Show current environment bindings");
        out.println("  (quit)    - Exit the REPL");
        out.println("  (exit)    - Exit the REPL");
        out.println();
        out.println("Keyboard Shortcuts:");
        out.println("  Ctrl+D    - Exit the REPL");
        out.println("  Ctrl+C    - Cancel current input");
        out.println("  Ctrl+R    - Search command history");
        out.println("  Tab       - Auto-complete symbols");
        out.println("  Up/Down   - Navigate command history");
        out.println();
        out.flush();
    }

    private void printEnvironment()
    {
        out.println();
        out.println("Environment bindings:");
        env.getAllBindings().forEach((symbol, value) ->
        {
            String valueStr = value == null ? "nil" : value.getClass().getSimpleName();
            out.println("  " + symbol.getName() + " : " + valueStr);
        });
        out.println();
        out.flush();
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
                out.println(new AttributedString(
                    "nil",
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT)
                ).toAnsi(terminal));
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
                out.println(new AttributedString(
                    result.toString(),
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN)
                ).toAnsi(terminal));
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
            out.println(new AttributedString(
                "Error: " + message,
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ).toAnsi(terminal));
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
                    out.println(new AttributedString(
                        "  Caused by: " + causeMsg,
                        AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
                    ).toAnsi(terminal));
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
