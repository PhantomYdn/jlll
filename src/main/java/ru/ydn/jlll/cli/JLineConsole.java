package ru.ydn.jlll.cli;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.JlllException;

/**
 * JLine-based console implementation with ANSI color support.
 *
 * <p>
 * Uses JLine's {@link Terminal} for output and optional {@link LineReader} for
 * input with line editing, history, and completion support.
 * </p>
 */
public class JLineConsole implements Console
{
    private final Terminal terminal;
    private final LineReader lineReader;
    private final PrintWriter out;
    private final boolean colorEnabled;

    /**
     * Creates a JLine console.
     *
     * @param terminal
     *            the JLine terminal (required)
     * @param lineReader
     *            the line reader for input (may be null for output-only)
     * @param colorEnabled
     *            whether to enable ANSI colors
     */
    public JLineConsole(Terminal terminal, LineReader lineReader, boolean colorEnabled)
    {
        if (terminal == null)
        {
            throw new IllegalArgumentException("Terminal cannot be null");
        }
        this.terminal = terminal;
        this.lineReader = lineReader;
        this.out = terminal.writer();
        this.colorEnabled = colorEnabled;
    }
    // ========== Output (low-level) ==========

    @Override
    public void print(String text)
    {
        out.print(text);
    }

    @Override
    public void println(String text)
    {
        out.println(text);
    }

    @Override
    public void println()
    {
        out.println();
    }

    @Override
    public void flush()
    {
        out.flush();
    }
    // ========== Styled Output ==========

    @Override
    public void printBold(String text)
    {
        if (colorEnabled)
        {
            out.print(new AttributedString(text, AttributedStyle.BOLD).toAnsi(terminal));
        }
        else
        {
            out.print(text);
        }
    }

    @Override
    public void printFaint(String text)
    {
        if (colorEnabled)
        {
            out.print(new AttributedString(text, AttributedStyle.DEFAULT.faint()).toAnsi(terminal));
        }
        else
        {
            out.print(text);
        }
    }

    @Override
    public void printColored(String text, Color color)
    {
        if (colorEnabled)
        {
            out.print(new AttributedString(text, AttributedStyle.DEFAULT.foreground(toAnsiColor(color)))
                    .toAnsi(terminal));
        }
        else
        {
            out.print(text);
        }
    }

    /**
     * Converts Console.Color to JLine AttributedStyle color constant.
     */
    private int toAnsiColor(Color color)
    {
        switch (color)
        {
            case RED :
                return AttributedStyle.RED;
            case GREEN :
                return AttributedStyle.GREEN;
            case YELLOW :
                return AttributedStyle.YELLOW;
            case BLUE :
                return AttributedStyle.BLUE;
            case CYAN :
                return AttributedStyle.CYAN;
            case MAGENTA :
                return AttributedStyle.MAGENTA;
            case WHITE :
                return AttributedStyle.WHITE;
            default :
                return AttributedStyle.WHITE;
        }
    }
    // ========== Input ==========

    @Override
    public String readLine() throws JlllException
    {
        return readLine(null);
    }

    @Override
    public String readLine(String prompt) throws JlllException
    {
        try
        {
            if (lineReader != null)
            {
                return lineReader.readLine(prompt != null ? prompt : "");
            }
            else
            {
                // Fallback to terminal's reader
                if (prompt != null && !prompt.isEmpty())
                {
                    out.print(prompt);
                    out.flush();
                }
                BufferedReader reader = new BufferedReader(terminal.reader());
                return reader.readLine();
            }
        }
        catch (org.jline.reader.EndOfFileException e)
        {
            return null; // EOF
        }
        catch (org.jline.reader.UserInterruptException e)
        {
            throw new JlllException("User interrupt");
        }
        catch (IOException e)
        {
            throw new JlllException("I/O error reading input", e);
        }
    }

    @Override
    public String readPassword() throws JlllException
    {
        return readPassword(null);
    }

    @Override
    public String readPassword(String prompt) throws JlllException
    {
        try
        {
            if (lineReader != null)
            {
                // Use LineReader with masking character
                return lineReader.readLine(prompt != null ? prompt : "", '*');
            }
            else
            {
                // Fallback - can't mask without LineReader, but try java.io.Console
                java.io.Console systemConsole = System.console();
                if (systemConsole != null)
                {
                    char[] password = systemConsole.readPassword(prompt != null ? prompt : "");
                    return password != null ? new String(password) : null;
                }
                else
                {
                    // Last resort - read without masking
                    return readLine(prompt);
                }
            }
        }
        catch (org.jline.reader.EndOfFileException e)
        {
            return null; // EOF
        }
        catch (org.jline.reader.UserInterruptException e)
        {
            throw new JlllException("User interrupt");
        }
    }
    // ========== Capabilities ==========

    @Override
    public boolean supportsColor()
    {
        return colorEnabled;
    }

    @Override
    public boolean supportsInput()
    {
        return true;
    }

    @Override
    public int getWidth()
    {
        return terminal.getWidth();
    }
    // ========== Access to underlying streams ==========

    @Override
    public PrintWriter getWriter()
    {
        return out;
    }

    @Override
    public BufferedReader getReader()
    {
        return new BufferedReader(terminal.reader());
    }
}
