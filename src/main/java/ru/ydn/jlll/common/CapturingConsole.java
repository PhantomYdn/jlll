package ru.ydn.jlll.common;

import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * A console implementation that captures all output to a buffer.
 *
 * <p>
 * This is useful for capturing output from code execution, particularly for AI tool
 * interactions where the AI needs to "see" what was printed. Optionally, output can
 * be echoed to a parent console for user visibility (traceability).
 * </p>
 *
 * <p>
 * Input operations delegate to a parent console if available, or throw an exception.
 * </p>
 */
public class CapturingConsole implements Console
{
    private final StringBuilder buffer = new StringBuilder();
    private final Console parent;
    private boolean echo;

    /**
     * Creates a capturing console with no parent (output only captured, not displayed).
     */
    public CapturingConsole()
    {
        this(null, false);
    }

    /**
     * Creates a capturing console with optional parent for echo.
     *
     * @param parent
     *            the parent console to echo output to (may be null)
     * @param echo
     *            whether to echo output to the parent console
     */
    public CapturingConsole(Console parent, boolean echo)
    {
        this.parent = parent;
        this.echo = echo;
    }

    /**
     * Returns the captured output.
     *
     * @return all output captured since creation or last clear
     */
    public String getCapturedOutput()
    {
        return buffer.toString();
    }

    /**
     * Clears the captured output buffer.
     */
    public void clear()
    {
        buffer.setLength(0);
    }

    /**
     * Returns whether echoing is enabled.
     *
     * @return true if output is echoed to parent
     */
    public boolean isEcho()
    {
        return echo;
    }

    /**
     * Sets whether to echo output to the parent console.
     *
     * @param echo
     *            true to enable echoing
     */
    public void setEcho(boolean echo)
    {
        this.echo = echo;
    }

    /**
     * Returns the parent console.
     *
     * @return the parent console, or null if none
     */
    public Console getParent()
    {
        return parent;
    }
    // ========== Output (low-level) ==========

    @Override
    public void print(String text)
    {
        buffer.append(text);
        if (echo && parent != null)
        {
            parent.print(text);
        }
    }

    @Override
    public void println(String text)
    {
        buffer.append(text).append("\n");
        if (echo && parent != null)
        {
            parent.println(text);
        }
    }

    @Override
    public void println()
    {
        buffer.append("\n");
        if (echo && parent != null)
        {
            parent.println();
        }
    }

    @Override
    public void flush()
    {
        if (echo && parent != null)
        {
            parent.flush();
        }
    }
    // ========== Styled Output (capture plain text, delegate styling to parent) ==========

    @Override
    public void printBold(String text)
    {
        buffer.append(text);
        if (echo && parent != null)
        {
            parent.printBold(text);
        }
    }

    @Override
    public void printFaint(String text)
    {
        buffer.append(text);
        if (echo && parent != null)
        {
            parent.printFaint(text);
        }
    }

    @Override
    public void printColored(String text, Color color)
    {
        buffer.append(text);
        if (echo && parent != null)
        {
            parent.printColored(text, color);
        }
    }
    // ========== Input (delegate to parent or throw) ==========

    @Override
    public String readLine() throws JlllException
    {
        if (parent != null && parent.supportsInput())
        {
            return parent.readLine();
        }
        throw new JlllException("Input not available in capturing console");
    }

    @Override
    public String readLine(String prompt) throws JlllException
    {
        if (parent != null && parent.supportsInput())
        {
            return parent.readLine(prompt);
        }
        throw new JlllException("Input not available in capturing console");
    }

    @Override
    public String readPassword() throws JlllException
    {
        if (parent != null && parent.supportsInput())
        {
            return parent.readPassword();
        }
        throw new JlllException("Input not available in capturing console");
    }

    @Override
    public String readPassword(String prompt) throws JlllException
    {
        if (parent != null && parent.supportsInput())
        {
            return parent.readPassword(prompt);
        }
        throw new JlllException("Input not available in capturing console");
    }
    // ========== Capabilities ==========

    @Override
    public boolean supportsColor()
    {
        return parent != null && parent.supportsColor();
    }

    @Override
    public boolean supportsInput()
    {
        return parent != null && parent.supportsInput();
    }

    @Override
    public int getWidth()
    {
        return parent != null ? parent.getWidth() : -1;
    }
    // ========== Access to underlying streams ==========

    @Override
    public PrintWriter getWriter()
    {
        // Return a writer that captures to buffer
        return new PrintWriter(new StringWriter()
        {
            @Override
            public void write(String str)
            {
                buffer.append(str);
                if (echo && parent != null)
                {
                    parent.getWriter().write(str);
                }
            }

            @Override
            public void flush()
            {
                if (echo && parent != null)
                {
                    parent.getWriter().flush();
                }
            }
        });
    }

    @Override
    public BufferedReader getReader()
    {
        return parent != null ? parent.getReader() : null;
    }
}
