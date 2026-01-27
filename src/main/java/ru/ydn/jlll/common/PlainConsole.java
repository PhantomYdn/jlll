package ru.ydn.jlll.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * Plain text console implementation without styling.
 *
 * <p>
 * All styled output methods (printBold, printColored, etc.) delegate to plain
 * {@link #print(String)}. Used for non-interactive CLI execution and testing.
 * </p>
 */
public class PlainConsole implements Console
{
    private final PrintWriter out;
    private final BufferedReader in;

    /**
     * Creates a plain console with specified output and optional input.
     *
     * @param out
     *            the output writer (required)
     * @param in
     *            the input reader (may be null if no input available)
     */
    public PlainConsole(PrintWriter out, BufferedReader in)
    {
        if (out == null)
        {
            throw new IllegalArgumentException("Output writer cannot be null");
        }
        this.out = out;
        this.in = in;
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
    // ========== Styled Output (no styling - delegate to plain) ==========

    @Override
    public void printBold(String text)
    {
        print(text);
    }

    @Override
    public void printFaint(String text)
    {
        print(text);
    }

    @Override
    public void printColored(String text, Color color)
    {
        print(text);
    }
    // ========== Input ==========

    @Override
    public String readLine() throws JlllException
    {
        if (in == null)
        {
            throw new JlllException("No input available");
        }
        try
        {
            return in.readLine();
        }
        catch (IOException e)
        {
            throw new JlllException("I/O error reading input", e);
        }
    }

    @Override
    public String readLine(String prompt) throws JlllException
    {
        if (prompt != null && !prompt.isEmpty())
        {
            print(prompt);
            flush();
        }
        return readLine();
    }

    @Override
    public String readPassword() throws JlllException
    {
        // Plain console cannot mask input, just read normally
        return readLine();
    }

    @Override
    public String readPassword(String prompt) throws JlllException
    {
        if (prompt != null && !prompt.isEmpty())
        {
            print(prompt);
            flush();
        }
        return readPassword();
    }
    // ========== Capabilities ==========

    @Override
    public boolean supportsColor()
    {
        return false;
    }

    @Override
    public boolean supportsInput()
    {
        return in != null;
    }

    @Override
    public int getWidth()
    {
        return -1; // Unknown
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
        return in;
    }
}
