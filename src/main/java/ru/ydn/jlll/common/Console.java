package ru.ydn.jlll.common;

import java.io.BufferedReader;
import java.io.PrintWriter;

/**
 * Abstract console interface for styled I/O operations.
 *
 * <p>
 * Provides a unified abstraction for terminal interaction that works across different
 * environments (plain text CLI, REPL with colors, etc.). All primitives that need
 * styled output should use this interface via {@code KernelLib.getConsole(env)}.
 * </p>
 *
 * <p>
 * Implementations:
 * </p>
 * <ul>
 * <li>{@code PlainConsole} - Plain text output, no styling</li>
 * <li>{@code JLineConsole} - ANSI colors via JLine terminal</li>
 * </ul>
 *
 * <p>
 * The console is bound to {@code *console*} in the environment.
 * </p>
 */
public interface Console
{
    // ========== Output (low-level) ==========
    /**
     * Prints text without newline.
     *
     * @param text
     *            the text to print
     */
    void print(String text);

    /**
     * Prints text followed by newline.
     *
     * @param text
     *            the text to print
     */
    void println(String text);

    /**
     * Prints a newline.
     */
    void println();

    /**
     * Flushes output buffer.
     */
    void flush();
    // ========== Styled Output (low-level) ==========

    /**
     * Prints text in bold style.
     *
     * @param text
     *            the text to print
     */
    void printBold(String text);

    /**
     * Prints text in faint/dim style.
     *
     * @param text
     *            the text to print
     */
    void printFaint(String text);

    /**
     * Prints text in specified color.
     *
     * @param text
     *            the text to print
     * @param color
     *            the color to use
     */
    void printColored(String text, Color color);
    // ========== Output (semantic - default implementations) ==========

    /**
     * Prints a section header (bold, with newline).
     *
     * @param text
     *            the header text
     */
    default void printHeader(String text)
    {
        printBold(text);
        println();
    }

    /**
     * Prints an error message (red).
     *
     * @param text
     *            the error text
     */
    default void printError(String text)
    {
        printColored(text, Color.RED);
    }

    /**
     * Prints a warning message (yellow).
     *
     * @param text
     *            the warning text
     */
    default void printWarning(String text)
    {
        printColored(text, Color.YELLOW);
    }

    /**
     * Prints a success message (green).
     *
     * @param text
     *            the success text
     */
    default void printSuccess(String text)
    {
        printColored(text, Color.GREEN);
    }

    /**
     * Prints a hint or secondary information (faint).
     *
     * @param text
     *            the hint text
     */
    default void printHint(String text)
    {
        printFaint(text);
    }

    /**
     * Prints a name/identifier (yellow, typically for symbols).
     *
     * @param text
     *            the name to print
     */
    default void printName(String text)
    {
        printColored(text, Color.YELLOW);
    }

    /**
     * Prints a keyword (cyan).
     *
     * @param text
     *            the keyword to print
     */
    default void printKeyword(String text)
    {
        printColored(text, Color.CYAN);
    }
    // ========== Input ==========

    /**
     * Reads a line of input.
     *
     * @return the line read (without newline), or null at EOF
     * @throws JlllException
     *             if input is not available or I/O error occurs
     */
    String readLine() throws JlllException;

    /**
     * Reads a line of input with a prompt.
     *
     * @param prompt
     *            the prompt to display
     * @return the line read (without newline), or null at EOF
     * @throws JlllException
     *             if input is not available or I/O error occurs
     */
    String readLine(String prompt) throws JlllException;

    /**
     * Reads a password (input masked).
     *
     * @return the password read
     * @throws JlllException
     *             if input is not available or I/O error occurs
     */
    String readPassword() throws JlllException;

    /**
     * Reads a password with a prompt (input masked).
     *
     * @param prompt
     *            the prompt to display
     * @return the password read
     * @throws JlllException
     *             if input is not available or I/O error occurs
     */
    String readPassword(String prompt) throws JlllException;
    // ========== Capabilities ==========

    /**
     * Returns whether this console supports colored output.
     *
     * @return true if colors are supported and enabled
     */
    boolean supportsColor();

    /**
     * Returns whether this console supports input operations.
     *
     * @return true if input is available
     */
    boolean supportsInput();

    /**
     * Returns the terminal width in characters.
     *
     * @return the width, or -1 if unknown
     */
    int getWidth();
    // ========== Access to underlying streams ==========

    /**
     * Returns the underlying PrintWriter for advanced output operations.
     *
     * @return the writer
     */
    PrintWriter getWriter();

    /**
     * Returns the underlying BufferedReader for advanced input operations.
     *
     * @return the reader, or null if input is not available
     */
    BufferedReader getReader();

    /**
     * Colors for styled output.
     */
    enum Color
    {
        RED, GREEN, YELLOW, BLUE, CYAN, MAGENTA, WHITE
    }
}
