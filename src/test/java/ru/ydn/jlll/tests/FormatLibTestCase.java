package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;

/**
 * Tests for the FormatLib formatted output functions.
 */
public class FormatLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== format basic directives ==========

    @Test
    public void testFormatNoDirectives() throws Exception
    {
        eval("Hello World", "(format \"Hello World\")");
    }

    @Test
    public void testFormatAesthetic() throws Exception
    {
        eval("Hello World", "(format \"Hello ~a\" \"World\")");
        eval("Value: 42", "(format \"Value: ~a\" 42)");
        eval("a b c", "(format \"~a ~a ~a\" \"a\" \"b\" \"c\")");
    }

    @Test
    public void testFormatAestheticString() throws Exception
    {
        // ~a should NOT quote strings
        eval("hello", "(format \"~a\" \"hello\")");
    }

    @Test
    public void testFormatStandard() throws Exception
    {
        // ~s should quote strings
        eval("\"hello\"", "(format \"~s\" \"hello\")");
        eval("42", "(format \"~s\" 42)");
    }

    @Test
    public void testFormatDecimal() throws Exception
    {
        eval("42", "(format \"~d\" 42)");
        eval("100", "(format \"~d\" 100)");
        // Float should be truncated to integer
        eval("3", "(format \"~d\" 3.7)");
    }

    @Test
    public void testFormatFloat() throws Exception
    {
        eval("3.14", "(format \"~f\" 3.14)");
        eval("42.0", "(format \"~f\" 42)");
    }

    @Test
    public void testFormatNewline() throws Exception
    {
        eval("line1\nline2", "(format \"line1~%line2\")");
        eval("\n", "(format \"~%\")");
    }

    @Test
    public void testFormatTilde() throws Exception
    {
        eval("100~", "(format \"100~~\")");
        eval("~a", "(format \"~~a\")");
    }

    @Test
    public void testFormatCaseInsensitive() throws Exception
    {
        // Directives should be case-insensitive
        eval("hello", "(format \"~A\" \"hello\")");
        eval("\"hello\"", "(format \"~S\" \"hello\")");
        eval("42", "(format \"~D\" 42)");
        eval("3.14", "(format \"~F\" 3.14)");
    }

    @Test
    public void testFormatMixed() throws Exception
    {
        eval("Name: Alice, Age: 30", "(format \"Name: ~a, Age: ~d\" \"Alice\" 30)");
        eval("Result: 3.14\nDone", "(format \"Result: ~f~%Done\" 3.14)");
    }

    @Test
    public void testFormatList() throws Exception
    {
        // ~a on a list should display it
        eval("(1 2 3)", "(format \"~a\" '(1 2 3))");
    }

    @Test
    public void testFormatKeyword() throws Exception
    {
        eval(":foo", "(format \"~a\" :foo)");
    }

    @Test
    public void testFormatSymbol() throws Exception
    {
        eval("foo", "(format \"~a\" 'foo)");
    }

    @Test(expected = JlllException.class)
    public void testFormatNotEnoughArgs() throws Exception
    {
        Jlll.eval("(format \"~a ~a\" \"only-one\")", env);
    }
    // ========== display ==========

    @Test
    public void testDisplayString() throws Exception
    {
        // display should print without quotes (side effect)
        // We can't easily test console output, but we can verify it doesn't throw
        Object result = Jlll.eval("(display \"hello\")", env);
        // display returns null (side effect only)
        assertTrue(result == null || result.toString().equals("()"));
    }

    @Test
    public void testDisplayNumber() throws Exception
    {
        Object result = Jlll.eval("(display 42)", env);
        assertTrue(result == null || result.toString().equals("()"));
    }
    // ========== write ==========

    @Test
    public void testWriteString() throws Exception
    {
        // write should print with quotes (side effect)
        Object result = Jlll.eval("(write \"hello\")", env);
        assertTrue(result == null || result.toString().equals("()"));
    }

    @Test
    public void testWriteNumber() throws Exception
    {
        Object result = Jlll.eval("(write 42)", env);
        assertTrue(result == null || result.toString().equals("()"));
    }
    // ========== printf (JLLL wrapper) ==========

    @Test
    public void testPrintf() throws Exception
    {
        // printf should not throw
        Object result = Jlll.eval("(printf \"Value: ~d\" 42)", env);
        assertTrue(result == null || result.toString().equals("()"));
    }
    // ========== format with special characters ==========

    @Test
    public void testFormatStringWithNewlines() throws Exception
    {
        // ~s should escape newlines in strings
        String result = (String) Jlll.eval("(format \"~s\" \"line1\\nline2\")", env);
        assertTrue("Should contain escaped newline", result.contains("\\n"));
    }

    @Test
    public void testFormatStringWithQuotes() throws Exception
    {
        // ~s should escape quotes in strings
        String result = (String) Jlll.eval("(format \"~s\" \"say \\\"hello\\\"\")", env);
        assertTrue("Should contain escaped quotes", result.contains("\\\""));
    }
    // ========== format with cons structures ==========

    @Test
    public void testFormatDottedPair() throws Exception
    {
        eval("(a . b)", "(format \"~a\" (cons 'a 'b))");
    }

    @Test
    public void testFormatNestedList() throws Exception
    {
        eval("((1 2) (3 4))", "(format \"~a\" '((1 2) (3 4)))");
    }

    @Test
    public void testFormatEmptyList() throws Exception
    {
        eval("()", "(format \"~a\" '())");
    }
    // ========== Helper ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
