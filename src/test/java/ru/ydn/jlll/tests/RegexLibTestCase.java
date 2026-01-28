package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;

/**
 * Tests for the RegexLib regular expression functions.
 */
public class RegexLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== regex-match ==========

    @Test
    public void testRegexMatchSimple() throws Exception
    {
        eval("abc", "(regex-match \"abc\" \"xyzabcdef\")");
    }

    @Test
    public void testRegexMatchWithDigits() throws Exception
    {
        eval("123", "(regex-match \"\\\\d+\" \"abc123def\")");
    }

    @Test
    public void testRegexMatchNoMatch() throws Exception
    {
        eval(false, "(regex-match \"xyz\" \"abcdef\")");
    }

    @Test
    public void testRegexMatchWithGroups() throws Exception
    {
        Object result = Jlll.eval("(regex-match \"(\\\\d+)-(\\\\d+)\" \"abc123-456def\")", env);
        assertTrue("Should return a list with groups", result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals("123-456", list.get(0)); // full match
        assertEquals("123", list.get(1)); // group 1
        assertEquals("456", list.get(2)); // group 2
    }

    @Test
    public void testRegexMatchSingleGroup() throws Exception
    {
        Object result = Jlll.eval("(regex-match \"a(\\\\d+)b\" \"xa123by\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(2, list.length());
        assertEquals("a123b", list.get(0));
        assertEquals("123", list.get(1));
    }
    // ========== regex-match-all ==========

    @Test
    public void testRegexMatchAllSimple() throws Exception
    {
        Object result = Jlll.eval("(regex-match-all \"\\\\d+\" \"a1b23c456\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals("1", list.get(0));
        assertEquals("23", list.get(1));
        assertEquals("456", list.get(2));
    }

    @Test
    public void testRegexMatchAllNoMatch() throws Exception
    {
        Object result = Jlll.eval("(regex-match-all \"\\\\d+\" \"abcdef\")", env);
        assertTrue(result instanceof Cons);
        assertTrue("Should return empty list", ((Cons) result).isNull());
    }

    @Test
    public void testRegexMatchAllWithGroups() throws Exception
    {
        Object result = Jlll.eval("(regex-match-all \"(\\\\w)(\\\\d)\" \"a1b2c3\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        // Each element is a list: (full-match group1 group2)
        Cons first = (Cons) list.get(0);
        assertEquals("a1", first.get(0));
        assertEquals("a", first.get(1));
        assertEquals("1", first.get(2));
    }
    // ========== regex-replace ==========

    @Test
    public void testRegexReplaceSimple() throws Exception
    {
        eval("aXbXcX", "(regex-replace \"\\\\d+\" \"a1b23c456\" \"X\")");
    }

    @Test
    public void testRegexReplaceNoMatch() throws Exception
    {
        eval("abcdef", "(regex-replace \"\\\\d+\" \"abcdef\" \"X\")");
    }

    @Test
    public void testRegexReplaceWithBackreference() throws Exception
    {
        eval("a[1]b[23]c[456]", "(regex-replace \"\\\\d+\" \"a1b23c456\" \"[$0]\")");
    }

    @Test
    public void testRegexReplaceWithFunction() throws Exception
    {
        // Double each number
        eval("a2b46c912",
                "(regex-replace \"\\\\d+\" \"a1b23c456\" (lambda (m) (number->string (* 2 (string->number m)))))");
    }

    @Test
    public void testRegexReplaceWithFunctionUppercase() throws Exception
    {
        eval("HELLO WORLD", "(regex-replace \"[a-z]+\" \"hello world\" (lambda (m) (string-upcase m)))");
    }
    // ========== regex-replace-first ==========

    @Test
    public void testRegexReplaceFirst() throws Exception
    {
        eval("aXb23c456", "(regex-replace-first \"\\\\d+\" \"a1b23c456\" \"X\")");
    }

    @Test
    public void testRegexReplaceFirstNoMatch() throws Exception
    {
        eval("abcdef", "(regex-replace-first \"\\\\d+\" \"abcdef\" \"X\")");
    }
    // ========== regex-split ==========

    @Test
    public void testRegexSplitSimple() throws Exception
    {
        Object result = Jlll.eval("(regex-split \"\\\\s+\" \"a b  c\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals("a", list.get(0));
        assertEquals("b", list.get(1));
        assertEquals("c", list.get(2));
    }

    @Test
    public void testRegexSplitPreservesEmpty() throws Exception
    {
        Object result = Jlll.eval("(regex-split \",\" \"a,,b,\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(4, list.length());
        assertEquals("a", list.get(0));
        assertEquals("", list.get(1));
        assertEquals("b", list.get(2));
        assertEquals("", list.get(3));
    }

    @Test
    public void testRegexSplitNoMatch() throws Exception
    {
        Object result = Jlll.eval("(regex-split \",\" \"abcdef\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(1, list.length());
        assertEquals("abcdef", list.get(0));
    }

    @Test
    public void testRegexSplitByDigits() throws Exception
    {
        Object result = Jlll.eval("(regex-split \"\\\\d+\" \"a1b23c\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals("a", list.get(0));
        assertEquals("b", list.get(1));
        assertEquals("c", list.get(2));
    }
    // ========== regex-matches? ==========

    @Test
    public void testRegexMatchesTrue() throws Exception
    {
        eval(true, "(regex-matches? \"\\\\d+\" \"12345\")");
    }

    @Test
    public void testRegexMatchesFalse() throws Exception
    {
        eval(false, "(regex-matches? \"\\\\d+\" \"123abc\")");
    }

    @Test
    public void testRegexMatchesFullString() throws Exception
    {
        eval(true, "(regex-matches? \"[a-z]+\\\\d+\" \"abc123\")");
        eval(false, "(regex-matches? \"[a-z]+\\\\d+\" \"abc123!\")");
    }

    @Test
    public void testRegexMatchesEmpty() throws Exception
    {
        eval(true, "(regex-matches? \".*\" \"\")");
        eval(false, "(regex-matches? \".+\" \"\")");
    }
    // ========== regex-find ==========

    @Test
    public void testRegexFindSimple() throws Exception
    {
        eval(3, "(regex-find \"\\\\d+\" \"abc123def\")");
    }

    @Test
    public void testRegexFindAtStart() throws Exception
    {
        eval(0, "(regex-find \"\\\\d+\" \"123abc\")");
    }

    @Test
    public void testRegexFindNoMatch() throws Exception
    {
        eval(false, "(regex-find \"\\\\d+\" \"abcdef\")");
    }

    @Test
    public void testRegexFindWord() throws Exception
    {
        eval(6, "(regex-find \"world\" \"hello world\")");
    }
    // ========== Error Handling ==========

    @Test(expected = JlllException.class)
    public void testInvalidPattern() throws Exception
    {
        Jlll.eval("(regex-match \"[invalid\" \"test\")", env);
    }

    @Test(expected = JlllException.class)
    public void testRegexReplaceMissingArgs() throws Exception
    {
        Jlll.eval("(regex-replace \"\\\\d+\" \"test\")", env);
    }
    // ========== Complex Patterns ==========

    @Test
    public void testEmailPattern() throws Exception
    {
        eval(true, "(regex-matches? \"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}\" \"test@example.com\")");
        eval(false, "(regex-matches? \"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}\" \"invalid-email\")");
    }

    @Test
    public void testExtractEmails() throws Exception
    {
        Object result = Jlll.eval(
                "(regex-match-all \"[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}\" \"Contact: a@b.com and c@d.org\")",
                env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(2, list.length());
        assertEquals("a@b.com", list.get(0));
        assertEquals("c@d.org", list.get(1));
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
