package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;

/**
 * Tests for the StringLib string manipulation functions.
 */
public class StringLibTestCase
{
    private final Environment env;

    public StringLibTestCase()
    {
        env = new Environment(Environment.top);
    }
    // ========== Length and Access ==========

    @Test
    public void testStringLength() throws Exception
    {
        eval(5, "(string-length \"hello\")");
        eval(0, "(string-length \"\")");
        eval(1, "(string-length \"x\")");
        eval(11, "(string-length \"hello world\")");
    }

    @Test
    public void testSubstring() throws Exception
    {
        eval("el", "(substring \"hello\" 1 3)");
        eval("hello", "(substring \"hello\" 0 5)");
        eval("", "(substring \"hello\" 2 2)");
        // substring with 2 args (to end)
        eval("llo", "(substring \"hello\" 2)");
        eval("hello", "(substring \"hello\" 0)");
        eval("", "(substring \"hello\" 5)");
    }

    @Test
    public void testStringRef() throws Exception
    {
        eval("h", "(string-ref \"hello\" 0)");
        eval("e", "(string-ref \"hello\" 1)");
        eval("o", "(string-ref \"hello\" 4)");
    }
    // ========== Search ==========

    @Test
    public void testStringIndex() throws Exception
    {
        eval(2, "(string-index \"hello\" \"l\")");
        eval(0, "(string-index \"hello\" \"h\")");
        eval(4, "(string-index \"hello\" \"o\")");
        eval(false, "(string-index \"hello\" \"x\")");
        eval(false, "(string-index \"hello\" \"xyz\")");
        eval(1, "(string-index \"hello\" \"ell\")");
    }

    @Test
    public void testStringContains() throws Exception
    {
        eval(true, "(string-contains? \"hello\" \"ell\")");
        eval(true, "(string-contains? \"hello\" \"hello\")");
        eval(true, "(string-contains? \"hello\" \"\")");
        eval(false, "(string-contains? \"hello\" \"xyz\")");
        eval(false, "(string-contains? \"hello\" \"Hello\")");
    }
    // ========== Case Conversion ==========

    @Test
    public void testStringUpcase() throws Exception
    {
        eval("HELLO", "(string-upcase \"hello\")");
        eval("HELLO", "(string-upcase \"HELLO\")");
        eval("HELLO WORLD", "(string-upcase \"Hello World\")");
        eval("", "(string-upcase \"\")");
    }

    @Test
    public void testStringDowncase() throws Exception
    {
        eval("hello", "(string-downcase \"HELLO\")");
        eval("hello", "(string-downcase \"hello\")");
        eval("hello world", "(string-downcase \"Hello World\")");
        eval("", "(string-downcase \"\")");
    }
    // ========== Trimming ==========

    @Test
    public void testStringTrim() throws Exception
    {
        eval("hello", "(string-trim \"  hello  \")");
        eval("hello", "(string-trim \"hello\")");
        eval("", "(string-trim \"   \")");
        eval("hello world", "(string-trim \"  hello world  \")");
    }

    @Test
    public void testStringTrimLeft() throws Exception
    {
        eval("hello  ", "(string-trim-left \"  hello  \")");
        eval("hello", "(string-trim-left \"hello\")");
        eval("", "(string-trim-left \"   \")");
    }

    @Test
    public void testStringTrimRight() throws Exception
    {
        eval("  hello", "(string-trim-right \"  hello  \")");
        eval("hello", "(string-trim-right \"hello\")");
        eval("", "(string-trim-right \"   \")");
    }
    // ========== Replacement ==========

    @Test
    public void testStringReplace() throws Exception
    {
        eval("heLLo", "(string-replace \"hello\" \"l\" \"L\")");
        eval("hello", "(string-replace \"hello\" \"x\" \"y\")");
        eval("heXXo", "(string-replace \"hello\" \"ll\" \"XX\")");
        eval("heo", "(string-replace \"hello\" \"ll\" \"\")");
        eval("world world", "(string-replace \"hello hello\" \"hello\" \"world\")");
    }
    // ========== Split and Join ==========

    @Test
    public void testStringSplit() throws Exception
    {
        Object expected = Jlll.prepare("(\"a\" \"b\" \"c\")");
        eval(expected, "(string-split \"a,b,c\" \",\")");
        expected = Jlll.prepare("(\"hello\" \"world\")");
        eval(expected, "(string-split \"hello world\" \" \")");
        expected = Jlll.prepare("(\"hello\")");
        eval(expected, "(string-split \"hello\" \",\")");
        // Trailing empty strings preserved
        expected = Jlll.prepare("(\"a\" \"\" \"c\")");
        eval(expected, "(string-split \"a,,c\" \",\")");
    }

    @Test
    public void testStringJoin() throws Exception
    {
        eval("a,b,c", "(string-join '(\"a\" \"b\" \"c\") \",\")");
        eval("hello world", "(string-join '(\"hello\" \"world\") \" \")");
        eval("abc", "(string-join '(\"a\" \"b\" \"c\") \"\")");
        eval("hello", "(string-join '(\"hello\") \",\")");
        eval("", "(string-join '() \",\")");
    }
    // ========== Number Conversion ==========

    @Test
    public void testStringToNumber() throws Exception
    {
        eval(42, "(string->number \"42\")");
        eval(0, "(string->number \"0\")");
        eval(-17, "(string->number \"-17\")");
        eval(3.14, "(string->number \"3.14\")");
        eval(255, "(string->number \"ff\" 16)");
        eval(255, "(string->number \"FF\" 16)");
        eval(7, "(string->number \"111\" 2)");
        eval(false, "(string->number \"not-a-number\")");
        eval(false, "(string->number \"12.34.56\")");
    }

    @Test
    public void testNumberToString() throws Exception
    {
        eval("42", "(number->string 42)");
        eval("0", "(number->string 0)");
        eval("-17", "(number->string -17)");
        eval("ff", "(number->string 255 16)");
        eval("111", "(number->string 7 2)");
        eval("1010", "(number->string 10 2)");
    }
    // ========== List Conversion ==========

    @Test
    public void testStringToList() throws Exception
    {
        Object expected = Jlll.prepare("(\"a\" \"b\" \"c\")");
        eval(expected, "(string->list \"abc\")");
        expected = Jlll.prepare("(\"h\" \"e\" \"l\" \"l\" \"o\")");
        eval(expected, "(string->list \"hello\")");
        // Empty string returns empty list
        Object result = Jlll.eval("(string->list \"\")", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car());
    }

    @Test
    public void testListToString() throws Exception
    {
        eval("abc", "(list->string '(\"a\" \"b\" \"c\"))");
        eval("hello", "(list->string '(\"h\" \"e\" \"l\" \"l\" \"o\"))");
        eval("", "(list->string '())");
        // Works with any string elements, not just single chars
        eval("helloworld", "(list->string '(\"hello\" \"world\"))");
    }
    // ========== String Construction ==========

    @Test
    public void testMakeString() throws Exception
    {
        eval("xxxxx", "(make-string 5 \"x\")");
        eval("", "(make-string 0 \"x\")");
        eval("aaa", "(make-string 3 \"a\")");
        // Works with multi-char strings too
        eval("ababab", "(make-string 3 \"ab\")");
    }
    // ========== Comparison ==========

    @Test
    public void testStringEquals() throws Exception
    {
        eval(true, "(string=? \"hello\" \"hello\")");
        eval(false, "(string=? \"hello\" \"world\")");
        eval(false, "(string=? \"hello\" \"Hello\")");
        eval(true, "(string=? \"\" \"\")");
    }

    @Test
    public void testStringLessThan() throws Exception
    {
        eval(true, "(string<? \"a\" \"b\")");
        eval(false, "(string<? \"b\" \"a\")");
        eval(false, "(string<? \"a\" \"a\")");
        eval(true, "(string<? \"apple\" \"banana\")");
        eval(true, "(string<? \"A\" \"a\")"); // uppercase < lowercase in ASCII
    }

    @Test
    public void testStringGreaterThan() throws Exception
    {
        eval(true, "(string>? \"b\" \"a\")");
        eval(false, "(string>? \"a\" \"b\")");
        eval(false, "(string>? \"a\" \"a\")");
        eval(true, "(string>? \"banana\" \"apple\")");
    }

    @Test
    public void testStringLessThanOrEqual() throws Exception
    {
        eval(true, "(string<=? \"a\" \"b\")");
        eval(true, "(string<=? \"a\" \"a\")");
        eval(false, "(string<=? \"b\" \"a\")");
    }

    @Test
    public void testStringGreaterThanOrEqual() throws Exception
    {
        eval(true, "(string>=? \"b\" \"a\")");
        eval(true, "(string>=? \"a\" \"a\")");
        eval(false, "(string>=? \"a\" \"b\")");
    }
    // ========== Case-Insensitive Comparison ==========

    @Test
    public void testStringCiEquals() throws Exception
    {
        eval(true, "(string-ci=? \"hello\" \"HELLO\")");
        eval(true, "(string-ci=? \"Hello\" \"hElLo\")");
        eval(false, "(string-ci=? \"hello\" \"world\")");
    }

    @Test
    public void testStringCiLessThan() throws Exception
    {
        eval(true, "(string-ci<? \"a\" \"B\")");
        eval(true, "(string-ci<? \"A\" \"b\")");
        eval(false, "(string-ci<? \"b\" \"A\")");
        eval(false, "(string-ci<? \"a\" \"A\")");
    }

    @Test
    public void testStringCiGreaterThan() throws Exception
    {
        eval(true, "(string-ci>? \"B\" \"a\")");
        eval(true, "(string-ci>? \"b\" \"A\")");
        eval(false, "(string-ci>? \"A\" \"b\")");
        eval(false, "(string-ci>? \"a\" \"A\")");
    }

    @Test
    public void testStringCiLessThanOrEqual() throws Exception
    {
        eval(true, "(string-ci<=? \"a\" \"B\")");
        eval(true, "(string-ci<=? \"a\" \"A\")");
        eval(false, "(string-ci<=? \"b\" \"A\")");
    }

    @Test
    public void testStringCiGreaterThanOrEqual() throws Exception
    {
        eval(true, "(string-ci>=? \"B\" \"a\")");
        eval(true, "(string-ci>=? \"a\" \"A\")");
        eval(false, "(string-ci>=? \"A\" \"b\")");
    }
    // ========== Aliases and Helpers ==========

    @Test
    public void testStringAppend() throws Exception
    {
        eval("abc", "(string-append \"a\" \"b\" \"c\")");
        eval("hello world", "(string-append \"hello\" \" \" \"world\")");
        eval("", "(string-append)");
    }

    @Test
    public void testStringEmpty() throws Exception
    {
        eval(true, "(string-empty? \"\")");
        eval(false, "(string-empty? \"x\")");
        eval(false, "(string-empty? \"hello\")");
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
