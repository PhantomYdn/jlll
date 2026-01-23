package ru.ydn.jlll.libs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * String manipulation primitives.
 *
 * <p>
 * Provides comprehensive string operations:
 * </p>
 * <ul>
 * <li><b>Length/Access:</b> string-length, substring, string-ref</li>
 * <li><b>Search:</b> string-index, string-contains?</li>
 * <li><b>Transform:</b> string-upcase, string-downcase, string-trim</li>
 * <li><b>Manipulation:</b> string-replace, string-split, string-join</li>
 * <li><b>Conversion:</b> string-&gt;number, number-&gt;string, string-&gt;list, list-&gt;string</li>
 * <li><b>Comparison:</b> string=?, string&lt;?, string-ci=?</li>
 * <li><b>Construction:</b> make-string</li>
 * </ul>
 *
 * <p>
 * JLLL does not have a character type, so functions like string-ref return
 * single-character strings instead of characters.
 * </p>
 */
public class StringLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // string-split: Split string by delimiter, return list
        new Primitive("string-split", env,
                "Splits a string by delimiter. (string-split \"a,b,c\" \",\") returns (\"a\" \"b\" \"c\").")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String s = (String) values.get(0);
                String delim = (String) values.get(1);
                // Use split with -1 to keep trailing empty strings
                String[] parts = s.split(java.util.regex.Pattern.quote(delim), -1);
                return ListUtil.arrayToCons(parts);
            }
        };
        // string-join: Join list elements with delimiter
        new Primitive("string-join", env,
                "Joins a list of strings with delimiter. (string-join '(\"a\" \"b\" \"c\") \",\") returns \"a,b,c\".")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object listArg = values.get(0);
                String delim = (String) values.get(1);
                if (listArg == null || (listArg instanceof Cons && ((Cons) listArg).car() == null))
                {
                    return "";
                }
                Cons list = (Cons) listArg;
                StringBuilder sb = new StringBuilder();
                Iterator<?> it = list.iterator();
                boolean first = true;
                while (it.hasNext())
                {
                    if (!first)
                    {
                        sb.append(delim);
                    }
                    Object item = it.next();
                    sb.append(item == null ? "" : item.toString());
                    first = false;
                }
                return sb.toString();
            }
        };
        // string->number: Parse string to number with optional radix
        new Primitive("string->number", env, "Converts string to number. (string->number \"42\") returns 42. "
                + "(string->number \"ff\" 16) returns 255. Returns false if parsing fails.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String s = (String) values.get(0);
                int radix = 10;
                if (values.length() > 1)
                {
                    radix = ((Number) values.get(1)).intValue();
                }
                try
                {
                    // Try parsing as integer first
                    if (radix == 10 && s.contains("."))
                    {
                        return Double.parseDouble(s);
                    }
                    return Integer.parseInt(s, radix);
                }
                catch (NumberFormatException e)
                {
                    // Return false on parse failure (Scheme convention)
                    return Boolean.FALSE;
                }
            }
        };
        // number->string: Format number as string with optional radix
        new Primitive("number->string", env, "Converts number to string. (number->string 42) returns \"42\". "
                + "(number->string 255 16) returns \"ff\".")
        {
            private static final long serialVersionUID = 4L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Number n = (Number) values.get(0);
                int radix = 10;
                if (values.length() > 1)
                {
                    radix = ((Number) values.get(1)).intValue();
                }
                if (radix == 10)
                {
                    return n.toString();
                }
                return Integer.toString(n.intValue(), radix);
            }
        };
        // string->list: Convert string to list of single-char strings
        new Primitive("string->list", env, "Converts string to list of single-character strings. "
                + "(string->list \"abc\") returns (\"a\" \"b\" \"c\").")
        {
            private static final long serialVersionUID = 5L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String s = (String) values.get(0);
                if (s.isEmpty())
                {
                    return new Cons(null, null);
                }
                List<Object> chars = new ArrayList<>(s.length());
                for (int i = 0; i < s.length(); i++)
                {
                    chars.add(String.valueOf(s.charAt(i)));
                }
                return ListUtil.arrayToCons(chars.toArray());
            }
        };
        // list->string: Convert list of strings to single string
        new Primitive("list->string", env,
                "Converts list of strings to single string. " + "(list->string '(\"a\" \"b\" \"c\")) returns \"abc\".")
        {
            private static final long serialVersionUID = 6L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object listArg = values.get(0);
                if (listArg == null || (listArg instanceof Cons && ((Cons) listArg).car() == null))
                {
                    return "";
                }
                Cons list = (Cons) listArg;
                StringBuilder sb = new StringBuilder();
                Iterator<?> it = list.iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    sb.append(item == null ? "" : item.toString());
                }
                return sb.toString();
            }
        };
        // make-string: Create string of repeated characters
        new Primitive("make-string", env,
                "Creates a string of n copies of a character. " + "(make-string 5 \"x\") returns \"xxxxx\".")
        {
            private static final long serialVersionUID = 7L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                int n = ((Number) values.get(0)).intValue();
                String ch = (String) values.get(1);
                if (n <= 0)
                {
                    return "";
                }
                StringBuilder sb = new StringBuilder(n * ch.length());
                for (int i = 0; i < n; i++)
                {
                    sb.append(ch);
                }
                return sb.toString();
            }
        };
        // substring with 2 args (start to end of string)
        new Primitive("substring", env, "Extracts a substring. (substring \"hello\" 1 3) returns \"el\". "
                + "(substring \"hello\" 2) returns \"llo\".")
        {
            private static final long serialVersionUID = 8L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String s = (String) values.get(0);
                int start = ((Number) values.get(1)).intValue();
                if (values.length() == 2)
                {
                    return s.substring(start);
                }
                int end = ((Number) values.get(2)).intValue();
                return s.substring(start, end);
            }
        };
        // string-index that returns false instead of -1 when not found
        new Primitive("string-index", env, "Finds the index of a substring. (string-index \"hello\" \"l\") returns 2. "
                + "Returns false if not found.")
        {
            private static final long serialVersionUID = 9L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String s = (String) values.get(0);
                String substr = (String) values.get(1);
                int idx = s.indexOf(substr);
                return idx < 0 ? Boolean.FALSE : idx;
            }
        };
        Jlll.eval("(load-system-script \"string.jlll\")", env);
    }
    // ========== Simple methods using @JlllName ==========

    /**
     * Returns the length of a string.
     * ({@code (string-length "hello")}) returns 5.
     *
     * @param s
     *            the string
     * @return the number of characters
     */
    @JlllName("string-length")
    public Integer stringLength(String s)
    {
        return s.length();
    }

    /**
     * Returns the character at the given index as a single-character string.
     * ({@code (string-ref "hello" 0)}) returns "h".
     *
     * @param s
     *            the string
     * @param index
     *            the 0-based index
     * @return single-character string at that position
     */
    @JlllName("string-ref")
    public String stringRef(String s, Integer index)
    {
        return String.valueOf(s.charAt(index));
    }

    /**
     * Checks if a string contains a substring.
     * ({@code (string-contains? "hello" "ell")}) returns true.
     *
     * @param s
     *            the string to search in
     * @param substr
     *            the substring to find
     * @return true if s contains substr
     */
    @JlllName("string-contains?")
    public Boolean stringContains(String s, String substr)
    {
        return s.contains(substr);
    }

    /**
     * Converts string to uppercase.
     * ({@code (string-upcase "hello")}) returns "HELLO".
     *
     * @param s
     *            the string
     * @return uppercase version
     */
    @JlllName("string-upcase")
    public String stringUpcase(String s)
    {
        return s.toUpperCase();
    }

    /**
     * Converts string to lowercase.
     * ({@code (string-downcase "HELLO")}) returns "hello".
     *
     * @param s
     *            the string
     * @return lowercase version
     */
    @JlllName("string-downcase")
    public String stringDowncase(String s)
    {
        return s.toLowerCase();
    }

    /**
     * Removes leading and trailing whitespace.
     * ({@code (string-trim "  hello  ")}) returns "hello".
     *
     * @param s
     *            the string
     * @return trimmed string
     */
    @JlllName("string-trim")
    public String stringTrim(String s)
    {
        return s.trim();
    }

    /**
     * Removes leading whitespace.
     * ({@code (string-trim-left "  hello")}) returns "hello".
     *
     * @param s
     *            the string
     * @return string with leading whitespace removed
     */
    @JlllName("string-trim-left")
    public String stringTrimLeft(String s)
    {
        return s.stripLeading();
    }

    /**
     * Removes trailing whitespace.
     * ({@code (string-trim-right "hello  ")}) returns "hello".
     *
     * @param s
     *            the string
     * @return string with trailing whitespace removed
     */
    @JlllName("string-trim-right")
    public String stringTrimRight(String s)
    {
        return s.stripTrailing();
    }

    /**
     * Replaces all occurrences of a substring.
     * ({@code (string-replace "hello" "l" "L")}) returns "heLLo".
     *
     * @param s
     *            the string
     * @param oldStr
     *            substring to replace
     * @param newStr
     *            replacement string
     * @return string with replacements made
     */
    @JlllName("string-replace")
    public String stringReplace(String s, String oldStr, String newStr)
    {
        return s.replace(oldStr, newStr);
    }
    // ========== String Comparison ==========

    /**
     * String equality comparison.
     * ({@code (string=? "hello" "hello")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if strings are equal
     */
    @JlllName("string=?")
    public Boolean stringEquals(String s1, String s2)
    {
        return s1.equals(s2);
    }

    /**
     * String less-than comparison (lexicographic).
     * ({@code (string<? "a" "b")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &lt; s2
     */
    @JlllName("string<?")
    public Boolean stringLessThan(String s1, String s2)
    {
        return s1.compareTo(s2) < 0;
    }

    /**
     * String greater-than comparison (lexicographic).
     * ({@code (string>? "b" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &gt; s2
     */
    @JlllName("string>?")
    public Boolean stringGreaterThan(String s1, String s2)
    {
        return s1.compareTo(s2) > 0;
    }

    /**
     * String less-than-or-equal comparison (lexicographic).
     * ({@code (string<=? "a" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &lt;= s2
     */
    @JlllName("string<=?")
    public Boolean stringLessThanOrEqual(String s1, String s2)
    {
        return s1.compareTo(s2) <= 0;
    }

    /**
     * String greater-than-or-equal comparison (lexicographic).
     * ({@code (string>=? "b" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &gt;= s2
     */
    @JlllName("string>=?")
    public Boolean stringGreaterThanOrEqual(String s1, String s2)
    {
        return s1.compareTo(s2) >= 0;
    }
    // ========== Case-Insensitive Comparison ==========

    /**
     * Case-insensitive string equality.
     * ({@code (string-ci=? "Hello" "hello")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if strings are equal ignoring case
     */
    @JlllName("string-ci=?")
    public Boolean stringCiEquals(String s1, String s2)
    {
        return s1.equalsIgnoreCase(s2);
    }

    /**
     * Case-insensitive less-than comparison.
     * ({@code (string-ci<? "A" "b")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &lt; s2 (case-insensitive)
     */
    @JlllName("string-ci<?")
    public Boolean stringCiLessThan(String s1, String s2)
    {
        return s1.compareToIgnoreCase(s2) < 0;
    }

    /**
     * Case-insensitive greater-than comparison.
     * ({@code (string-ci>? "B" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &gt; s2 (case-insensitive)
     */
    @JlllName("string-ci>?")
    public Boolean stringCiGreaterThan(String s1, String s2)
    {
        return s1.compareToIgnoreCase(s2) > 0;
    }

    /**
     * Case-insensitive less-than-or-equal comparison.
     * ({@code (string-ci<=? "A" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &lt;= s2 (case-insensitive)
     */
    @JlllName("string-ci<=?")
    public Boolean stringCiLessThanOrEqual(String s1, String s2)
    {
        return s1.compareToIgnoreCase(s2) <= 0;
    }

    /**
     * Case-insensitive greater-than-or-equal comparison.
     * ({@code (string-ci>=? "B" "a")}) returns true.
     *
     * @param s1
     *            first string
     * @param s2
     *            second string
     * @return true if s1 &gt;= s2 (case-insensitive)
     */
    @JlllName("string-ci>=?")
    public Boolean stringCiGreaterThanOrEqual(String s1, String s2)
    {
        return s1.compareToIgnoreCase(s2) >= 0;
    }
}
