package ru.ydn.jlll.libs;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * Regular expression primitives for pattern matching and text manipulation.
 *
 * <p>
 * Provides functions for matching, searching, replacing, and splitting strings
 * using Java's regular expression engine. Patterns are cached for performance.
 * </p>
 *
 * <ul>
 * <li><b>Matching:</b> regex-match, regex-match-all, regex-matches?</li>
 * <li><b>Searching:</b> regex-find</li>
 * <li><b>Replacing:</b> regex-replace, regex-replace-first</li>
 * <li><b>Splitting:</b> regex-split</li>
 * </ul>
 */
public class RegexLib extends ReflectionLibrary
{
    /** Cache compiled patterns for performance (thread-safe) */
    private static final Map<String, Pattern> patternCache = new ConcurrentHashMap<>();

    /**
     * Gets or compiles a regex pattern, using cache for performance.
     *
     * @param regex
     *            the regular expression string
     * @return the compiled Pattern
     * @throws JlllException
     *             if the regex syntax is invalid
     */
    private static Pattern getPattern(String regex) throws JlllException
    {
        try
        {
            return patternCache.computeIfAbsent(regex, Pattern::compile);
        }
        catch (PatternSyntaxException e)
        {
            throw new JlllException("Invalid regex pattern: " + e.getMessage());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // regex-replace: Replace all occurrences (supports string or function replacement)
        new Primitive("regex-replace", env,
                "Replaces all matches of pattern in input. Replacement can be a string or "
                        + "a function that receives each match and returns the replacement. "
                        + "(regex-replace \"\\\\d+\" \"a1b2\" \"X\") => \"aXbX\". "
                        + "(regex-replace \"\\\\d+\" \"a1b2\" (lambda (m) (str (* 2 (string->number m))))) => \"a2b4\"")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.length() < 3)
                {
                    throw new JlllException("regex-replace requires pattern, input, and replacement arguments");
                }
                String pattern = (String) values.get(0);
                String input = (String) values.get(1);
                Object replacement = values.get(2);
                Pattern p = getPattern(pattern);
                Matcher m = p.matcher(input);
                if (replacement instanceof String)
                {
                    return m.replaceAll((String) replacement);
                }
                else if (replacement instanceof Procedure)
                {
                    Procedure fn = (Procedure) replacement;
                    StringBuilder result = new StringBuilder();
                    while (m.find())
                    {
                        String match = m.group();
                        Object replaced = fn.applyEvaluated(env, match);
                        m.appendReplacement(result, Matcher.quoteReplacement(String.valueOf(replaced)));
                    }
                    m.appendTail(result);
                    return result.toString();
                }
                else
                {
                    throw new JlllException("regex-replace: replacement must be string or function");
                }
            }
        };
    }

    /**
     * Finds the first match of pattern in input.
     * Returns a list containing the full match followed by any capture groups,
     * or false if no match is found.
     *
     * <p>
     * {@code (regex-match "a(\\d+)b" "a123b")} returns {@code ("a123b" "123")}
     * </p>
     *
     * @param pattern
     *            the regular expression
     * @param input
     *            the string to search
     * @return list of (full-match group1 group2 ...) or Boolean.FALSE
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-match")
    public Object regexMatch(String pattern, String input) throws JlllException
    {
        Pattern p = getPattern(pattern);
        Matcher m = p.matcher(input);
        if (m.find())
        {
            int groupCount = m.groupCount();
            if (groupCount == 0)
            {
                return m.group();
            }
            List<Object> groups = new ArrayList<>();
            groups.add(m.group(0)); // full match
            for (int i = 1; i <= groupCount; i++)
            {
                String g = m.group(i);
                groups.add(g != null ? g : "");
            }
            return ListUtil.arrayToCons(groups.toArray());
        }
        return Boolean.FALSE;
    }

    /**
     * Finds all matches of pattern in input.
     * Returns a list of all matches. Each match is either a string (no groups)
     * or a list (with capture groups).
     *
     * <p>
     * {@code (regex-match-all "\\d+" "a1b23c456")} returns {@code ("1" "23" "456")}
     * </p>
     *
     * @param pattern
     *            the regular expression
     * @param input
     *            the string to search
     * @return list of matches (strings or lists with groups)
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-match-all")
    public Cons regexMatchAll(String pattern, String input) throws JlllException
    {
        Pattern p = getPattern(pattern);
        Matcher m = p.matcher(input);
        List<Object> matches = new ArrayList<>();
        int groupCount = p.matcher("").groupCount();
        while (m.find())
        {
            if (groupCount == 0)
            {
                matches.add(m.group());
            }
            else
            {
                List<Object> groups = new ArrayList<>();
                groups.add(m.group(0));
                for (int i = 1; i <= groupCount; i++)
                {
                    String g = m.group(i);
                    groups.add(g != null ? g : "");
                }
                matches.add(ListUtil.arrayToCons(groups.toArray()));
            }
        }
        if (matches.isEmpty())
        {
            return new Cons(null, null);
        }
        return ListUtil.arrayToCons(matches.toArray());
    }

    /**
     * Replaces the first match of pattern in input with replacement string.
     *
     * <p>
     * {@code (regex-replace-first "\\d+" "a1b2c3" "X")} returns {@code "aXb2c3"}
     * </p>
     *
     * @param pattern
     *            the regular expression
     * @param input
     *            the string to search
     * @param replacement
     *            the replacement string
     * @return the string with first match replaced
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-replace-first")
    public String regexReplaceFirst(String pattern, String input, String replacement) throws JlllException
    {
        Pattern p = getPattern(pattern);
        return p.matcher(input).replaceFirst(replacement);
    }

    /**
     * Splits input string by pattern.
     * Empty strings are preserved (unlike some implementations).
     *
     * <p>
     * {@code (regex-split "\\s+" "a b  c")} returns {@code ("a" "b" "c")}
     * </p>
     *
     * @param pattern
     *            the regular expression delimiter
     * @param input
     *            the string to split
     * @return list of substrings
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-split")
    public Cons regexSplit(String pattern, String input) throws JlllException
    {
        Pattern p = getPattern(pattern);
        String[] parts = p.split(input, -1); // -1 to preserve trailing empty strings
        return ListUtil.arrayToCons(parts);
    }

    /**
     * Tests if the entire input string matches the pattern.
     *
     * <p>
     * {@code (regex-matches? "\\d+" "123")} returns {@code true}
     * </p>
     * <p>
     * {@code (regex-matches? "\\d+" "a123")} returns {@code false}
     * </p>
     *
     * @param pattern
     *            the regular expression
     * @param input
     *            the string to test
     * @return true if entire string matches, false otherwise
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-matches?")
    public Boolean regexMatches(String pattern, String input) throws JlllException
    {
        Pattern p = getPattern(pattern);
        return p.matcher(input).matches();
    }

    /**
     * Finds the index of the first match of pattern in input.
     * Returns the starting index or false if no match is found.
     *
     * <p>
     * {@code (regex-find "\\d+" "abc123def")} returns {@code 3}
     * </p>
     *
     * @param pattern
     *            the regular expression
     * @param input
     *            the string to search
     * @return starting index of first match, or Boolean.FALSE if not found
     * @throws JlllException
     *             if pattern is invalid
     */
    @JlllName("regex-find")
    public Object regexFind(String pattern, String input) throws JlllException
    {
        Pattern p = getPattern(pattern);
        Matcher m = p.matcher(input);
        if (m.find())
        {
            return m.start();
        }
        return Boolean.FALSE;
    }
}
