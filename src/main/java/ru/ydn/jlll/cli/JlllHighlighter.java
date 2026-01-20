package ru.ydn.jlll.cli;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.jline.reader.Highlighter;
import org.jline.reader.LineReader;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;

/**
 * Syntax highlighter for JLLL REPL.
 * Colorizes keywords, strings, numbers, comments, and parentheses.
 */
public class JlllHighlighter implements Highlighter
{
    // Keywords to highlight
    private static final Set<String> KEYWORDS = new HashSet<>();
    private static final Set<String> SPECIAL_FORMS = new HashSet<>();
    private static final Set<String> BUILTINS = new HashSet<>();

    static
    {
        // Special forms (blue)
        SPECIAL_FORMS.add("define");
        SPECIAL_FORMS.add("defmacro");
        SPECIAL_FORMS.add("lambda");
        SPECIAL_FORMS.add("if");
        SPECIAL_FORMS.add("cond");
        SPECIAL_FORMS.add("case");
        SPECIAL_FORMS.add("let");
        SPECIAL_FORMS.add("let*");
        SPECIAL_FORMS.add("letrec");
        SPECIAL_FORMS.add("begin");
        SPECIAL_FORMS.add("quote");
        SPECIAL_FORMS.add("quasiquote");
        SPECIAL_FORMS.add("set");
        SPECIAL_FORMS.add("set!");

        // Built-in functions (cyan)
        BUILTINS.add("car");
        BUILTINS.add("cdr");
        BUILTINS.add("cons");
        BUILTINS.add("list");
        BUILTINS.add("append");
        BUILTINS.add("map");
        BUILTINS.add("filter");
        BUILTINS.add("apply");
        BUILTINS.add("eval");
        BUILTINS.add("and");
        BUILTINS.add("or");
        BUILTINS.add("not");
        BUILTINS.add("load-lib");
        BUILTINS.add("load-url");
        BUILTINS.add("print");
        BUILTINS.add("println");
        BUILTINS.add("concat");
        BUILTINS.add("describe");

        // Constants/keywords (magenta)
        KEYWORDS.add("true");
        KEYWORDS.add("false");
        KEYWORDS.add("null");
        KEYWORDS.add("nil");
        KEYWORDS.add("else");
        KEYWORDS.add("#t");
        KEYWORDS.add("#f");
    }

    // Styles
    private static final AttributedStyle STYLE_DEFAULT = AttributedStyle.DEFAULT;
    private static final AttributedStyle STYLE_SPECIAL_FORM = AttributedStyle.BOLD.foreground(AttributedStyle.BLUE);
    private static final AttributedStyle STYLE_BUILTIN = AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN);
    private static final AttributedStyle STYLE_KEYWORD = AttributedStyle.DEFAULT.foreground(AttributedStyle.MAGENTA);
    private static final AttributedStyle STYLE_STRING = AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN);
    private static final AttributedStyle STYLE_NUMBER = AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW);
    private static final AttributedStyle STYLE_COMMENT = AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT).italic();
    private static final AttributedStyle STYLE_PAREN = AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT);
    private static final AttributedStyle STYLE_QUOTE = AttributedStyle.DEFAULT.foreground(AttributedStyle.RED);
    private static final AttributedStyle STYLE_ERROR = AttributedStyle.DEFAULT.foreground(AttributedStyle.RED).bold();

    @Override
    public AttributedString highlight(LineReader reader, String buffer)
    {
        AttributedStringBuilder sb = new AttributedStringBuilder();

        int i = 0;
        int parenDepth = 0;

        while (i < buffer.length())
        {
            char c = buffer.charAt(i);

            // Comment (starts with ;)
            if (c == ';')
            {
                int end = buffer.indexOf('\n', i);
                if (end == -1) end = buffer.length();
                sb.styled(STYLE_COMMENT, buffer.substring(i, end));
                i = end;
                continue;
            }

            // String literal
            if (c == '"')
            {
                int end = findStringEnd(buffer, i);
                sb.styled(STYLE_STRING, buffer.substring(i, end));
                i = end;
                continue;
            }

            // Quote characters
            if (c == '\'' || c == '`' || c == ',')
            {
                sb.styled(STYLE_QUOTE, String.valueOf(c));
                i++;
                // Handle ,@ for unquote-splicing
                if (c == ',' && i < buffer.length() && buffer.charAt(i) == '@')
                {
                    sb.styled(STYLE_QUOTE, "@");
                    i++;
                }
                continue;
            }

            // Parentheses
            if (c == '(' || c == ')')
            {
                if (c == '(') parenDepth++;
                if (c == ')') parenDepth--;

                if (parenDepth < 0)
                {
                    // Unmatched closing paren - highlight as error
                    sb.styled(STYLE_ERROR, String.valueOf(c));
                }
                else
                {
                    sb.styled(STYLE_PAREN, String.valueOf(c));
                }
                i++;
                continue;
            }

            // Brackets (alternative list syntax)
            if (c == '[' || c == ']')
            {
                sb.styled(STYLE_PAREN, String.valueOf(c));
                i++;
                continue;
            }

            // Whitespace
            if (Character.isWhitespace(c))
            {
                sb.append(c);
                i++;
                continue;
            }

            // Symbol or number
            int start = i;
            while (i < buffer.length() && !isDelimiter(buffer.charAt(i)))
            {
                i++;
            }

            String token = buffer.substring(start, i);

            if (isNumber(token))
            {
                sb.styled(STYLE_NUMBER, token);
            }
            else if (SPECIAL_FORMS.contains(token))
            {
                sb.styled(STYLE_SPECIAL_FORM, token);
            }
            else if (BUILTINS.contains(token))
            {
                sb.styled(STYLE_BUILTIN, token);
            }
            else if (KEYWORDS.contains(token))
            {
                sb.styled(STYLE_KEYWORD, token);
            }
            else
            {
                sb.styled(STYLE_DEFAULT, token);
            }
        }

        return sb.toAttributedString();
    }

    @Override
    public void setErrorPattern(Pattern errorPattern)
    {
        // Not used
    }

    @Override
    public void setErrorIndex(int errorIndex)
    {
        // Not used
    }

    /**
     * Find the end of a string literal (handling escape sequences).
     */
    private int findStringEnd(String buffer, int start)
    {
        int i = start + 1;
        while (i < buffer.length())
        {
            char c = buffer.charAt(i);
            if (c == '\\' && i + 1 < buffer.length())
            {
                i += 2; // Skip escaped character
                continue;
            }
            if (c == '"')
            {
                return i + 1;
            }
            i++;
        }
        return buffer.length();
    }

    /**
     * Check if a character is a delimiter.
     */
    private boolean isDelimiter(char c)
    {
        return c == '(' || c == ')' || c == '[' || c == ']' ||
               c == '\'' || c == '`' || c == ',' || c == '@' ||
               c == '"' || c == ';' || Character.isWhitespace(c);
    }

    /**
     * Check if a token is a number.
     */
    private boolean isNumber(String token)
    {
        if (token.isEmpty())
        {
            return false;
        }

        int start = 0;
        if (token.charAt(0) == '-' || token.charAt(0) == '+')
        {
            if (token.length() == 1) return false;
            start = 1;
        }

        boolean hasDecimal = false;
        boolean hasExponent = false;

        for (int i = start; i < token.length(); i++)
        {
            char c = token.charAt(i);

            if (c == '.')
            {
                if (hasDecimal || hasExponent) return false;
                hasDecimal = true;
            }
            else if (c == 'e' || c == 'E')
            {
                if (hasExponent) return false;
                hasExponent = true;
                // Allow sign after exponent
                if (i + 1 < token.length())
                {
                    char next = token.charAt(i + 1);
                    if (next == '+' || next == '-') i++;
                }
            }
            else if (!Character.isDigit(c))
            {
                return false;
            }
        }

        return true;
    }
}
