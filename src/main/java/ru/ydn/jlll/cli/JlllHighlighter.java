package ru.ydn.jlll.cli;

import java.util.regex.Pattern;
import org.jline.reader.Highlighter;
import org.jline.reader.LineReader;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStringBuilder;
import org.jline.utils.AttributedStyle;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;

/**
 * Syntax highlighter for JLLL REPL.
 *
 * <p>
 * Provides context-aware highlighting based on actual environment bindings:
 * </p>
 * <ul>
 * <li><b>Blue:</b> Callable bindings (Procedure, Primitive, Macros)</li>
 * <li><b>Cyan:</b> Variable bindings (non-callable values)</li>
 * <li><b>Default:</b> Unbound symbols</li>
 * <li><b>Magenta:</b> Keywords (:foo syntax)</li>
 * <li><b>Green:</b> String literals</li>
 * <li><b>Yellow:</b> Numbers</li>
 * <li><b>Gray/italic:</b> Comments</li>
 * <li><b>Red:</b> Quote characters and unmatched parens</li>
 * </ul>
 */
public class JlllHighlighter implements Highlighter
{
    private final JlllRepl repl;
    // Styles
    private static final AttributedStyle STYLE_DEFAULT = AttributedStyle.DEFAULT;
    private static final AttributedStyle STYLE_CALLABLE = AttributedStyle.DEFAULT.foreground(AttributedStyle.BLUE);
    private static final AttributedStyle STYLE_VARIABLE = AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN);
    private static final AttributedStyle STYLE_KEYWORD = AttributedStyle.DEFAULT.foreground(AttributedStyle.MAGENTA);
    private static final AttributedStyle STYLE_STRING = AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN);
    private static final AttributedStyle STYLE_NUMBER = AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW);
    private static final AttributedStyle STYLE_COMMENT = AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT)
            .italic();
    private static final AttributedStyle STYLE_PAREN = AttributedStyle.DEFAULT.foreground(AttributedStyle.BRIGHT);
    private static final AttributedStyle STYLE_QUOTE = AttributedStyle.DEFAULT.foreground(AttributedStyle.RED);
    private static final AttributedStyle STYLE_ERROR = AttributedStyle.DEFAULT.foreground(AttributedStyle.RED).bold();

    /**
     * Creates a highlighter for the given REPL session.
     *
     * @param repl
     *            the REPL session to get environment from
     */
    public JlllHighlighter(JlllRepl repl)
    {
        this.repl = repl;
    }

    /**
     * Highlights a line of JLLL code with syntax coloring.
     *
     * @param reader
     *            the line reader
     * @param buffer
     *            the input text to highlight
     * @return the highlighted attributed string
     */
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
                if (end == -1)
                    end = buffer.length();
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
                if (c == '(')
                    parenDepth++;
                if (c == ')')
                    parenDepth--;
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
            else
            {
                sb.styled(getStyleForToken(token), token);
            }
        }
        return sb.toAttributedString();
    }

    /**
     * Determines the style for a token based on its binding in the environment.
     */
    private AttributedStyle getStyleForToken(String token)
    {
        // Check for keyword syntax (:foo)
        if (token.startsWith(":"))
        {
            return STYLE_KEYWORD;
        }
        // Look up in current environment
        Environment env = repl.getEnvironment();
        Symbol sym = Symbol.intern(token);
        Object value = env.lookup(sym);
        if (value == null)
        {
            // Unbound - default color
            return STYLE_DEFAULT;
        }
        else if (value instanceof Procedure || value instanceof Primitive || value instanceof Macros)
        {
            // Callable - blue
            return STYLE_CALLABLE;
        }
        else
        {
            // Variable (non-callable) - cyan
            return STYLE_VARIABLE;
        }
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
        return c == '(' || c == ')' || c == '[' || c == ']' || c == '\'' || c == '`' || c == ',' || c == '@' || c == '"'
                || c == ';' || Character.isWhitespace(c);
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
            if (token.length() == 1)
                return false;
            start = 1;
        }
        boolean hasDecimal = false;
        boolean hasExponent = false;
        for (int i = start; i < token.length(); i++)
        {
            char c = token.charAt(i);
            if (c == '.')
            {
                if (hasDecimal || hasExponent)
                    return false;
                hasDecimal = true;
            }
            else if (c == 'e' || c == 'E')
            {
                if (hasExponent)
                    return false;
                hasExponent = true;
                // Allow sign after exponent
                if (i + 1 < token.length())
                {
                    char next = token.charAt(i + 1);
                    if (next == '+' || next == '-')
                        i++;
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
