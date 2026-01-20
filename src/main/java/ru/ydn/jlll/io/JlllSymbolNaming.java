package ru.ydn.jlll.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import ru.ydn.jlll.common.JlllException;

/**
 * Symbol naming utilities for escape sequence handling.
 * Converts between internal symbol representation and display format,
 * handling special characters like spaces and newlines.
 */
public class JlllSymbolNaming
{
    /**
     * Converts an input string to internal symbol name, processing escape sequences.
     * Escape sequences: {@code \\} for backslash, {@code \n} for newline, {@code \s} for space.
     *
     * @param str
     *            the input string possibly containing escape sequences
     * @return the symbol name with escapes resolved
     * @throws JlllException
     *             if escape sequence is malformed
     */
    public static String convertFromInToSymbolName(String str) throws JlllException
    {
        if (str.indexOf("\\") >= 0)
        {
            StringReader sr = new StringReader(str);
            BufferedReader br = new BufferedReader(sr);
            StringWriter sw = new StringWriter();
            try
            {
                while (true)
                {
                    int next = br.read();
                    if (next == -1)
                        break;
                    if (next == '\\')
                    {
                        int next2 = br.read();
                        if (next2 == -1)
                            throw new JlllException("Broken symbol name1");
                        switch (next2)
                        {
                            case '\\' :
                                sw.write('\\');
                                break;
                            case 'n' :
                                sw.write('\n');
                                break;
                            case 's' :
                                sw.write(' ');
                                break;
                            default :
                                throw new JlllException("Broken symbol name");
                        }
                    }
                    else
                    {
                        sw.write(next);
                    }
                }
            }
            catch (IOException e)
            {
                throw new JlllException("IOException while parsing", e);
            }
            return sw.toString();
        }
        else
        {
            return str;
        }
    }

    /**
     * Converts a symbol name to output format, escaping special characters.
     *
     * @param str
     *            the internal symbol name
     * @return the display string with special characters escaped
     */
    public static String convertFromSymbolNameToOut(String str)
    {
        return str.replace("\\", "\\\\").replace(" ", "\\s").replace("\n", "\\n");
    }
}
