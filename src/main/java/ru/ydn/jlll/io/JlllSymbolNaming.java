package ru.ydn.jlll.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import ru.ydn.jlll.common.JlllException;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: Oct 27, 2004
 * Time: 6:11:41 PM
 * To change this template use Options | File Templates.
 */
public class JlllSymbolNaming
{
    public static String convertFromInToSymbolName(String str) throws JlllException
    {
        if(str.indexOf("\\")>=0)
        {
            StringReader sr = new StringReader(str);
            BufferedReader br = new BufferedReader(sr);
            StringWriter sw = new StringWriter();
            try
            {
                while(true)
                {
                    int next = br.read();
                    if(next==-1) break;
                    if(next=='\\')
                    {
                        int next2 = br.read();
                        if(next2==-1) throw new JlllException("Broken symbol name1");
                        switch(next2)
                        {
                            case '\\':
                                sw.write('\\');
                                break;
                            case 'n':
                                sw.write('\n');
                                break;
                            case 's':
                                sw.write(' ');
                                break;
                            default:
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
                throw new JlllException("IOException while parsing",e);
            }
            return sw.toString();
        }
        else
        {
            return str;
        }
    }

    public static String convertFromSymbolNameToOut(String str)
    {
        return str.replace("\\","\\\\").replace(" ","\\s").replace("\n","\\n");
    }
}
