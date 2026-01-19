package ru.ydn.jlll.io;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.util.ListUtil;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: May 6, 2003
 * Time: 4:25:17 PM
 * To change this template use Options | File Templates.
 */
public class JlllTokenizer extends StreamTokenizer
{
    private boolean readList = false;

    public JlllTokenizer(Reader reader)
    {
        super(reader);
        setSyntax();
    }

    private void setSyntax()
    {
//        resetSyntax();
//        wordChars('a','z');
//        wordChars('A','Z');
//        this.
////        wordChars('0','9');
//        wordChars('.','.');
//        this.
//        wordChars('*','+');
//        wordChars('-','-');
//        quoteChar('"');
//        quoteChar('\'');        commentChar(';');
//
//        whitespaceChars('\n','\n');
//        whitespaceChars(0,' ');
//        ordinaryChar('\'');
////        ordinaryChar('.');
        resetSyntax();
        // changes from resetSyntax
        wordChars('a', 'z');
        wordChars('A', 'Z');
        wordChars(128 + 32, 255);
        whitespaceChars(0, ' ');
        quoteChar('"');
        quoteChar('\'');
        // parseNumbers();
        wordChars('0', '9');

        // newlines are whitespace!
        whitespaceChars('\n', '\n');

        // changes to default
        wordChars('*', '+');
        //wordChars('!', '!');
        wordChars('<', '?');		// <=>?
        wordChars('-', '/');		// -./
        wordChars(':', ':');		// colon is text (for filenames)
        wordChars('\\', '\\');	// backslash is text (for filenames)
        wordChars('$', '&');		// $%&
        wordChars('~', '~');		// ~
        wordChars('^', '_');		// ^_
//        wordChars('#', '#');		// ^_
        wordChars('{', '}');		// ^_
        wordChars('[', ']');		// ^_
        quoteChar('"');
        commentChar(';');
        ordinaryChar('\'');     // singlequote as token
        ordinaryChar('`');      // singlequote as token
        ordinaryChar('!');
        ordinaryChar('#');
//        ordinaryChar('@');
    }

    public Object nextObject() throws IOException, JlllException
    {
        nextToken();
        switch (ttype)
        {
            case TT_EOF:
                {
                    return null;
                }
            case TT_NUMBER:
                {
                    parseNumbers();
                    return nval;
                }
            case TT_WORD:
                {
                    return parseTTWord();
                }
            case '(':
                {
                    return readList();
                }
            case ')':
                {
                    if (readList)
                    {
                        return ')';
                    }
                    else
                    {
                        throw new IOException("There is overlapen )");
                    }
                }
            case '\'':
                {
                    return ListUtil.list(Symbol.QUOTE, nextObject());
                }
            case '`':
                {
                    return ListUtil.list(Symbol.QUASIQUOTE, nextObject());
                }
            case '@':
                {
                    return "@";
                }
            case ',':
                {
                    Object next = nextObject();
                    if ("@".equals(next))
                    {
                        return ListUtil.list(Symbol.UNQUOTE_SPLICING, nextObject());
                    }
                    else
                    {
                        return ListUtil.list(Symbol.UNQUOTE, next);
                    }
                }
            case '!':
                {
                    return ListUtil.list(Symbol.EXLAMATION,nextObject());
                }
            case '#':
                {
                    return ListUtil.list(Symbol.SHARP,nextObject());
                }
            default:
                {
                    switch (ttype)
                    {
                        case '"':
                            {
//                                System.out.println("invoked");
                                return sval;
                            }
                        default:
                            {
                                return "<" + (char) ttype + "!" + sval + ">";
                            }
                    }
                }
        }
    }

    private Object readList() throws IOException, JlllException
    {
        //Cons ret = new Cons();
        List<Object> ret =  new ArrayList<Object>();
        Object dotted = null;
        
        boolean save = readList;
        readList = true;
        while (true)
        {
            Object next = nextObject();
            if (next instanceof Character && next.equals(')'))
            {
                break;
            }
            else if (next == Symbol.DOT)
            {
                Object cdr = nextObject();
                Object close = nextObject();
                if (!(close instanceof Character) || !close.equals(')'))
                {
                    throw new JlllException("Not a dotted list");
                }
                else
                {
                    //ListUtil.getLastCons(ret).cdr(cdr);
                    dotted = cdr;
                }
                break;
            }
            else if (next == null)
            {
                throw new JlllException("EOF when list isn't closed");
            }
            else
            {
                //ListUtil.append(ret, next);
                ret.add(next);
            }
        }
        readList = save;
        Cons retCons = ListUtil.arrayToCons(ret.toArray(), dotted);
        /*if(dotted!=null)
        {
        	if(retCons.isNull()) retCons = new Cons(null, dotted);
        	else ListUtil.getLastCons(retCons).cdr(dotted);
        }*/
        return retCons;
    }

    private Object parseTTWord() throws JlllException
    {
        String what = sval;
//        System.out.println("What = " + sval);
        if (what.length() == 1 && !isDigitable(sval.charAt(0), false)) return Symbol.intern(what);
        if (isDigitable(what.charAt(0), true))
        {
            if(what.indexOf(".")>=0)
            {
                return Double.valueOf(what);
            }
            else
            {
                try
                {
                    return Integer.valueOf(what);
                }
                catch (NumberFormatException e)
                {
                    return new BigInteger(what);
                }
            }
        }
        else
        {
            what = JlllSymbolNaming.convertFromInToSymbolName(what);
            return Symbol.intern(what);
        }
    }

    private boolean isDigitable(char ch, boolean withSym)
    {
        if (ch >= '0' && ch <= '9') return true;
        if (withSym && (ch == '+' || ch == '-' || ch == '.')) return true;
        return false;
    }

}