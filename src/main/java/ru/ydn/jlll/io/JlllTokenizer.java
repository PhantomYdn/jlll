package ru.ydn.jlll.io;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.util.ListUtil;

/**
 * Lexer for JLLL source code.
 * Extends {@link StreamTokenizer} to recognize Lisp syntax elements including
 * parentheses, quotes, symbols, numbers, strings, and comments.
 */
public class JlllTokenizer extends StreamTokenizer
{
    private boolean readList = false;

    /**
     * Creates a tokenizer for the given input.
     *
     * @param reader
     *            the source to tokenize
     */
    public JlllTokenizer(Reader reader)
    {
        super(reader);
        setSyntax();
    }

    /**
     * Configures the tokenizer syntax for JLLL.
     */
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
        wordChars('<', '?'); // <=>?
        wordChars('-', '/'); // -./
        // Note: colon is now ordinary char to support keywords (:foo)
        ordinaryChar(':');
        wordChars('\\', '\\'); // backslash is text (for filenames)
        wordChars('$', '&'); // $%&
        wordChars('~', '~'); // ~
        wordChars('^', '_'); // ^_
        //        wordChars('#', '#');		// ^_
        wordChars('{', '}'); // ^_
        wordChars('[', ']'); // ^_
        wordChars('!', '!'); // Allow ! in symbols like set!
        quoteChar('"');
        commentChar(';');
        ordinaryChar('\''); // singlequote as token
        ordinaryChar('`'); // singlequote as token
        ordinaryChar('#');
        //        ordinaryChar('@');
    }

    /**
     * Reads and returns the next JLLL object from the input.
     * Handles atoms (symbols, numbers, strings) and compound structures (lists).
     *
     * @return the next parsed object, or null at end of input
     * @throws IOException
     *             if reading fails
     * @throws JlllException
     *             if syntax is invalid
     */
    public Object nextObject() throws IOException, JlllException
    {
        nextToken();
        switch (ttype)
        {
            case TT_EOF : {
                return null;
            }
            case TT_NUMBER : {
                parseNumbers();
                return nval;
            }
            case TT_WORD : {
                // Check for ! prefix as reader macro (e.g., !expr becomes (! expr))
                // But symbols ending in ! like set! should remain as-is
                if (sval.startsWith("!") && sval.length() > 1)
                {
                    // Parse the rest as a separate token
                    String rest = sval.substring(1);
                    sval = rest;
                    return ListUtil.list(Symbol.EXCLAMATION, parseTTWord());
                }
                else if (sval.equals("!"))
                {
                    // Just "!" by itself - read next object
                    return ListUtil.list(Symbol.EXCLAMATION, nextObject());
                }
                return parseTTWord();
            }
            case '(' : {
                return readList();
            }
            case ')' : {
                if (readList)
                {
                    return ')';
                }
                else
                {
                    throw new IOException("There is overlapen )");
                }
            }
            case '\'' : {
                return ListUtil.list(Symbol.QUOTE, nextObject());
            }
            case '`' : {
                return ListUtil.list(Symbol.QUASIQUOTE, nextObject());
            }
            case '@' : {
                return "@";
            }
            case ',' : {
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
            case '#' : {
                return ListUtil.list(Symbol.SHARP, nextObject());
            }
            case ':' : {
                return parseKeyword();
            }
            default : {
                switch (ttype)
                {
                    case '"' : {
                        //                                System.out.println("invoked");
                        return sval;
                    }
                    default : {
                        return "<" + (char) ttype + "!" + sval + ">";
                    }
                }
            }
        }
    }

    private Object readList() throws IOException, JlllException
    {
        //Cons ret = new Cons();
        List<Object> ret = new ArrayList<Object>();
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
        /*
         * if(dotted!=null)
         * {
         * if(retCons.isNull()) retCons = new Cons(null, dotted);
         * else ListUtil.getLastCons(retCons).cdr(dotted);
         * }
         */
        return retCons;
    }

    private Object parseTTWord() throws JlllException
    {
        String what = sval;
        //        System.out.println("What = " + sval);
        if (what.length() == 1 && !isDigitable(sval.charAt(0), false))
            return Symbol.intern(what);
        if (isDigitable(what.charAt(0), true))
        {
            if (what.indexOf(".") >= 0)
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
        if (ch >= '0' && ch <= '9')
            return true;
        if (withSym && (ch == '+' || ch == '-' || ch == '.'))
            return true;
        return false;
    }

    /**
     * Parses a keyword token starting with colon.
     * Called after ':' has been read.
     *
     * @return the Keyword object
     * @throws IOException
     *             if reading fails
     * @throws JlllException
     *             if keyword syntax is invalid
     */
    private Object parseKeyword() throws IOException, JlllException
    {
        nextToken();
        if (ttype == ':')
        {
            throw new JlllException("Invalid keyword syntax: double colon not allowed");
        }
        if (ttype != TT_WORD)
        {
            throw new JlllException("Invalid keyword: expected name after colon, got " + tokenDescription());
        }
        String name = sval;
        // Apply same naming conversion as symbols
        name = JlllSymbolNaming.convertFromInToSymbolName(name);
        return Keyword.intern(name);
    }

    /**
     * Returns a description of the current token for error messages.
     */
    private String tokenDescription()
    {
        if (ttype == TT_EOF)
            return "end of file";
        if (ttype == TT_WORD)
            return "word '" + sval + "'";
        if (ttype == TT_NUMBER)
            return "number " + nval;
        return "character '" + (char) ttype + "'";
    }
}
