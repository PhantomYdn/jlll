package ru.ydn.jlll.libs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Iterator;
import java.util.List;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Eof;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.PlainConsole;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.io.JlllTokenizer;
import ru.ydn.jlll.io.Marshaller;
import ru.ydn.jlll.io.MultiPartFormOutputStream;

/**
 * I/O and networking primitives.
 *
 * <p>
 * Provides input/output and HTTP operations:
 * </p>
 * <ul>
 * <li><b>print:</b> outputs values to stdout or specified writer</li>
 * <li><b>read:</b> reads and parses a JLLL expression from input</li>
 * <li><b>read-line:</b> reads a line as a string</li>
 * <li><b>read-char:</b> reads a single character as a string</li>
 * <li><b>peek-char:</b> peeks at the next character without consuming it</li>
 * <li><b>char-ready?:</b> checks if input is available</li>
 * <li><b>remote-eval:</b> evaluates code on a remote JLLL server</li>
 * <li><b>remote-login:</b> authenticates with a remote JLLL server</li>
 * </ul>
 *
 * <p>
 * Also loads iolib.jlll which provides println and other utilities.
 * </p>
 */
public class IOLib implements Library
{
    /**
     * Evaluates code on a remote server and returns description.
     *
     * @param remoteURL
     *            the remote JLLL server URL
     * @param code
     *            the JLLL code to evaluate
     * @return string description of the result
     * @throws IOException
     *             if network error occurs
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object remoteDescribeEval(URL remoteURL, String code) throws IOException, JlllException
    {
        return remoteEval(remoteURL, code, true, true);
    }

    /**
     * Evaluates source code on a remote JLLL server.
     *
     * @param remoteURL
     *            the remote server URL
     * @param code
     *            the JLLL source code string
     * @return the evaluation result
     * @throws IOException
     *             if network error occurs
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object remoteEval(URL remoteURL, String code) throws IOException, JlllException
    {
        return remoteEval(remoteURL, code, true, false);
    }

    /**
     * Evaluates a serialized object on a remote JLLL server.
     *
     * @param remoteURL
     *            the remote server URL
     * @param code
     *            the object to serialize and evaluate
     * @return the evaluation result
     * @throws IOException
     *             if network error occurs
     * @throws JlllException
     *             if evaluation fails
     */
    public static Object remoteEval(URL remoteURL, Object code) throws IOException, JlllException
    {
        return remoteEval(remoteURL, code, false, false);
    }

    private static Object remoteEval(URL remoteURL, Object code, boolean isSourceCode, boolean justDescribe)
            throws IOException, JlllException
    {
        if (code == null)
            return null;
        URL url = remoteURL;
        if (isSourceCode || justDescribe)
        {
            String path = remoteURL.getPath();
            String query = remoteURL.getQuery();
            String updatedFile = path + (query == null ? "?" : "&") + (isSourceCode ? "type=code&" : "")
                    + (justDescribe ? "rettype=describe" : "");
            url = new URL(url.getProtocol(), url.getHost(), url.getPort(), updatedFile);
        }
        String boundary = MultiPartFormOutputStream.createBoundary();
        HttpURLConnection conn = (HttpURLConnection) MultiPartFormOutputStream.createConnection(url);
        conn.setRequestProperty("Accept", "*/*");
        conn.setRequestProperty("Content-Type", MultiPartFormOutputStream.getContentType(boundary));
        conn.setRequestProperty("Connection", "Keep-Alive");
        conn.setRequestProperty("Cache-Control", "no-cache");
        //         no need to connect cuz getOutputStream() does it
        MultiPartFormOutputStream out = new MultiPartFormOutputStream(conn.getOutputStream(), boundary);
        if (isSourceCode)
        {
            out.writeField("code", code.toString());
        }
        else
        {
            byte[] serialized = Marshaller.marshall(code);
            out.writeFile("eval", "application/x-data", "eval.ser", serialized);
        }
        out.close();
        if (conn.getResponseCode() == 200)
        {
            if (!justDescribe)
            {
                List<?> ret = Marshaller.unmarshall(conn.getInputStream());
                return ret.size() > 0 ? ret.get(0) : null;
            }
            else
            {
                return getReaderAsString(new InputStreamReader(conn.getInputStream(), "UTF8"));
            }
        }
        else
        {
            String in = getReaderAsString(new InputStreamReader(conn.getErrorStream()));
            throw new JlllException(in + "\n---------------------\n");
        }
    }

    /**
     * Reads all content from a Reader into a String.
     *
     * @param reader
     *            the reader to consume
     * @return the complete content as a string
     * @throws RuntimeException
     *             wrapping IOException if read fails
     */
    public static String getReaderAsString(Reader reader)
    {
        char[] buf = new char[1024];
        StringWriter sw = new StringWriter();
        int how = 0;
        try
        {
            while (true)
            {
                how = reader.read(buf);
                if (how < 0)
                    break;
                sw.write(buf, 0, how);
            }
        }
        catch (IOException e)
        {
            throw new RuntimeException("IOException", e);
        }
        return sw.toString();
    }

    /**
     * Gets a BufferedReader from the given object.
     * Supports BufferedReader, Reader, or uses *stdin* from environment.
     *
     * @param portObj
     *            the port object (BufferedReader, Reader, or null for stdin)
     * @param env
     *            the environment to get stdin from if portObj is null
     * @return a BufferedReader for input
     * @throws JlllException
     *             if portObj is not a valid input source
     */
    private static BufferedReader getInputReader(Object portObj, Environment env) throws JlllException
    {
        if (portObj == null)
        {
            // Get reader from console
            Console console = KernelLib.getConsole(env);
            BufferedReader reader = console.getReader();
            if (reader != null)
            {
                return reader;
            }
            throw new JlllException("No input available");
        }
        if (portObj instanceof BufferedReader)
        {
            return (BufferedReader) portObj;
        }
        else if (portObj instanceof Reader)
        {
            return new BufferedReader((Reader) portObj);
        }
        else
        {
            throw new JlllException("Expected a reader/port, got: " + portObj);
        }
    }

    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        // Bind console if not already bound (provides stdin/stdout via Console interface)
        if (env.lookup(Symbol.CONSOLE) == null)
        {
            env.addBinding(Symbol.CONSOLE, new PlainConsole(new PrintWriter(System.out, true),
                    new BufferedReader(new InputStreamReader(System.in))));
        }
        // read - Read and parse JLLL expression
        new Primitive("read", env,
                "Reads and parses a JLLL expression from input. "
                        + "(read) reads from stdin, (read port) reads from specified port. "
                        + "Returns the parsed expression, or EOF object at end of input.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object portObj = values.length() > 0 ? values.get(0) : null;
                    BufferedReader reader = getInputReader(portObj, env);
                    JlllTokenizer tokenizer = new JlllTokenizer(reader);
                    Object result = tokenizer.nextObject();
                    return result == null ? Eof.EOF : result;
                }
                catch (IOException e)
                {
                    throw new JlllException("read: I/O error", e);
                }
            }
        };
        // read-line - Read a line as string
        new Primitive("read-line", env,
                "Reads a line of text as a string. "
                        + "(read-line) reads from stdin, (read-line port) reads from specified port. "
                        + "Returns the line without the newline, or EOF object at end of input.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object portObj = values.length() > 0 ? values.get(0) : null;
                    BufferedReader reader = getInputReader(portObj, env);
                    String line = reader.readLine();
                    return line == null ? Eof.EOF : line;
                }
                catch (IOException e)
                {
                    throw new JlllException("read-line: I/O error", e);
                }
            }
        };
        // read-char - Read single character
        new Primitive("read-char", env,
                "Reads a single character from input. "
                        + "(read-char) reads from stdin, (read-char port) reads from specified port. "
                        + "Returns the character as a single-character string, or EOF object at end of input.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object portObj = values.length() > 0 ? values.get(0) : null;
                    BufferedReader reader = getInputReader(portObj, env);
                    int ch = reader.read();
                    return ch == -1 ? Eof.EOF : String.valueOf((char) ch);
                }
                catch (IOException e)
                {
                    throw new JlllException("read-char: I/O error", e);
                }
            }
        };
        // peek-char - Peek at next character without consuming
        new Primitive("peek-char", env,
                "Peeks at the next character without consuming it. "
                        + "(peek-char) peeks at stdin, (peek-char port) peeks at specified port. "
                        + "Returns the character as a single-character string, or EOF object at end of input.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object portObj = values.length() > 0 ? values.get(0) : null;
                    BufferedReader reader = getInputReader(portObj, env);
                    reader.mark(1);
                    int ch = reader.read();
                    if (ch != -1)
                    {
                        reader.reset();
                    }
                    return ch == -1 ? Eof.EOF : String.valueOf((char) ch);
                }
                catch (IOException e)
                {
                    throw new JlllException("peek-char: I/O error", e);
                }
            }
        };
        // char-ready? - Check if input is available
        new Primitive("char-ready?", env,
                "Checks if input is available without blocking. "
                        + "(char-ready?) checks stdin, (char-ready? port) checks specified port. "
                        + "Returns true if at least one character is available.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object portObj = values.length() > 0 ? values.get(0) : null;
                    BufferedReader reader = getInputReader(portObj, env);
                    return reader.ready();
                }
                catch (IOException e)
                {
                    throw new JlllException("char-ready?: I/O error", e);
                }
            }
        };
        new Primitive("remote-eval", env,
                "Evaluates code on a remote JLLL server. (remote-eval code) or (remote-eval code url). "
                        + "Uses 'remote-host binding if URL not specified.")
        {
            private static final long serialVersionUID = -8955658976965979264L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    Object code = values.get(0);
                    Object hostObj = values.length() > 1 ? values.get(1) : env.lookup("remote-host");
                    if (hostObj == null)
                        throw new JlllException("Remote host is unknown. Please specify it.");
                    URL remoteURL = hostObj instanceof URL ? (URL) hostObj : new URL(hostObj.toString());
                    return remoteEval(remoteURL, code);
                }
                catch (IOException e)
                {
                    throw new JlllException("remote-eval exception", e);
                }
            }
        };
        new Primitive("remote-login", env, "Authenticates with a remote JLLL server. (remote-login username password). "
                + "Uses 'remote-host binding for server URL.")
        {
            private static final long serialVersionUID = 498385309144215955L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    String username = values.get(0).toString();
                    String password = values.get(1).toString();
                    Object hostObj = env.lookup("remote-host");
                    if (hostObj == null)
                        throw new JlllException("Remote host is unknown. Please specify it.");
                    URL remoteURL = hostObj instanceof URL ? (URL) hostObj : new URL(hostObj.toString());
                    String query = "j_username=" + URLEncoder.encode(username, "UTF8") + "&" + "j_password="
                            + URLEncoder.encode(password, "UTF8");
                    URL url = remoteURL;//new URL(remoteURL.getProtocol(), remoteURL.getHost(),remoteURL.getPort(),"/");
                    HttpURLConnection conn = (HttpURLConnection) url.openConnection();
                    conn.setDoOutput(true);
                    conn.setRequestMethod("POST");
                    Writer out = new OutputStreamWriter(conn.getOutputStream());
                    out.write(query);
                    out.close();
                    //System.out.println(conn.getResponseMessage());
                    //System.out.println(conn.getResponseCode());
                    //System.out.println(conn.getHeaderFields());
                    //System.out.println("connURL:"+conn.getURL());
                    //System.out.println("url:"+url);
                    return conn.getURL().equals(url);
                }
                catch (IOException e)
                {
                    throw new JlllException("Remote login faild", e);
                }
            }
        };
        new Primitive("print", env, "Outputs values to the console. (print a b c) prints without newline. "
                + "If a value is a lazy sequence, prints each element as it is realized (streaming output).")
        {
            private static final long serialVersionUID = -92971491929006863L;

            public Object apply(Cons vaCons, Environment env) throws JlllException
            {
                Iterator<?> it = vaCons.iterator();
                Console console = KernelLib.getConsole(env);
                while (it.hasNext())
                {
                    Object value = Evaluator.eval(it.next(), env);
                    printValue(value, console);
                }
                return vaCons;
            }

            /**
             * Prints a value, streaming lazy sequences element by element.
             */
            private void printValue(Object value, Console console)
            {
                if (value instanceof Cons cons && cons.hasLazyCdr())
                {
                    // Stream lazy sequence: print each element as it's realized
                    Cons current = cons;
                    while (current != null && !Null.NULL.equals(current) && !current.isNull())
                    {
                        Object car = current.car();
                        console.print(String.valueOf(car));
                        console.flush(); // Flush after each chunk for real-time streaming
                        Object cdr = current.cdr(); // This forces the lazy thunk
                        if (cdr instanceof Cons)
                        {
                            current = (Cons) cdr;
                        }
                        else if (Null.NULL.equals(cdr) || cdr == null)
                        {
                            break;
                        }
                        else
                        {
                            // Dotted pair - print remaining
                            console.print(String.valueOf(cdr));
                            console.flush();
                            break;
                        }
                    }
                }
                else
                {
                    // Normal print
                    console.print(String.valueOf(value));
                    console.flush();
                }
            }
        };
        Jlll.eval("(load-system-script \"iolib.jlll\")", env);
    }
}
