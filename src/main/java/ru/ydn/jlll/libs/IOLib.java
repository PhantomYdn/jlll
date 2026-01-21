package ru.ydn.jlll.libs;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
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
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Symbol;
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

    /** {@inheritDoc} */
    public void load(Enviroment env) throws JlllException
    {
        env.addBinding(Symbol.STDIN, new InputStreamReader(System.in));
        env.addBinding(Symbol.STDOUT, new PrintWriter(System.out));
        new Primitive("remote-eval", env,
                "Evaluates code on a remote JLLL server. (remote-eval code) or (remote-eval code url). "
                        + "Uses 'remote-host binding if URL not specified.")
        {
            private static final long serialVersionUID = -8955658976965979264L;

            @Override
            public Object applyEvaluated(Cons values, Enviroment env) throws JlllException
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
            public Object applyEvaluated(Cons values, Enviroment env) throws JlllException
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
        new Primitive("print", env,
                "Outputs values to stdout (or *stdout* binding). (print a b c) prints without newline.")
        {
            private static final long serialVersionUID = -92971491929006863L;

            public Object apply(Cons vaCons, Enviroment env) throws JlllException
            {
                Iterator<?> it = vaCons.iterator();
                Object outObject = env.lookup(Symbol.STDOUT);
                PrintWriter out = null;
                if (outObject instanceof PrintWriter)
                {
                    out = (PrintWriter) outObject;
                }
                else if (outObject instanceof Writer)
                {
                    out = new PrintWriter((Writer) outObject);
                }
                else if (outObject instanceof OutputStream)
                {
                    out = new PrintWriter((OutputStream) outObject);
                }
                else
                {
                    out = new PrintWriter(System.out);
                }
                while (it.hasNext())
                {
                    out.print(Evaluator.eval(it.next(), env));
                }
                //                System.out.println();
                return vaCons;
            }
        };
        Jlll.eval("(load-system-script \"iolib.jlll\")", env);
    }
}
