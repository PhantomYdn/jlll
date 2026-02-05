package ru.ydn.jlll.libs;

import java.io.InputStream;
import java.io.StringReader;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import io.javalin.http.Context;
import io.javalin.http.HandlerType;
import io.javalin.http.sse.SseClient;
import ru.ydn.jlll.common.CapturingConsole;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ParameterParser;
import ru.ydn.jlll.common.ParameterParser.KeywordExtraction;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Web Console library - browser-based REPL for JLLL.
 *
 * <p>
 * Provides a web-based REPL accessible from any browser, featuring:
 * <ul>
 * <li>Syntax highlighting via CodeMirror</li>
 * <li>Auto-complete for symbols</li>
 * <li>Command history (in-memory)</li>
 * <li>SSE streaming for evaluation output</li>
 * </ul>
 *
 * <h3>Usage</h3>
 *
 * <pre>
 * ;; Start web console on default port (8080)
 * (web-console)
 *
 * ;; Custom port
 * (web-console :port 3000)
 *
 * ;; Allow external connections (security warning!)
 * (web-console :bind "0.0.0.0")
 *
 * ;; Get status
 * (web-console :status)  ; =&gt; {:running true :port 8080 :url "..."}
 *
 * ;; Stop
 * (web-console :stop)
 * </pre>
 *
 * <h3>Security</h3>
 * <p>
 * By default, binds to localhost (127.0.0.1) only. External binding requires explicit
 * {@code :bind "0.0.0.0"} and displays a security warning.
 * </p>
 */
public class WebConsoleLib extends ReflectionLibrary
{
    private static JlllHttpServer server;
    private static Environment sharedEnv;
    private static boolean isolated = false;
    private static int currentPort = 8080;
    private static String currentHost = "127.0.0.1";
    private static final ExecutorService executor = Executors.newCachedThreadPool();
    private static final AtomicReference<Future<?>> currentEval = new AtomicReference<>();

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        registerWebConsole(env);
    }

    /**
     * Registers the web-console primitive.
     */
    private void registerWebConsole(Environment env)
    {
        new Primitive("web-console", env,
                "Start browser-based REPL. Options: :port (default 8080), :bind (default \"127.0.0.1\"), "
                        + ":isolated (each connection gets own environment). "
                        + "Use :status to check status, :stop to stop server.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                KeywordExtraction extraction = ParameterParser.extractKeywords(values);
                // Check for :status or :stop keywords
                if (ParameterParser.getBoolean(extraction, "status", false)
                        || extraction.positional.contains(Keyword.intern("status")))
                {
                    return getStatus();
                }
                if (ParameterParser.getBoolean(extraction, "stop", false)
                        || extraction.positional.contains(Keyword.intern("stop")))
                {
                    return stopServer();
                }
                // Start server
                int port = (int) ParameterParser.getLong(extraction, "port", 8080);
                String host = ParameterParser.getString(extraction, "bind", "127.0.0.1");
                boolean iso = ParameterParser.getBoolean(extraction, "isolated", false);
                return startServer(env, port, host, iso);
            }
        };
    }

    /**
     * Starts the web console server.
     */
    private static synchronized Object startServer(Environment env, int port, String host, boolean iso)
            throws JlllException
    {
        if (server != null && server.isRunning())
        {
            Map<String, Object> status = new LinkedHashMap<>();
            status.put("error", "Web console already running");
            status.put("port", currentPort);
            status.put("url",
                    "http://" + (currentHost.equals("0.0.0.0") ? "localhost" : currentHost) + ":" + currentPort);
            return status;
        }
        // Security warning for external binding
        if (!host.equals("127.0.0.1") && !host.equals("localhost"))
        {
            Console console = KernelLib.getConsole(env);
            console.printWarning("WARNING: Web console bound to " + host);
            console.println();
            console.printWarning("  Anyone with network access can execute code!");
            console.println();
            console.printWarning("  Use only in trusted networks or behind a firewall.");
            console.println();
            console.flush();
        }
        // Load web console specific REPL library (help, clear commands)
        new WebConsoleReplLib().load(env);
        sharedEnv = env;
        isolated = iso;
        currentPort = port;
        currentHost = host;
        // Create server
        server = new JlllHttpServer(port, host, null, env);
        // Setup routes
        setupRoutes(server, env);
        // Start
        server.start();
        Console console = KernelLib.getConsole(env);
        String url = "http://" + (host.equals("0.0.0.0") ? "localhost" : host) + ":" + port;
        console.printSuccess("Web console started at " + url);
        console.println();
        console.flush();
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("running", true);
        result.put("port", port);
        result.put("host", host);
        result.put("isolated", isolated);
        result.put("url", url);
        return result;
    }

    /**
     * Stops the web console server.
     */
    private static synchronized Object stopServer()
    {
        if (server == null || !server.isRunning())
        {
            Map<String, Object> status = new LinkedHashMap<>();
            status.put("running", false);
            status.put("message", "Web console is not running");
            return status;
        }
        server.stop();
        server = null;
        Map<String, Object> status = new LinkedHashMap<>();
        status.put("running", false);
        status.put("message", "Web console stopped");
        return status;
    }

    /**
     * Gets the current server status.
     */
    private static Object getStatus()
    {
        Map<String, Object> status = new LinkedHashMap<>();
        if (server != null && server.isRunning())
        {
            status.put("running", true);
            status.put("port", currentPort);
            status.put("host", currentHost);
            status.put("isolated", isolated);
            status.put("url",
                    "http://" + (currentHost.equals("0.0.0.0") ? "localhost" : currentHost) + ":" + currentPort);
        }
        else
        {
            status.put("running", false);
        }
        return status;
    }

    /**
     * Sets up HTTP routes for the web console.
     */
    private static void setupRoutes(JlllHttpServer server, Environment env)
    {
        // Serve the main HTML page
        server.addRoute(HandlerType.GET, "/", createResourceHandler("web-console/web-console.html", "text/html", env));
        // Serve static resources
        server.addRoute(HandlerType.GET, "/web-console.css",
                createResourceHandler("web-console/web-console.css", "text/css", env));
        server.addRoute(HandlerType.GET, "/web-console.js",
                createResourceHandler("web-console/web-console.js", "application/javascript", env));
        server.addRoute(HandlerType.GET, "/codemirror/codemirror.min.css",
                createResourceHandler("web-console/codemirror/codemirror.min.css", "text/css", env));
        server.addRoute(HandlerType.GET, "/codemirror/codemirror.min.js",
                createResourceHandler("web-console/codemirror/codemirror.min.js", "application/javascript", env));
        server.addRoute(HandlerType.GET, "/codemirror/mode/commonlisp.min.js",
                createResourceHandler("web-console/codemirror/mode/commonlisp.min.js", "application/javascript", env));
        // Auto-complete endpoint
        server.addRoute(HandlerType.GET, "/complete", createCompleteHandler(env));
        // Symbols endpoint (for syntax highlighting)
        server.addRoute(HandlerType.GET, "/symbols", createSymbolsHandler(env));
        // Status endpoint
        server.addRoute(HandlerType.GET, "/status", createStatusHandler(env));
        // Interrupt endpoint
        server.addRoute(HandlerType.POST, "/interrupt", createInterruptHandler(env));
        // Eval endpoint (SSE)
        server.addSseEndpoint("/eval", createEvalHandler(env));
    }

    /**
     * Creates a handler that serves a classpath resource.
     */
    private static Procedure createResourceHandler(String resourcePath, String contentType, Environment env)
    {
        return new Primitive("web-console-resource", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing context argument");
                }
                Object ctxObj = values.car();
                if (!(ctxObj instanceof Context))
                {
                    throw new JlllException("Expected Context, got " + ctxObj.getClass().getSimpleName());
                }
                Context ctx = (Context) ctxObj;
                serveResource(ctx, resourcePath, contentType);
                return Null.NULL;
            }
        };
    }

    /**
     * Creates a handler for the /complete endpoint.
     */
    private static Procedure createCompleteHandler(Environment env)
    {
        return new Primitive("web-console-complete", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing context argument");
                }
                Object ctxObj = values.car();
                if (!(ctxObj instanceof Context))
                {
                    throw new JlllException("Expected Context, got " + ctxObj.getClass().getSimpleName());
                }
                Context ctx = (Context) ctxObj;
                String prefix = ctx.queryParam("prefix");
                if (prefix == null)
                {
                    prefix = "";
                }
                Object result = getCompletions(prefix, getEvalEnvironment());
                ctx.json(result);
                return Null.NULL;
            }
        };
    }

    /**
     * Creates a handler for the /symbols endpoint (for syntax highlighting).
     */
    private static Procedure createSymbolsHandler(Environment env)
    {
        return new Primitive("web-console-symbols", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing context argument");
                }
                Object ctxObj = values.car();
                if (!(ctxObj instanceof Context))
                {
                    throw new JlllException("Expected Context, got " + ctxObj.getClass().getSimpleName());
                }
                Context ctx = (Context) ctxObj;
                String type = ctx.queryParam("type");
                if (type == null)
                {
                    type = "all";
                }
                Object result = getSymbols(type, getEvalEnvironment());
                ctx.json(result);
                return Null.NULL;
            }
        };
    }

    /**
     * Creates a handler for the /status endpoint.
     */
    private static Procedure createStatusHandler(Environment env)
    {
        return new Primitive("web-console-status", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing context argument");
                }
                Object ctxObj = values.car();
                if (!(ctxObj instanceof Context))
                {
                    throw new JlllException("Expected Context, got " + ctxObj.getClass().getSimpleName());
                }
                Context ctx = (Context) ctxObj;
                ctx.json(getStatus());
                return Null.NULL;
            }
        };
    }

    /**
     * Creates a handler for the /interrupt endpoint.
     */
    private static Procedure createInterruptHandler(Environment env)
    {
        return new Primitive("web-console-interrupt", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing context argument");
                }
                Object ctxObj = values.car();
                if (!(ctxObj instanceof Context))
                {
                    throw new JlllException("Expected Context, got " + ctxObj.getClass().getSimpleName());
                }
                Context ctx = (Context) ctxObj;
                ctx.json(interruptEval());
                return Null.NULL;
            }
        };
    }

    /**
     * Creates a handler for the SSE /eval endpoint.
     */
    private static Procedure createEvalHandler(Environment env)
    {
        return new Primitive("web-console-eval", env)
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("Missing SseClient argument");
                }
                Object clientObj = values.car();
                if (!(clientObj instanceof SseClient))
                {
                    throw new JlllException("Expected SseClient, got " + clientObj.getClass().getSimpleName());
                }
                SseClient client = (SseClient) clientObj;
                handleEval(client, env);
                return Null.NULL;
            }
        };
    }

    /**
     * Serves a resource from the classpath.
     */
    private static void serveResource(Context ctx, String resourcePath, String contentType) throws JlllException
    {
        try (InputStream is = WebConsoleLib.class.getResourceAsStream("/ru/ydn/jlll/libs/" + resourcePath))
        {
            if (is == null)
            {
                ctx.status(404);
                ctx.result("Resource not found: " + resourcePath);
                return;
            }
            byte[] content = is.readAllBytes();
            ctx.contentType(contentType);
            ctx.result(content);
        }
        catch (Exception e)
        {
            throw new JlllException("Failed to serve resource: " + resourcePath, e);
        }
    }

    /**
     * Gets completions for a prefix.
     */
    private static Object getCompletions(String prefix, Environment env)
    {
        TreeSet<Map<String, String>> completions = new TreeSet<>((a, b) -> a.get("value").compareTo(b.get("value")));
        Map<Symbol, Object> bindings = env.getAllBindings();
        for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
        {
            String name = entry.getKey().getName();
            if (name.startsWith(prefix))
            {
                Map<String, String> completion = new LinkedHashMap<>();
                completion.put("value", name);
                completion.put("description", getDescription(name, entry.getValue(), env));
                completions.add(completion);
            }
        }
        Map<String, Object> result = new LinkedHashMap<>();
        // Return as ArrayList for proper JSON array serialization (not Cons list)
        result.put("completions", new java.util.ArrayList<>(completions));
        return result;
    }

    /**
     * Gets symbols categorized by type for syntax highlighting.
     *
     * @param type
     *            "all" for both callables and variables, "callables" for just callables, "variables"
     *            for just variables
     * @param env
     *            the environment to get symbols from
     * @return map with "callables" and/or "variables" arrays
     */
    private static Object getSymbols(String type, Environment env)
    {
        TreeSet<String> callables = new TreeSet<>();
        TreeSet<String> variables = new TreeSet<>();
        Map<Symbol, Object> bindings = env.getAllBindings();
        for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
        {
            String name = entry.getKey().getName();
            Object value = entry.getValue();
            if (value instanceof Primitive || value instanceof Procedure || value instanceof Macros)
            {
                callables.add(name);
            }
            else if (value != null)
            {
                variables.add(name);
            }
        }
        Map<String, Object> result = new LinkedHashMap<>();
        if ("callables".equals(type))
        {
            result.put("callables", new java.util.ArrayList<>(callables));
        }
        else if ("variables".equals(type))
        {
            result.put("variables", new java.util.ArrayList<>(variables));
        }
        else
        {
            // "all" or default
            result.put("callables", new java.util.ArrayList<>(callables));
            result.put("variables", new java.util.ArrayList<>(variables));
        }
        return result;
    }

    /**
     * Gets a description for a symbol value.
     */
    private static String getDescription(String name, Object value, Environment env)
    {
        // Try to get doc from metadata first
        Symbol symbol = Symbol.intern(name);
        Object doc = env.getMeta(symbol, Symbol.intern("doc"));
        if (doc != null && !doc.toString().isEmpty())
        {
            String docStr = doc.toString();
            if (docStr.length() > 60)
            {
                int period = docStr.indexOf('.');
                if (period > 0 && period < 60)
                {
                    return docStr.substring(0, period + 1);
                }
                int end = docStr.lastIndexOf(' ', 57);
                if (end < 30)
                {
                    end = 57;
                }
                return docStr.substring(0, end) + "...";
            }
            return docStr;
        }
        // Fall back to type-based description
        if (value == null)
        {
            return "unbound";
        }
        else if (value instanceof Primitive)
        {
            return "primitive";
        }
        else if (value instanceof Macros)
        {
            return "macro";
        }
        else if (value instanceof Procedure)
        {
            return "procedure";
        }
        else if (value instanceof Boolean)
        {
            return "boolean: " + value;
        }
        else if (value instanceof Number)
        {
            return "number: " + value;
        }
        else if (value instanceof String)
        {
            return "string";
        }
        else
        {
            return value.getClass().getSimpleName();
        }
    }

    /**
     * Handles SSE evaluation request.
     */
    private static void handleEval(SseClient client, Environment env)
    {
        try
        {
            // Note: keepAlive() is called by JlllHttpServer.applySseEndpoint()
            // Get code from query parameter (sent via POST body is not accessible in SSE)
            // Client sends code as query param: /eval?code=...
            String code = client.ctx().queryParam("code");
            if (code == null || code.trim().isEmpty())
            {
                sendSseEvent(client, "error", Map.of("message", "No code provided"));
                sendSseEvent(client, "done", Map.of());
                return;
            }
            // Get environment for evaluation
            Environment evalEnv = getEvalEnvironment();
            // Create capturing console for output
            // IMPORTANT: We must bind the CapturingConsole to Environment.top because
            // library functions like for-each have lexical environments that were captured
            // when libraries were loaded. Their lookup chain eventually reaches Environment.top,
            // not sharedEnv. So we must modify Environment.top to intercept all console output.
            CapturingConsole captureConsole = new CapturingConsole();
            Console originalConsole = KernelLib.getConsole(Environment.top);
            Environment.top.addBinding(Symbol.CONSOLE, captureConsole);
            // Execute in background thread so we can send output progressively
            Future<?> future = executor.submit(() ->
            {
                try
                {
                    // Evaluate
                    Object result = Jlll.eval(new StringReader(code), evalEnv);
                    // Send any captured output
                    String output = captureConsole.getCapturedOutput();
                    if (!output.isEmpty())
                    {
                        sendSseEvent(client, "output", Map.of("text", output));
                    }
                    // Send result (check for special clear marker)
                    if (result == WebConsoleReplLib.CLEAR_MARKER)
                    {
                        sendSseEvent(client, "clear", Map.of());
                    }
                    else if (result != null && !(result instanceof Null))
                    {
                        // Only send result for non-null values (matching REPL behavior)
                        String resultStr = result.toString();
                        String resultType = result.getClass().getSimpleName();
                        sendSseEvent(client, "result", Map.of("value", resultStr, "type", resultType));
                    }
                    // null/Null results are silently suppressed (like REPL)
                }
                catch (JlllException e)
                {
                    // Send any captured output first
                    String output = captureConsole.getCapturedOutput();
                    if (!output.isEmpty())
                    {
                        sendSseEvent(client, "output", Map.of("text", output));
                    }
                    // Send error
                    String message = e.getMessage();
                    if (message == null)
                    {
                        message = e.getClass().getSimpleName();
                    }
                    sendSseEvent(client, "error", Map.of("message", message, "type", "JlllException"));
                }
                catch (Exception e)
                {
                    String message = e.getMessage();
                    if (message == null)
                    {
                        message = e.getClass().getSimpleName();
                    }
                    sendSseEvent(client, "error", Map.of("message", message, "type", e.getClass().getSimpleName()));
                }
                finally
                {
                    // Restore original console to Environment.top
                    Environment.top.addBinding(Symbol.CONSOLE, originalConsole);
                    // Send done event
                    sendSseEvent(client, "done", Map.of());
                    // Clear current eval reference
                    currentEval.compareAndSet(null, null);
                }
            });
            currentEval.set(future);
        }
        catch (Exception e)
        {
            sendSseEvent(client, "error", Map.of("message", "Failed to process request: " + e.getMessage()));
            sendSseEvent(client, "done", Map.of());
        }
    }

    /**
     * Sends an SSE event.
     */
    private static void sendSseEvent(SseClient client, String event, Map<String, ?> data)
    {
        try
        {
            StringBuilder json = new StringBuilder("{");
            boolean first = true;
            for (Map.Entry<String, ?> entry : data.entrySet())
            {
                if (!first)
                {
                    json.append(",");
                }
                first = false;
                json.append("\"").append(entry.getKey()).append("\":");
                Object val = entry.getValue();
                if (val instanceof String)
                {
                    json.append("\"").append(escapeJson((String) val)).append("\"");
                }
                else if (val instanceof Number || val instanceof Boolean)
                {
                    json.append(val);
                }
                else if (val == null)
                {
                    json.append("null");
                }
                else
                {
                    json.append("\"").append(escapeJson(val.toString())).append("\"");
                }
            }
            json.append("}");
            client.sendEvent(event, json.toString());
        }
        catch (Exception e)
        {
            // Client may have disconnected, ignore
        }
    }

    /**
     * Escapes a string for JSON.
     */
    private static String escapeJson(String s)
    {
        StringBuilder sb = new StringBuilder();
        for (char c : s.toCharArray())
        {
            switch (c)
            {
                case '"' :
                    sb.append("\\\"");
                    break;
                case '\\' :
                    sb.append("\\\\");
                    break;
                case '\b' :
                    sb.append("\\b");
                    break;
                case '\f' :
                    sb.append("\\f");
                    break;
                case '\n' :
                    sb.append("\\n");
                    break;
                case '\r' :
                    sb.append("\\r");
                    break;
                case '\t' :
                    sb.append("\\t");
                    break;
                default :
                    if (c < 0x20)
                    {
                        sb.append(String.format("\\u%04x", (int) c));
                    }
                    else
                    {
                        sb.append(c);
                    }
            }
        }
        return sb.toString();
    }

    /**
     * Gets the environment for evaluation.
     * Returns either the shared environment or a child environment if isolated mode.
     */
    private static Environment getEvalEnvironment()
    {
        if (isolated)
        {
            // Create child environment for isolation
            return new Environment(sharedEnv);
        }
        return sharedEnv;
    }

    /**
     * Interrupts the current evaluation.
     */
    private static Object interruptEval()
    {
        Future<?> future = currentEval.getAndSet(null);
        if (future != null && !future.isDone())
        {
            future.cancel(true);
            return Map.of("interrupted", true);
        }
        return Map.of("interrupted", false, "message", "No evaluation in progress");
    }

    /**
     * Tests if value is a web console server.
     *
     * @param value
     *            the value to test
     * @return true if the server is the web console server
     */
    @JlllName("web-console?")
    public boolean isWebConsole(Object value)
    {
        return value == server;
    }

    /**
     * Tests if web console is running.
     *
     * @return true if the web console server is running
     */
    @JlllName("web-console-running?")
    public boolean isWebConsoleRunning()
    {
        return server != null && server.isRunning();
    }
}
