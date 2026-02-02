package ru.ydn.jlll.libs;

import java.util.LinkedHashMap;
import java.util.Map;
import io.javalin.http.Context;
import io.javalin.http.HandlerType;
import io.javalin.http.HttpStatus;
import io.javalin.http.sse.SseClient;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ParameterParser;
import ru.ydn.jlll.common.ParameterParser.KeywordExtraction;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * HTTP server library using Javalin.
 *
 * <p>
 * Provides primitives for creating and configuring embedded HTTP servers with REST API support,
 * static file serving, before/after filters, error handling, and Server-Sent Events (SSE).
 * </p>
 *
 * <h3>Server Lifecycle</h3>
 * <ul>
 * <li><b>http-server</b> - Create a server instance with options (:port, :host, :context-path)</li>
 * <li><b>http-start</b> - Start the server (can override :port, :host)</li>
 * <li><b>http-stop</b> - Stop the server</li>
 * <li><b>http-restart</b> - Restart the server (stop + start)</li>
 * <li><b>http-server?</b> - Test if value is a server instance</li>
 * <li><b>http-running?</b> - Test if server is currently running</li>
 * </ul>
 *
 * <h3>Route Registration</h3>
 * <ul>
 * <li><b>http-get, http-post, http-put, http-delete, http-patch</b> - Register route handlers</li>
 * <li><b>http-before, http-after</b> - Register filters</li>
 * <li><b>http-error</b> - Register error handlers by status code</li>
 * <li><b>http-exception</b> - Register global exception handler</li>
 * </ul>
 *
 * <h3>Context Accessors (Request)</h3>
 * <ul>
 * <li><b>http-method</b> - Get HTTP method as keyword</li>
 * <li><b>http-path</b> - Get request path</li>
 * <li><b>http-path-param</b> - Get path parameter by name</li>
 * <li><b>http-query-param</b> - Get query parameter by name</li>
 * <li><b>http-query-params</b> - Get all query parameters as hash-map</li>
 * <li><b>http-header</b> - Get request header by name</li>
 * <li><b>http-headers</b> - Get all headers as hash-map</li>
 * <li><b>http-body</b> - Get request body as string</li>
 * <li><b>http-body-json</b> - Get request body parsed as JSON</li>
 * <li><b>http-remote-addr</b> - Get client IP address</li>
 * </ul>
 *
 * <h3>Context Setters (Response)</h3>
 * <ul>
 * <li><b>http-result</b> - Set response body</li>
 * <li><b>http-json</b> - Set JSON response</li>
 * <li><b>http-html</b> - Set HTML response</li>
 * <li><b>http-status</b> - Set HTTP status code</li>
 * <li><b>http-header!</b> - Set response header</li>
 * <li><b>http-redirect</b> - Redirect to URL</li>
 * </ul>
 *
 * <h3>Static Files and SSE</h3>
 * <ul>
 * <li><b>http-static</b> - Serve static files from directory</li>
 * <li><b>http-sse</b> - Server-Sent Events endpoint</li>
 * <li><b>sse-send</b> - Send SSE event to client</li>
 * <li><b>sse-close</b> - Close SSE connection</li>
 * </ul>
 *
 * <h3>Example Usage</h3>
 *
 * <pre>
 * (define server (http-server :port 8080))
 *
 * (http-get server "/hello"
 *   (lambda (ctx) (http-result ctx "Hello, World!")))
 *
 * (http-get server "/users/{id}"
 *   (lambda (ctx)
 *     (define id (http-path-param ctx "id"))
 *     (http-json ctx (hash-map :id id :name "User"))))
 *
 * (http-start server)
 * </pre>
 */
public class HttpLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // Server lifecycle
        registerHttpServer(env);
        registerHttpStart(env);
        registerHttpStop(env);
        registerHttpRestart(env);
        // Route registration
        registerRouteHandler(env, "http-get", HandlerType.GET);
        registerRouteHandler(env, "http-post", HandlerType.POST);
        registerRouteHandler(env, "http-put", HandlerType.PUT);
        registerRouteHandler(env, "http-delete", HandlerType.DELETE);
        registerRouteHandler(env, "http-patch", HandlerType.PATCH);
        registerRouteHandler(env, "http-head", HandlerType.HEAD);
        registerRouteHandler(env, "http-options", HandlerType.OPTIONS);
        // Filters
        registerBeforeAfter(env, "http-before", true);
        registerBeforeAfter(env, "http-after", false);
        // Error handling
        registerHttpError(env);
        registerHttpException(env);
        // Context accessors
        registerContextAccessors(env);
        // Context setters
        registerContextSetters(env);
        // Static files
        registerHttpStatic(env);
        // SSE
        registerHttpSse(env);
        registerSseSend(env);
        registerSseClose(env);
        registerSseKeepAlive(env);
        // Load JLLL wrappers
        Jlll.eval("(load-system-script \"http.jlll\")", env);
    }

    /**
     * Registers http-server primitive.
     */
    private void registerHttpServer(Environment env)
    {
        new Primitive("http-server", env,
                "Create HTTP server. Options: :port (default 8080), :host (default \"0.0.0.0\"), "
                        + ":context-path (base path for all routes). Returns server instance.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                KeywordExtraction extraction = ParameterParser.extractKeywords(values);
                int port = (int) ParameterParser.getLong(extraction, "port", 8080L);
                String host = ParameterParser.getString(extraction, "host", "0.0.0.0");
                String contextPath = ParameterParser.getString(extraction, "context-path", null);
                return new JlllHttpServer(port, host, contextPath, env);
            }
        };
    }

    /**
     * Registers http-start primitive.
     */
    private void registerHttpStart(Environment env)
    {
        new Primitive("http-start", env,
                "Start HTTP server. Options: :port (override configured port), :host (override configured host). "
                        + "Returns the server instance. If start fails (e.g., port in use), you can retry.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                JlllHttpServer server = requireServer(values, "http-start");
                // Extract keyword arguments from remaining args
                Cons rest = values.cdr() instanceof Cons ? (Cons) values.cdr() : null;
                KeywordExtraction extraction = ParameterParser.extractKeywords(rest);
                if (ParameterParser.hasKeyword(extraction, "port") || ParameterParser.hasKeyword(extraction, "host"))
                {
                    int port = (int) ParameterParser.getLong(extraction, "port", server.getPort());
                    String host = ParameterParser.getString(extraction, "host", server.getHost());
                    server.start(port, host);
                }
                else
                {
                    server.start();
                }
                return server;
            }
        };
    }

    /**
     * Registers http-stop primitive.
     */
    private void registerHttpStop(Environment env)
    {
        new Primitive("http-stop", env, "Stop HTTP server. Returns null.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                JlllHttpServer server = requireServer(values, "http-stop");
                server.stop();
                return Null.NULL;
            }
        };
    }

    /**
     * Registers http-restart primitive.
     */
    private void registerHttpRestart(Environment env)
    {
        new Primitive("http-restart", env, "Restart HTTP server (stop + start). Returns the server instance.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                JlllHttpServer server = requireServer(values, "http-restart");
                server.restart();
                return server;
            }
        };
    }

    /**
     * Registers a route handler primitive (get, post, put, delete, patch, head, options).
     */
    private void registerRouteHandler(Environment env, String name, HandlerType type)
    {
        new Primitive(name, env,
                "Register " + type.name() + " route handler. Usage: (" + name + " server path handler)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 3)
                {
                    throw new JlllException(name + ": requires server, path, and handler");
                }
                JlllHttpServer server = requireServer(values, name);
                Object pathObj = values.get(1);
                Object handler = values.get(2);
                if (!(pathObj instanceof String))
                {
                    throw new JlllException(name + ": path must be a string");
                }
                if (!(handler instanceof Procedure))
                {
                    throw new JlllException(name + ": handler must be a procedure");
                }
                server.addRoute(type, (String) pathObj, (Procedure) handler);
                return server;
            }
        };
    }

    /**
     * Registers before/after filter primitives.
     */
    private void registerBeforeAfter(Environment env, String name, boolean isBefore)
    {
        new Primitive(name, env, "Register " + (isBefore ? "before" : "after") + " filter. " + "Usage: (" + name
                + " server handler) or (" + name + " server path handler)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException(name + ": requires server and handler");
                }
                JlllHttpServer server = requireServer(values, name);
                int len = values.length();
                if (len == 2)
                {
                    // (http-before server handler)
                    Object handler = values.get(1);
                    if (!(handler instanceof Procedure))
                    {
                        throw new JlllException(name + ": handler must be a procedure");
                    }
                    if (isBefore)
                    {
                        server.addBeforeFilter(null, (Procedure) handler);
                    }
                    else
                    {
                        server.addAfterFilter(null, (Procedure) handler);
                    }
                }
                else
                {
                    // (http-before server path handler)
                    Object pathObj = values.get(1);
                    Object handler = values.get(2);
                    if (!(pathObj instanceof String))
                    {
                        throw new JlllException(name + ": path must be a string");
                    }
                    if (!(handler instanceof Procedure))
                    {
                        throw new JlllException(name + ": handler must be a procedure");
                    }
                    if (isBefore)
                    {
                        server.addBeforeFilter((String) pathObj, (Procedure) handler);
                    }
                    else
                    {
                        server.addAfterFilter((String) pathObj, (Procedure) handler);
                    }
                }
                return server;
            }
        };
    }

    /**
     * Registers http-error primitive.
     */
    private void registerHttpError(Environment env)
    {
        new Primitive("http-error", env,
                "Register error handler for HTTP status code. Usage: (http-error server status handler)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 3)
                {
                    throw new JlllException("http-error: requires server, status code, and handler");
                }
                JlllHttpServer server = requireServer(values, "http-error");
                Object statusObj = values.get(1);
                Object handler = values.get(2);
                if (!(statusObj instanceof Number))
                {
                    throw new JlllException("http-error: status must be a number");
                }
                if (!(handler instanceof Procedure))
                {
                    throw new JlllException("http-error: handler must be a procedure");
                }
                server.addErrorHandler(((Number) statusObj).intValue(), (Procedure) handler);
                return server;
            }
        };
    }

    /**
     * Registers http-exception primitive.
     */
    private void registerHttpException(Environment env)
    {
        new Primitive("http-exception", env,
                "Register global exception handler. Usage: (http-exception server handler). "
                        + "Handler receives (ctx exception-message).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-exception: requires server and handler");
                }
                JlllHttpServer server = requireServer(values, "http-exception");
                Object handler = values.get(1);
                if (!(handler instanceof Procedure))
                {
                    throw new JlllException("http-exception: handler must be a procedure");
                }
                server.setExceptionHandler((Procedure) handler);
                return server;
            }
        };
    }

    /**
     * Registers context accessor primitives.
     */
    private void registerContextAccessors(Environment env)
    {
        // http-method
        new Primitive("http-method", env, "Get HTTP method as keyword (:get, :post, etc.).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-method");
                return Keyword.intern(ctx.method().name().toLowerCase());
            }
        };
        // http-path
        new Primitive("http-path", env, "Get request path.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-path");
                return ctx.path();
            }
        };
        // http-path-param
        new Primitive("http-path-param", env, "Get path parameter by name. Usage: (http-path-param ctx \"id\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-path-param: requires ctx and param name");
                }
                Context ctx = requireContext(values, "http-path-param");
                Object nameObj = values.get(1);
                String name = getParamName(nameObj, "http-path-param");
                return ctx.pathParam(name);
            }
        };
        // http-query-param
        new Primitive("http-query-param", env,
                "Get query parameter by name. Returns null if not present. " + "Usage: (http-query-param ctx \"q\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-query-param: requires ctx and param name");
                }
                Context ctx = requireContext(values, "http-query-param");
                Object nameObj = values.get(1);
                String name = getParamName(nameObj, "http-query-param");
                String value = ctx.queryParam(name);
                return value != null ? value : Null.NULL;
            }
        };
        // http-query-params
        new Primitive("http-query-params", env, "Get all query parameters as hash-map.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-query-params");
                Map<Object, Object> result = new LinkedHashMap<>();
                for (Map.Entry<String, java.util.List<String>> entry : ctx.queryParamMap().entrySet())
                {
                    String k = entry.getKey();
                    java.util.List<String> v = entry.getValue();
                    if (v.size() == 1)
                    {
                        result.put(Keyword.intern(k), v.get(0));
                    }
                    else
                    {
                        result.put(Keyword.intern(k), ListUtil.listToCons(v));
                    }
                }
                return result;
            }
        };
        // http-header
        new Primitive("http-header", env,
                "Get request header by name. Returns null if not present. " + "Usage: (http-header ctx \"Accept\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-header: requires ctx and header name");
                }
                Context ctx = requireContext(values, "http-header");
                Object nameObj = values.get(1);
                String name = getParamName(nameObj, "http-header");
                String value = ctx.header(name);
                return value != null ? value : Null.NULL;
            }
        };
        // http-headers
        new Primitive("http-headers", env, "Get all request headers as hash-map.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-headers");
                Map<Object, Object> result = new LinkedHashMap<>();
                for (Map.Entry<String, String> entry : ctx.headerMap().entrySet())
                {
                    result.put(Keyword.intern(entry.getKey().toLowerCase()), entry.getValue());
                }
                return result;
            }
        };
        // http-body
        new Primitive("http-body", env, "Get request body as string.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-body");
                return ctx.body();
            }
        };
        // http-body-json
        new Primitive("http-body-json", env, "Get request body parsed as JSON (hash-map or list).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-body-json");
                String body = ctx.body();
                if (body == null || body.isEmpty())
                {
                    return Null.NULL;
                }
                try
                {
                    return Jlll.eval("(json-parse \"" + escapeJsonString(body) + "\")", env);
                }
                catch (JlllException e)
                {
                    throw new JlllException("http-body-json: invalid JSON - " + e.getMessage());
                }
            }
        };
        // http-remote-addr
        new Primitive("http-remote-addr", env, "Get client IP address.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-remote-addr");
                return ctx.ip();
            }
        };
        // http-matched-path
        new Primitive("http-matched-path", env, "Get the path pattern that matched (e.g., \"/users/{id}\").")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Context ctx = requireContext(values, "http-matched-path");
                return ctx.matchedPath();
            }
        };
    }

    /**
     * Registers context setter primitives.
     */
    private void registerContextSetters(Environment env)
    {
        // http-result
        new Primitive("http-result", env, "Set response body. Usage: (http-result ctx data)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-result: requires ctx and data");
                }
                Context ctx = requireContext(values, "http-result");
                Object data = values.get(1);
                ctx.result(data.toString());
                return ctx;
            }
        };
        // http-json
        new Primitive("http-json", env, "Set JSON response with Content-Type. Usage: (http-json ctx data)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-json: requires ctx and data");
                }
                Context ctx = requireContext(values, "http-json");
                Object data = values.get(1);
                try
                {
                    // Use JsonLib's stringify
                    Object json = Jlll.invokeProcedure("json-stringify", env, data);
                    ctx.contentType("application/json").result(json.toString());
                }
                catch (JlllException e)
                {
                    throw new JlllException("http-json: failed to serialize - " + e.getMessage());
                }
                return ctx;
            }
        };
        // http-html
        new Primitive("http-html", env, "Set HTML response with Content-Type. Usage: (http-html ctx html)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-html: requires ctx and html");
                }
                Context ctx = requireContext(values, "http-html");
                Object html = values.get(1);
                ctx.html(html.toString());
                return ctx;
            }
        };
        // http-status
        new Primitive("http-status", env,
                "Set or get HTTP status code. Usage: (http-status ctx) or (http-status ctx 404)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("http-status: requires ctx");
                }
                Context ctx = requireContext(values, "http-status");
                if (values.length() == 1)
                {
                    return ctx.status().getCode();
                }
                Object statusObj = values.get(1);
                if (!(statusObj instanceof Number))
                {
                    throw new JlllException("http-status: status must be a number");
                }
                ctx.status(HttpStatus.forStatus(((Number) statusObj).intValue()));
                return ctx;
            }
        };
        // http-header!
        new Primitive("http-header!", env, "Set response header. Usage: (http-header! ctx \"X-Custom\" \"value\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 3)
                {
                    throw new JlllException("http-header!: requires ctx, name, and value");
                }
                Context ctx = requireContext(values, "http-header!");
                Object nameObj = values.get(1);
                Object valueObj = values.get(2);
                String name = getParamName(nameObj, "http-header!");
                ctx.header(name, valueObj.toString());
                return ctx;
            }
        };
        // http-redirect
        new Primitive("http-redirect", env,
                "Redirect to URL. Usage: (http-redirect ctx \"/login\") or (http-redirect ctx \"/new\" 301)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-redirect: requires ctx and url");
                }
                Context ctx = requireContext(values, "http-redirect");
                Object urlObj = values.get(1);
                String url = urlObj.toString();
                if (values.length() >= 3)
                {
                    Object statusObj = values.get(2);
                    if (!(statusObj instanceof Number))
                    {
                        throw new JlllException("http-redirect: status must be a number");
                    }
                    ctx.redirect(url, HttpStatus.forStatus(((Number) statusObj).intValue()));
                }
                else
                {
                    ctx.redirect(url);
                }
                return ctx;
            }
        };
        // http-content-type
        new Primitive("http-content-type", env,
                "Set response Content-Type. Usage: (http-content-type ctx \"text/plain\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("http-content-type: requires ctx and content type");
                }
                Context ctx = requireContext(values, "http-content-type");
                Object typeObj = values.get(1);
                ctx.contentType(typeObj.toString());
                return ctx;
            }
        };
    }

    /**
     * Registers http-static primitive.
     */
    private void registerHttpStatic(Environment env)
    {
        new Primitive("http-static", env, "Serve static files. Usage: (http-static server url-path directory). "
                + "Example: (http-static server \"/assets\" \"./public/\")")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 3)
                {
                    throw new JlllException("http-static: requires server, url-path, and directory");
                }
                JlllHttpServer server = requireServer(values, "http-static");
                Object pathObj = values.get(1);
                Object dirObj = values.get(2);
                if (!(pathObj instanceof String))
                {
                    throw new JlllException("http-static: url-path must be a string");
                }
                if (!(dirObj instanceof String))
                {
                    throw new JlllException("http-static: directory must be a string");
                }
                server.addStaticFiles((String) pathObj, (String) dirObj);
                return server;
            }
        };
    }

    /**
     * Registers http-sse primitive for Server-Sent Events.
     */
    private void registerHttpSse(Environment env)
    {
        new Primitive("http-sse", env, "Register SSE endpoint. Usage: (http-sse server path handler). "
                + "Handler receives sse-client. Use sse-send to send events, sse-close to close.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 3)
                {
                    throw new JlllException("http-sse: requires server, path, and handler");
                }
                JlllHttpServer server = requireServer(values, "http-sse");
                Object pathObj = values.get(1);
                Object handler = values.get(2);
                if (!(pathObj instanceof String))
                {
                    throw new JlllException("http-sse: path must be a string");
                }
                if (!(handler instanceof Procedure))
                {
                    throw new JlllException("http-sse: handler must be a procedure");
                }
                server.addSseEndpoint((String) pathObj, (Procedure) handler);
                return server;
            }
        };
    }

    /**
     * Registers sse-send primitive.
     */
    private void registerSseSend(Environment env)
    {
        new Primitive("sse-send", env,
                "Send SSE event. Usage: (sse-send client data) or (sse-send client event-name data) "
                        + "or (sse-send client event-name data id)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull() || values.length() < 2)
                {
                    throw new JlllException("sse-send: requires client and data");
                }
                Object clientObj = values.car();
                if (!(clientObj instanceof SseClient))
                {
                    throw new JlllException("sse-send: expected sse-client, got "
                            + (clientObj == null ? "null" : clientObj.getClass().getSimpleName()));
                }
                SseClient client = (SseClient) clientObj;
                int len = values.length();
                if (len == 2)
                {
                    // (sse-send client data)
                    Object data = values.get(1);
                    client.sendEvent(data.toString());
                }
                else if (len == 3)
                {
                    // (sse-send client event-name data)
                    Object eventObj = values.get(1);
                    Object data = values.get(2);
                    String event = getParamName(eventObj, "sse-send");
                    client.sendEvent(event, data.toString());
                }
                else
                {
                    // (sse-send client event-name data id)
                    Object eventObj = values.get(1);
                    Object data = values.get(2);
                    Object idObj = values.get(3);
                    String event = getParamName(eventObj, "sse-send");
                    client.sendEvent(event, data.toString(), idObj.toString());
                }
                return Null.NULL;
            }
        };
    }

    /**
     * Registers sse-close primitive.
     */
    private void registerSseClose(Environment env)
    {
        new Primitive("sse-close", env, "Close SSE connection. Usage: (sse-close client)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("sse-close: requires client");
                }
                Object clientObj = values.car();
                if (!(clientObj instanceof SseClient))
                {
                    throw new JlllException("sse-close: expected sse-client, got "
                            + (clientObj == null ? "null" : clientObj.getClass().getSimpleName()));
                }
                ((SseClient) clientObj).close();
                return Null.NULL;
            }
        };
    }

    /**
     * Registers sse-keep-alive primitive.
     */
    private void registerSseKeepAlive(Environment env)
    {
        new Primitive("sse-keep-alive", env,
                "Enable automatic keep-alive pings for SSE client. Usage: (sse-keep-alive client)")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("sse-keep-alive: requires client");
                }
                Object clientObj = values.car();
                if (!(clientObj instanceof SseClient))
                {
                    throw new JlllException("sse-keep-alive: expected sse-client, got "
                            + (clientObj == null ? "null" : clientObj.getClass().getSimpleName()));
                }
                ((SseClient) clientObj).keepAlive();
                return Null.NULL;
            }
        };
    }
    // =============================================
    // Predicates
    // =============================================

    /**
     * Test if value is an HTTP server instance.
     *
     * @param value
     *            the value to test
     * @return true if value is a JlllHttpServer instance
     */
    @JlllName("http-server?")
    public boolean isHttpServer(Object value)
    {
        return value instanceof JlllHttpServer;
    }

    /**
     * Test if HTTP server is currently running.
     *
     * @param value
     *            the value to test
     * @return true if value is a running JlllHttpServer
     */
    @JlllName("http-running?")
    public boolean isHttpRunning(Object value)
    {
        return value instanceof JlllHttpServer && ((JlllHttpServer) value).isRunning();
    }

    /**
     * Test if value is an SSE client.
     *
     * @param value
     *            the value to test
     * @return true if value is an SseClient
     */
    @JlllName("sse-client?")
    public boolean isSseClient(Object value)
    {
        return value instanceof SseClient;
    }

    /**
     * Test if value is an HTTP context.
     *
     * @param value
     *            the value to test
     * @return true if value is a Javalin Context
     */
    @JlllName("http-context?")
    public boolean isHttpContext(Object value)
    {
        return value instanceof Context;
    }
    // =============================================
    // Helper methods
    // =============================================

    /**
     * Extracts server from values, throwing if not a JlllHttpServer.
     */
    private JlllHttpServer requireServer(Cons values, String funcName) throws JlllException
    {
        if (values == null || values.isNull())
        {
            throw new JlllException(funcName + ": server required");
        }
        Object server = values.car();
        if (!(server instanceof JlllHttpServer))
        {
            throw new JlllException(funcName + ": expected server, got "
                    + (server == null ? "null" : server.getClass().getSimpleName()));
        }
        return (JlllHttpServer) server;
    }

    /**
     * Extracts context from values, throwing if not a Context.
     */
    private Context requireContext(Cons values, String funcName) throws JlllException
    {
        if (values == null || values.isNull())
        {
            throw new JlllException(funcName + ": requires ctx");
        }
        Object ctxObj = values.car();
        if (!(ctxObj instanceof Context))
        {
            throw new JlllException(funcName + ": expected http context, got "
                    + (ctxObj == null ? "null" : ctxObj.getClass().getSimpleName()));
        }
        return (Context) ctxObj;
    }

    /**
     * Gets parameter name from string, symbol, or keyword.
     */
    private String getParamName(Object nameObj, String funcName) throws JlllException
    {
        if (nameObj instanceof String)
        {
            return (String) nameObj;
        }
        if (nameObj instanceof Symbol)
        {
            return ((Symbol) nameObj).getName();
        }
        if (nameObj instanceof Keyword)
        {
            return ((Keyword) nameObj).toSymbol().getName();
        }
        throw new JlllException(funcName + ": name must be string, symbol, or keyword");
    }

    /**
     * Escapes a string for use in a JLLL string literal.
     */
    private String escapeJsonString(String s)
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
                    sb.append(c);
            }
        }
        return sb.toString();
    }
    // =============================================
    // Gson JSON Mapper for Javalin
    // =============================================

    /**
     * Gson-based JSON mapper for Javalin to maintain consistency with JsonLib.
     */
    static class GsonJsonMapper implements io.javalin.json.JsonMapper
    {
        private final com.google.gson.Gson gson = new com.google.gson.Gson();

        @Override
        public String toJsonString(Object obj, java.lang.reflect.Type type)
        {
            return gson.toJson(obj, type);
        }

        @Override
        public <T> T fromJsonString(String json, java.lang.reflect.Type type)
        {
            return gson.fromJson(json, type);
        }
    }
}
