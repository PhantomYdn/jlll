package ru.ydn.jlll.libs;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import io.javalin.Javalin;
import io.javalin.config.Key;
import io.javalin.http.Handler;
import io.javalin.http.HandlerType;
import io.javalin.http.sse.SseClient;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Procedure;

/**
 * Wrapper around Javalin server that allows restart after failed start.
 *
 * <p>
 * This class stores server configuration and route registrations separately from the actual Javalin
 * instance. When {@link #start()} is called, a fresh Javalin instance is created and all stored
 * registrations are applied. This allows retrying start after a failure (e.g., port already in
 * use).
 * </p>
 *
 * <p>
 * Routes can be added both before and after the server is started. If added while running, they are
 * immediately applied to the active Javalin instance.
 * </p>
 */
public class JlllHttpServer implements Serializable
{
    private static final long serialVersionUID = 1L;
    /** Key for storing environment in Javalin app data. */
    static final Key<Environment> ENV_KEY = new Key<>("jlll-env");
    // Configuration
    private int port;
    private String host;
    private String contextPath;
    private final Environment env;
    // Stored registrations (survive restart)
    private final List<RouteRegistration> routes = new ArrayList<>();
    private final List<FilterRegistration> beforeFilters = new ArrayList<>();
    private final List<FilterRegistration> afterFilters = new ArrayList<>();
    private final List<StaticFilesRegistration> staticFiles = new ArrayList<>();
    private final List<SseRegistration> sseEndpoints = new ArrayList<>();
    private final Map<Integer, Procedure> errorHandlers = new LinkedHashMap<>();
    private Procedure exceptionHandler = null;
    // Active Javalin instance (null when stopped)
    private transient Javalin app = null;

    /**
     * Creates a new HTTP server wrapper.
     *
     * @param port
     *            the port to listen on
     * @param host
     *            the host/interface to bind to
     * @param contextPath
     *            optional base path for all routes (may be null)
     * @param env
     *            the JLLL environment
     */
    public JlllHttpServer(int port, String host, String contextPath, Environment env)
    {
        this.port = port;
        this.host = host;
        this.contextPath = contextPath;
        this.env = env;
    }
    // =============================================
    // Configuration Getters
    // =============================================

    /**
     * Gets the configured port.
     *
     * @return the port number
     */
    public int getPort()
    {
        return port;
    }

    /**
     * Gets the configured host.
     *
     * @return the host/interface
     */
    public String getHost()
    {
        return host;
    }

    /**
     * Gets the context path.
     *
     * @return the context path or null
     */
    public String getContextPath()
    {
        return contextPath;
    }

    /**
     * Gets the underlying Javalin instance if running.
     *
     * @return the Javalin instance or null if not running
     */
    public Javalin getJavalin()
    {
        return app;
    }
    // =============================================
    // Route Registration
    // =============================================

    /**
     * Adds a route handler.
     *
     * @param type
     *            the HTTP method type
     * @param path
     *            the route path
     * @param handler
     *            the JLLL procedure to handle requests
     */
    public void addRoute(HandlerType type, String path, Procedure handler)
    {
        RouteRegistration reg = new RouteRegistration(type, path, handler);
        routes.add(reg);
        // If running, apply immediately
        if (app != null)
        {
            applyRoute(app, reg);
        }
    }

    /**
     * Adds a before filter.
     *
     * @param path
     *            the path pattern (null for all paths)
     * @param handler
     *            the JLLL procedure
     */
    public void addBeforeFilter(String path, Procedure handler)
    {
        FilterRegistration reg = new FilterRegistration(path, handler, true);
        beforeFilters.add(reg);
        if (app != null)
        {
            applyFilter(app, reg);
        }
    }

    /**
     * Adds an after filter.
     *
     * @param path
     *            the path pattern (null for all paths)
     * @param handler
     *            the JLLL procedure
     */
    public void addAfterFilter(String path, Procedure handler)
    {
        FilterRegistration reg = new FilterRegistration(path, handler, false);
        afterFilters.add(reg);
        if (app != null)
        {
            applyFilter(app, reg);
        }
    }

    /**
     * Adds an error handler for a specific HTTP status code.
     *
     * @param status
     *            the HTTP status code
     * @param handler
     *            the JLLL procedure
     */
    public void addErrorHandler(int status, Procedure handler)
    {
        errorHandlers.put(status, handler);
        if (app != null)
        {
            applyErrorHandler(app, status, handler);
        }
    }

    /**
     * Sets the global exception handler.
     *
     * @param handler
     *            the JLLL procedure (receives ctx and exception message)
     */
    public void setExceptionHandler(Procedure handler)
    {
        this.exceptionHandler = handler;
        if (app != null)
        {
            applyExceptionHandler(app, handler);
        }
    }

    /**
     * Adds static file serving.
     *
     * @param urlPath
     *            the URL path prefix
     * @param directory
     *            the directory to serve files from
     */
    public void addStaticFiles(String urlPath, String directory)
    {
        StaticFilesRegistration reg = new StaticFilesRegistration(urlPath, directory);
        staticFiles.add(reg);
        if (app != null)
        {
            applyStaticFiles(app, reg);
        }
    }

    /**
     * Adds an SSE endpoint.
     *
     * @param path
     *            the endpoint path
     * @param handler
     *            the JLLL procedure (receives SseClient)
     */
    public void addSseEndpoint(String path, Procedure handler)
    {
        SseRegistration reg = new SseRegistration(path, handler);
        sseEndpoints.add(reg);
        if (app != null)
        {
            applySseEndpoint(app, reg);
        }
    }
    // =============================================
    // Lifecycle
    // =============================================

    /**
     * Starts the server with the configured port and host.
     *
     * @throws JlllException
     *             if the server is already running or fails to start
     */
    public void start() throws JlllException
    {
        start(this.port, this.host);
    }

    /**
     * Starts the server with the specified port and host.
     *
     * @param port
     *            the port to listen on (updates stored config)
     * @param host
     *            the host to bind to (updates stored config)
     * @throws JlllException
     *             if the server is already running or fails to start
     */
    public void start(int port, String host) throws JlllException
    {
        if (isRunning())
        {
            throw new JlllException("http-start: server already running, use http-stop or http-restart");
        }
        // Update stored config
        this.port = port;
        this.host = host;
        // Create fresh Javalin instance
        app = Javalin.create(config ->
        {
            if (contextPath != null)
            {
                config.router.contextPath = contextPath;
            }
            config.appData(ENV_KEY, env);
            config.jsonMapper(new HttpLib.GsonJsonMapper());
        });
        // Apply all stored registrations
        applyAllRegistrations();
        // Start the server
        try
        {
            app.start(host, port);
        }
        catch (Exception e)
        {
            app = null; // Clear so retry works
            String msg = e.getMessage();
            if (msg == null)
            {
                msg = e.toString();
            }
            throw new JlllException("http-start: " + msg);
        }
    }

    /**
     * Stops the server.
     */
    public void stop()
    {
        if (app != null)
        {
            try
            {
                app.stop();
            }
            catch (Exception e)
            {
                // Ignore stop errors
            }
            app = null;
        }
    }

    /**
     * Restarts the server (stop + start).
     *
     * @throws JlllException
     *             if start fails
     */
    public void restart() throws JlllException
    {
        stop();
        start();
    }

    /**
     * Checks if the server is currently running.
     *
     * @return true if the server is running
     */
    public boolean isRunning()
    {
        if (app == null)
        {
            return false;
        }
        try
        {
            return app.jettyServer() != null && app.jettyServer().server() != null
                    && app.jettyServer().server().isRunning();
        }
        catch (Exception e)
        {
            return false;
        }
    }
    // =============================================
    // Internal: Apply Registrations
    // =============================================

    private void applyAllRegistrations()
    {
        // Before filters first
        for (FilterRegistration reg : beforeFilters)
        {
            applyFilter(app, reg);
        }
        // Routes
        for (RouteRegistration reg : routes)
        {
            applyRoute(app, reg);
        }
        // After filters
        for (FilterRegistration reg : afterFilters)
        {
            applyFilter(app, reg);
        }
        // Error handlers
        for (Map.Entry<Integer, Procedure> entry : errorHandlers.entrySet())
        {
            applyErrorHandler(app, entry.getKey(), entry.getValue());
        }
        // Exception handler
        if (exceptionHandler != null)
        {
            applyExceptionHandler(app, exceptionHandler);
        }
        // Static files
        for (StaticFilesRegistration reg : staticFiles)
        {
            applyStaticFiles(app, reg);
        }
        // SSE endpoints
        for (SseRegistration reg : sseEndpoints)
        {
            applySseEndpoint(app, reg);
        }
    }

    private void applyRoute(Javalin javalin, RouteRegistration reg)
    {
        Handler handler = ctx -> invokeHandler(reg.handler, ctx);
        switch (reg.type)
        {
            case GET :
                javalin.get(reg.path, handler);
                break;
            case POST :
                javalin.post(reg.path, handler);
                break;
            case PUT :
                javalin.put(reg.path, handler);
                break;
            case DELETE :
                javalin.delete(reg.path, handler);
                break;
            case PATCH :
                javalin.patch(reg.path, handler);
                break;
            case HEAD :
                javalin.head(reg.path, handler);
                break;
            case OPTIONS :
                javalin.options(reg.path, handler);
                break;
            default :
                // Ignore unsupported types
                break;
        }
    }

    private void applyFilter(Javalin javalin, FilterRegistration reg)
    {
        Handler handler = ctx -> invokeHandler(reg.handler, ctx);
        if (reg.isBefore)
        {
            if (reg.path == null)
            {
                javalin.before(handler);
            }
            else
            {
                javalin.before(reg.path, handler);
            }
        }
        else
        {
            if (reg.path == null)
            {
                javalin.after(handler);
            }
            else
            {
                javalin.after(reg.path, handler);
            }
        }
    }

    private void applyErrorHandler(Javalin javalin, int status, Procedure handler)
    {
        javalin.error(status, ctx -> invokeHandler(handler, ctx));
    }

    private void applyExceptionHandler(Javalin javalin, Procedure handler)
    {
        javalin.exception(Exception.class, (e, ctx) ->
        {
            try
            {
                Cons args = Cons.list(ctx, e.getMessage() != null ? e.getMessage() : e.toString());
                handler.apply(args, env);
            }
            catch (JlllException je)
            {
                ctx.status(500).result("Internal error: " + je.getMessage());
            }
        });
    }

    private void applyStaticFiles(Javalin javalin, StaticFilesRegistration reg)
    {
        String urlPath = reg.urlPath;
        if (!urlPath.endsWith("/*"))
        {
            urlPath = urlPath.endsWith("/") ? urlPath + "*" : urlPath + "/*";
        }
        String finalUrlPath = urlPath;
        String finalDir = reg.directory.endsWith("/") ? reg.directory : reg.directory + "/";
        javalin.get(urlPath, ctx ->
        {
            String requestPath = ctx.path();
            String basePath = finalUrlPath.replace("/*", "").replace("*", "");
            String filePath = requestPath.substring(basePath.length());
            if (filePath.startsWith("/"))
            {
                filePath = filePath.substring(1);
            }
            java.io.File file = new java.io.File(finalDir + filePath);
            if (file.exists() && file.isFile())
            {
                ctx.result(java.nio.file.Files.newInputStream(file.toPath()));
                String ext = filePath.substring(filePath.lastIndexOf('.') + 1).toLowerCase();
                ctx.contentType(getContentType(ext));
            }
            else
            {
                ctx.status(404).result("Not Found");
            }
        });
    }

    private void applySseEndpoint(Javalin javalin, SseRegistration reg)
    {
        Consumer<SseClient> sseHandler = client ->
        {
            try
            {
                client.keepAlive();
                Cons args = Cons.list(client);
                reg.handler.apply(args, env);
            }
            catch (JlllException e)
            {
                System.err.println("SSE handler error: " + e.getMessage());
            }
        };
        javalin.sse(reg.path, sseHandler);
    }

    private void invokeHandler(Procedure proc, io.javalin.http.Context ctx) throws Exception
    {
        try
        {
            Cons args = Cons.list(ctx);
            Object result = proc.apply(args, env);
            // If handler returns a string and result is not set, use it as result
            if (result != null && !ru.ydn.jlll.common.Null.NULL.equals(result) && ctx.resultInputStream() == null)
            {
                if (result instanceof String)
                {
                    ctx.result((String) result);
                }
            }
        }
        catch (JlllException e)
        {
            throw new RuntimeException("Handler error: " + e.getMessage(), e);
        }
    }

    private String getContentType(String ext)
    {
        return switch (ext)
        {
            case "html", "htm" -> "text/html";
            case "css" -> "text/css";
            case "js" -> "application/javascript";
            case "json" -> "application/json";
            case "png" -> "image/png";
            case "jpg", "jpeg" -> "image/jpeg";
            case "gif" -> "image/gif";
            case "svg" -> "image/svg+xml";
            case "ico" -> "image/x-icon";
            case "txt" -> "text/plain";
            case "xml" -> "application/xml";
            case "pdf" -> "application/pdf";
            case "zip" -> "application/zip";
            case "woff" -> "font/woff";
            case "woff2" -> "font/woff2";
            case "ttf" -> "font/ttf";
            case "eot" -> "application/vnd.ms-fontobject";
            default -> "application/octet-stream";
        };
    }
    // =============================================
    // Registration Records
    // =============================================

    private record RouteRegistration(HandlerType type, String path, Procedure handler) implements Serializable
    {
        private static final long serialVersionUID = 1L;
    }

    private record FilterRegistration(String path, Procedure handler, boolean isBefore) implements Serializable
    {
        private static final long serialVersionUID = 1L;
    }

    private record StaticFilesRegistration(String urlPath, String directory) implements Serializable
    {
        private static final long serialVersionUID = 1L;
    }

    private record SseRegistration(String path, Procedure handler) implements Serializable
    {
        private static final long serialVersionUID = 1L;
    }
}
