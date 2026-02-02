package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.libs.JlllHttpServer;

/**
 * Tests for HttpLib - Javalin-based HTTP server library.
 */
public class HttpLibTestCase
{
    private Environment env;
    private JlllHttpServer server;
    private int port = 0; // Will use random available port

    @Before
    public void setUp() throws Exception
    {
        env = new Environment(Environment.top);
        // Find available port
        try (java.net.ServerSocket socket = new java.net.ServerSocket(0))
        {
            port = socket.getLocalPort();
        }
    }

    @After
    public void tearDown()
    {
        // Stop server if running
        if (server != null)
        {
            try
            {
                server.stop();
            }
            catch (Exception e)
            {
                // Ignore
            }
            server = null;
        }
    }
    // ========================================
    // Server Lifecycle Tests
    // ========================================

    @Test
    public void testHttpServerCreation() throws Exception
    {
        Object result = Jlll.eval("(http-server :port " + port + ")", env);
        assertNotNull(result);
        assertTrue(result instanceof JlllHttpServer);
        server = (JlllHttpServer) result;
        assertEquals(port, server.getPort());
    }

    @Test
    public void testHttpServerPredicate() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        assertEquals(true, Jlll.eval("(http-server? s)", env));
        assertEquals(false, Jlll.eval("(http-server? \"not a server\")", env));
        assertEquals(false, Jlll.eval("(http-server? 123)", env));
    }

    @Test
    public void testHttpRunningPredicate() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        // Not running yet
        assertEquals(false, Jlll.eval("(http-running? s)", env));
        // Start it
        Jlll.eval("(http-start s)", env);
        assertEquals(true, Jlll.eval("(http-running? s)", env));
        // Stop it
        Jlll.eval("(http-stop s)", env);
        assertEquals(false, Jlll.eval("(http-running? s)", env));
    }

    @Test
    public void testHttpServerStartStop() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        // Start server
        Jlll.eval("(http-start s)", env);
        // Verify server is running by making a request (should get 404)
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/").openConnection();
        conn.setRequestMethod("GET");
        int responseCode = conn.getResponseCode();
        assertTrue(responseCode == 404 || responseCode == 200);
        conn.disconnect();
        // Stop server
        Jlll.eval("(http-stop s)", env);
    }

    @Test
    public void testHttpRestart() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/test\" (lambda (ctx) (http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        // Verify it works
        String response = httpGet("http://localhost:" + port + "/test");
        assertEquals("OK", response);
        // Restart
        Jlll.eval("(http-restart s)", env);
        // Verify routes still work after restart
        response = httpGet("http://localhost:" + port + "/test");
        assertEquals("OK", response);
    }

    @Test
    public void testRetryAfterFailedStart() throws Exception
    {
        // Start a server on the port to block it
        Jlll.eval("(define blocker (http-server :port " + port + "))", env);
        JlllHttpServer blocker = (JlllHttpServer) env.lookup(Symbol.intern("blocker"));
        Jlll.eval("(http-start blocker)", env);
        // Find another port
        int newPort;
        try (java.net.ServerSocket socket = new java.net.ServerSocket(0))
        {
            newPort = socket.getLocalPort();
        }
        // Try to start another server on the blocked port - should fail
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/test\" (lambda (ctx) (http-result ctx \"OK\")))", env);
        try
        {
            Jlll.eval("(http-start s)", env);
            fail("Should have thrown exception for port in use");
        }
        catch (JlllException e)
        {
            // Expected - port is in use
            assertTrue(e.getMessage().toLowerCase().contains("port") || e.getMessage().toLowerCase().contains("use")
                    || e.getMessage().toLowerCase().contains("bind"));
        }
        // Stop blocker
        blocker.stop();
        // Retry with new port - should work and routes should be preserved
        Jlll.eval("(http-start s :port " + newPort + ")", env);
        String response = httpGet("http://localhost:" + newPort + "/test");
        assertEquals("OK", response);
    }

    @Test
    public void testStartWithPortOverride() throws Exception
    {
        // Create server with one port
        Jlll.eval("(define s (http-server :port 9999))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        assertEquals(9999, server.getPort());
        // Start with different port
        Jlll.eval("(http-start s :port " + port + ")", env);
        assertEquals(port, server.getPort()); // Port should be updated
        // Verify server is running on the new port
        Jlll.eval("(http-get s \"/test\" (lambda (ctx) (http-result ctx \"OK\")))", env);
        // Need to wait a moment for route to be applied since server is already started
        Thread.sleep(100);
        String response = httpGet("http://localhost:" + port + "/test");
        assertEquals("OK", response);
    }
    // ========================================
    // Route Registration Tests
    // ========================================

    @Test
    public void testHttpGet() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/hello\" (lambda (ctx) (http-result ctx \"Hello World\")))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpGet("http://localhost:" + port + "/hello");
        assertEquals("Hello World", response);
    }

    @Test
    public void testHttpPost() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-post s \"/echo\" (lambda (ctx) (http-result ctx (http-body ctx))))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpPost("http://localhost:" + port + "/echo", "test body");
        assertEquals("test body", response);
    }

    @Test
    public void testHttpPut() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-put s \"/data\" (lambda (ctx) (http-result ctx \"PUT received\")))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpMethod("http://localhost:" + port + "/data", "PUT", "");
        assertEquals("PUT received", response);
    }

    @Test
    public void testHttpDelete() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-delete s \"/item\" (lambda (ctx) (http-result ctx \"Deleted\")))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpMethod("http://localhost:" + port + "/item", "DELETE", null);
        assertEquals("Deleted", response);
    }

    @Test
    public void testPathParameters() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/users/{id}\" (lambda (ctx) (http-result ctx (http-path-param ctx \"id\"))))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpGet("http://localhost:" + port + "/users/123");
        assertEquals("123", response);
    }

    @Test
    public void testQueryParameters() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/search\" (lambda (ctx) (http-result ctx (http-query-param ctx \"q\"))))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpGet("http://localhost:" + port + "/search?q=test");
        assertEquals("test", response);
    }

    @Test
    public void testAddRouteWhileRunning() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/first\" (lambda (ctx) (http-result ctx \"First\")))", env);
        Jlll.eval("(http-start s)", env);
        // Add route while server is running
        Jlll.eval("(http-get s \"/second\" (lambda (ctx) (http-result ctx \"Second\")))", env);
        // Both routes should work
        assertEquals("First", httpGet("http://localhost:" + port + "/first"));
        assertEquals("Second", httpGet("http://localhost:" + port + "/second"));
    }
    // ========================================
    // JSON Response Tests
    // ========================================

    @Test
    public void testJsonResponse() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/api/data\" (lambda (ctx) (http-json ctx (hash-map :name \"test\" :value 42))))", env);
        Jlll.eval("(http-start s)", env);
        String response = httpGet("http://localhost:" + port + "/api/data");
        // Keywords serialize differently, check for the actual output
        // JSON should contain the name and value in some form
        assertTrue("Response should contain 'test': " + response, response.contains("test"));
        assertTrue("Response should contain '42': " + response, response.contains("42"));
    }
    // ========================================
    // Status Code Tests
    // ========================================

    @Test
    public void testStatusCode() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/notfound\" (lambda (ctx) (http-status ctx 404) (http-result ctx \"Not Found\")))",
                env);
        Jlll.eval("(http-start s)", env);
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/notfound").openConnection();
        conn.setRequestMethod("GET");
        assertEquals(404, conn.getResponseCode());
        conn.disconnect();
    }
    // ========================================
    // Before/After Filter Tests
    // ========================================

    @Test
    public void testBeforeFilter() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        Jlll.eval("(define before-called (atom false))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-before s (lambda (ctx) (reset! before-called true)))", env);
        Jlll.eval("(http-get s \"/test\" (lambda (ctx) (http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        httpGet("http://localhost:" + port + "/test");
        assertEquals(true, Jlll.eval("(deref before-called)", env));
    }

    @Test
    public void testAfterFilter() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        Jlll.eval("(define after-called (atom false))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/test\" (lambda (ctx) (http-result ctx \"OK\")))", env);
        Jlll.eval("(http-after s (lambda (ctx) (reset! after-called true)))", env);
        Jlll.eval("(http-start s)", env);
        httpGet("http://localhost:" + port + "/test");
        assertEquals(true, Jlll.eval("(deref after-called)", env));
    }
    // ========================================
    // Error Handler Tests
    // ========================================

    @Test
    public void testErrorHandler() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-error s 404 (lambda (ctx) (http-result ctx \"Custom 404\")))", env);
        Jlll.eval("(http-start s)", env);
        // Request a path that doesn't exist
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/nonexistent")
                .openConnection();
        conn.setRequestMethod("GET");
        assertEquals(404, conn.getResponseCode());
        // Read error stream
        BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
        String response = reader.readLine();
        reader.close();
        assertEquals("Custom 404", response);
        conn.disconnect();
    }
    // ========================================
    // Context Accessor Tests
    // ========================================

    @Test
    public void testHttpMethod() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        Jlll.eval("(define method-result (atom null))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/method\" (lambda (ctx) " + "(reset! method-result (http-method ctx)) "
                + "(http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        httpGet("http://localhost:" + port + "/method");
        Object result = Jlll.eval("(deref method-result)", env);
        assertNotNull(result);
        assertEquals(":get", result.toString());
    }

    @Test
    public void testHttpPath() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        Jlll.eval("(define path-result (atom null))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/api/users\" (lambda (ctx) " + "(reset! path-result (http-path ctx)) "
                + "(http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        httpGet("http://localhost:" + port + "/api/users");
        Object result = Jlll.eval("(deref path-result)", env);
        assertEquals("/api/users", result);
    }

    @Test
    public void testHttpHeaders() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        Jlll.eval("(define header-result (atom null))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/headers\" (lambda (ctx) " + "(reset! header-result (http-header ctx \"X-Custom\")) "
                + "(http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/headers").openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("X-Custom", "test-value");
        conn.getResponseCode();
        conn.disconnect();
        Object result = Jlll.eval("(deref header-result)", env);
        assertEquals("test-value", result);
    }

    @Test
    public void testHttpSetHeader() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/custom\" (lambda (ctx) " + "(http-header! ctx \"X-Response\" \"custom-value\") "
                + "(http-result ctx \"OK\")))", env);
        Jlll.eval("(http-start s)", env);
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/custom").openConnection();
        conn.setRequestMethod("GET");
        conn.getResponseCode();
        String headerValue = conn.getHeaderField("X-Response");
        conn.disconnect();
        assertEquals("custom-value", headerValue);
    }
    // ========================================
    // Redirect Test
    // ========================================

    @Test
    public void testHttpRedirect() throws Exception
    {
        Jlll.eval("(define s (http-server :port " + port + "))", env);
        server = (JlllHttpServer) env.lookup(Symbol.intern("s"));
        Jlll.eval("(http-get s \"/old\" (lambda (ctx) (http-redirect ctx \"/new\")))", env);
        Jlll.eval("(http-get s \"/new\" (lambda (ctx) (http-result ctx \"New Location\")))", env);
        Jlll.eval("(http-start s)", env);
        HttpURLConnection conn = (HttpURLConnection) new URL("http://localhost:" + port + "/old").openConnection();
        conn.setInstanceFollowRedirects(false);
        conn.setRequestMethod("GET");
        int responseCode = conn.getResponseCode();
        String location = conn.getHeaderField("Location");
        conn.disconnect();
        assertEquals(302, responseCode);
        assertEquals("/new", location);
    }
    // ========================================
    // Helper Methods
    // ========================================

    private String httpGet(String urlStr) throws Exception
    {
        HttpURLConnection conn = (HttpURLConnection) new URL(urlStr).openConnection();
        conn.setRequestMethod("GET");
        return readResponse(conn);
    }

    private String httpPost(String urlStr, String body) throws Exception
    {
        return httpMethod(urlStr, "POST", body);
    }

    private String httpMethod(String urlStr, String method, String body) throws Exception
    {
        HttpURLConnection conn = (HttpURLConnection) new URL(urlStr).openConnection();
        conn.setRequestMethod(method);
        if (body != null)
        {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream())
            {
                os.write(body.getBytes(StandardCharsets.UTF_8));
            }
        }
        return readResponse(conn);
    }

    private String readResponse(HttpURLConnection conn) throws Exception
    {
        int responseCode = conn.getResponseCode();
        BufferedReader reader;
        if (responseCode >= 400)
        {
            reader = new BufferedReader(new InputStreamReader(conn.getErrorStream()));
        }
        else
        {
            reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));
        }
        StringBuilder response = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null)
        {
            response.append(line);
        }
        reader.close();
        conn.disconnect();
        return response.toString();
    }
}
