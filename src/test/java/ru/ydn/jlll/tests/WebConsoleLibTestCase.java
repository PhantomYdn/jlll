package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.util.Map;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.libs.WebConsoleLib;

/**
 * Tests for WebConsoleLib - browser-based REPL.
 */
public class WebConsoleLibTestCase
{
    private Environment env;
    private int port = 0;

    @Before
    public void setUp() throws Exception
    {
        env = new Environment(Environment.top);
        // Load web console library
        new WebConsoleLib().load(env);
        // Find available port
        try (java.net.ServerSocket socket = new java.net.ServerSocket(0))
        {
            port = socket.getLocalPort();
        }
    }

    @After
    public void tearDown()
    {
        // Stop web console if running
        try
        {
            Jlll.eval("(web-console :stop true)", env);
        }
        catch (Exception e)
        {
            // Ignore
        }
    }
    // ========================================
    // Basic Tests
    // ========================================

    @Test
    public void testWebConsoleStatus() throws Exception
    {
        Object result = Jlll.eval("(web-console :status true)", env);
        assertNotNull(result);
        assertTrue(result instanceof Map);
        @SuppressWarnings("unchecked")
        Map<String, Object> status = (Map<String, Object>) result;
        assertEquals(false, status.get("running"));
    }

    @Test
    public void testWebConsoleRunningPredicate() throws Exception
    {
        assertEquals(false, Jlll.eval("(web-console-running?)", env));
    }

    @Test
    public void testWebConsoleStartStop() throws Exception
    {
        // Start web console
        Object result = Jlll.eval("(web-console :port " + port + " :bind \"127.0.0.1\")", env);
        assertNotNull(result);
        assertTrue(result instanceof Map);
        @SuppressWarnings("unchecked")
        Map<String, Object> startResult = (Map<String, Object>) result;
        assertEquals(true, startResult.get("running"));
        assertEquals(port, startResult.get("port"));
        assertEquals("127.0.0.1", startResult.get("host"));
        // Check running predicate
        assertEquals(true, Jlll.eval("(web-console-running?)", env));
        // Check status
        @SuppressWarnings("unchecked")
        Map<String, Object> status = (Map<String, Object>) Jlll.eval("(web-console :status true)", env);
        assertEquals(true, status.get("running"));
        assertEquals(port, status.get("port"));
        // Stop web console
        @SuppressWarnings("unchecked")
        Map<String, Object> stopResult = (Map<String, Object>) Jlll.eval("(web-console :stop true)", env);
        assertEquals(false, stopResult.get("running"));
        // Check running predicate
        assertEquals(false, Jlll.eval("(web-console-running?)", env));
    }

    @Test
    public void testWebConsoleAlreadyRunning() throws Exception
    {
        // Start first time
        Jlll.eval("(web-console :port " + port + " :bind \"127.0.0.1\")", env);
        assertEquals(true, Jlll.eval("(web-console-running?)", env));
        // Try to start again
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) Jlll.eval("(web-console :port " + (port + 1) + ")", env);
        // Should report already running
        assertNotNull(result.get("error"));
        assertTrue(result.get("error").toString().contains("already running"));
    }

    @Test
    public void testWebConsoleStopWhenNotRunning() throws Exception
    {
        // Stop when not running
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) Jlll.eval("(web-console :stop true)", env);
        assertEquals(false, result.get("running"));
        assertNotNull(result.get("message"));
    }
    // ========================================
    // HTTP Endpoint Tests (if server is running)
    // ========================================

    @Test
    public void testWebConsoleEndpoints() throws Exception
    {
        // Start web console
        Jlll.eval("(web-console :port " + port + " :bind \"127.0.0.1\")", env);
        // Give server time to start
        Thread.sleep(200);
        // Test status endpoint
        java.net.URL statusUrl = new java.net.URL("http://127.0.0.1:" + port + "/status");
        java.net.HttpURLConnection conn = (java.net.HttpURLConnection) statusUrl.openConnection();
        conn.setRequestMethod("GET");
        assertEquals(200, conn.getResponseCode());
        String response = new String(conn.getInputStream().readAllBytes());
        assertTrue(response.contains("\"running\""));
        conn.disconnect();
        // Test HTML page
        java.net.URL htmlUrl = new java.net.URL("http://127.0.0.1:" + port + "/");
        conn = (java.net.HttpURLConnection) htmlUrl.openConnection();
        conn.setRequestMethod("GET");
        assertEquals(200, conn.getResponseCode());
        response = new String(conn.getInputStream().readAllBytes());
        assertTrue(response.contains("JLLL Web Console"));
        conn.disconnect();
        // Test CSS
        java.net.URL cssUrl = new java.net.URL("http://127.0.0.1:" + port + "/web-console.css");
        conn = (java.net.HttpURLConnection) cssUrl.openConnection();
        conn.setRequestMethod("GET");
        assertEquals(200, conn.getResponseCode());
        conn.disconnect();
        // Test JS
        java.net.URL jsUrl = new java.net.URL("http://127.0.0.1:" + port + "/web-console.js");
        conn = (java.net.HttpURLConnection) jsUrl.openConnection();
        conn.setRequestMethod("GET");
        assertEquals(200, conn.getResponseCode());
        conn.disconnect();
        // Test complete endpoint
        java.net.URL completeUrl = new java.net.URL("http://127.0.0.1:" + port + "/complete?prefix=def");
        conn = (java.net.HttpURLConnection) completeUrl.openConnection();
        conn.setRequestMethod("GET");
        assertEquals(200, conn.getResponseCode());
        response = new String(conn.getInputStream().readAllBytes());
        assertTrue(response.contains("\"completions\""));
        assertTrue(response.contains("define")); // 'define' should be in completions
        conn.disconnect();
    }
}
