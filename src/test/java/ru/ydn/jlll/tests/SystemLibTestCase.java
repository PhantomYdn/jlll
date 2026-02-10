package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;

/**
 * Tests for the SystemLib system and environment functions.
 */
public class SystemLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Environment Variables ==========

    @Test
    public void testGetenv() throws Exception
    {
        // HOME or PATH should exist on most systems
        Object result = Jlll.eval("(getenv \"PATH\")", env);
        assertNotNull("PATH environment variable should exist", result);
        assertTrue(result instanceof String);
    }

    @Test
    public void testGetenvMissing() throws Exception
    {
        Object result = Jlll.eval("(getenv \"JLLL_NONEXISTENT_VAR_12345\")", env);
        // JLLL converts Java null to Null.NULL which displays as ()
        assertTrue("Nonexistent env var should return null/empty", result == null
                || (result instanceof ru.ydn.jlll.common.Cons && ((ru.ydn.jlll.common.Cons) result).isNull()));
    }

    @Test
    public void testGetenvWithDefault() throws Exception
    {
        eval("default-value", "(getenv \"JLLL_NONEXISTENT_VAR_12345\" \"default-value\")");
    }

    @Test
    public void testGetenvAll() throws Exception
    {
        Object result = Jlll.eval("(getenv-all)", env);
        assertTrue("getenv-all should return a Map", result instanceof Map);
        Map<?, ?> envMap = (Map<?, ?>) result;
        assertTrue("Environment should have entries", envMap.size() > 0);
        // PATH should exist
        assertTrue("PATH should be in environment", envMap.containsKey("PATH"));
    }
    // ========== System Properties ==========

    @Test
    public void testGetProperty() throws Exception
    {
        Object result = Jlll.eval("(get-property \"java.version\")", env);
        assertNotNull("java.version property should exist", result);
        assertTrue(result instanceof String);
    }

    @Test
    public void testGetPropertyMissing() throws Exception
    {
        Object result = Jlll.eval("(get-property \"jlll.nonexistent.prop\")", env);
        // JLLL converts Java null to Null.NULL which displays as ()
        assertTrue("Nonexistent property should return null/empty", result == null
                || (result instanceof ru.ydn.jlll.common.Cons && ((ru.ydn.jlll.common.Cons) result).isNull()));
    }

    @Test
    public void testGetPropertyWithDefault() throws Exception
    {
        eval("default", "(get-property \"jlll.nonexistent.prop\" \"default\")");
    }

    @Test
    public void testSetProperty() throws Exception
    {
        // Set a property
        Jlll.eval("(set-property! \"jlll.test.prop\" \"test-value\")", env);
        eval("test-value", "(get-property \"jlll.test.prop\")");
        // Clean up
        System.clearProperty("jlll.test.prop");
    }
    // ========== System Info (JLLL wrappers) ==========

    @Test
    public void testUserName() throws Exception
    {
        Object result = Jlll.eval("(user-name)", env);
        assertNotNull("user-name should return a value", result);
        assertEquals(System.getProperty("user.name"), result);
    }

    @Test
    public void testUserHome() throws Exception
    {
        Object result = Jlll.eval("(user-home)", env);
        assertNotNull("user-home should return a value", result);
        assertEquals(System.getProperty("user.home"), result);
    }

    @Test
    public void testOsName() throws Exception
    {
        Object result = Jlll.eval("(os-name)", env);
        assertNotNull("os-name should return a value", result);
        assertEquals(System.getProperty("os.name"), result);
    }

    @Test
    public void testOsArch() throws Exception
    {
        Object result = Jlll.eval("(os-arch)", env);
        assertNotNull("os-arch should return a value", result);
        assertEquals(System.getProperty("os.arch"), result);
    }

    @Test
    public void testJavaVersion() throws Exception
    {
        Object result = Jlll.eval("(java-version)", env);
        assertNotNull("java-version should return a value", result);
        assertEquals(System.getProperty("java.version"), result);
    }

    @Test
    public void testHostname() throws Exception
    {
        Object result = Jlll.eval("(hostname)", env);
        assertNotNull("hostname should return a value", result);
        assertTrue(result instanceof String);
        assertFalse(((String) result).isEmpty());
    }
    // ========== Memory Functions ==========

    @Test
    public void testGc() throws Exception
    {
        // gc should not throw and return null
        Object result = Jlll.eval("(gc)", env);
        // JLLL converts Java null to Null.NULL which displays as ()
        assertTrue("gc should return null/empty", result == null
                || (result instanceof ru.ydn.jlll.common.Cons && ((ru.ydn.jlll.common.Cons) result).isNull()));
    }

    @Test
    public void testMemoryUsed() throws Exception
    {
        Object result = Jlll.eval("(memory-used)", env);
        assertTrue("memory-used should return a Long", result instanceof Long);
        assertTrue("memory-used should be positive", (Long) result > 0);
    }

    @Test
    public void testMemoryFree() throws Exception
    {
        Object result = Jlll.eval("(memory-free)", env);
        assertTrue("memory-free should return a Long", result instanceof Long);
        assertTrue("memory-free should be positive", (Long) result > 0);
    }

    @Test
    public void testMemoryTotal() throws Exception
    {
        Object result = Jlll.eval("(memory-total)", env);
        assertTrue("memory-total should return a Long", result instanceof Long);
        assertTrue("memory-total should be positive", (Long) result > 0);
    }

    @Test
    public void testMemoryMax() throws Exception
    {
        Object result = Jlll.eval("(memory-max)", env);
        assertTrue("memory-max should return a Long", result instanceof Long);
        assertTrue("memory-max should be positive", (Long) result > 0);
    }

    @Test
    public void testMemoryConsistency() throws Exception
    {
        // used + free should equal total
        Long used = (Long) Jlll.eval("(memory-used)", env);
        Long free = (Long) Jlll.eval("(memory-free)", env);
        Long total = (Long) Jlll.eval("(memory-total)", env);
        assertEquals("used + free should equal total", total, Long.valueOf(used + free));
    }
    // ========== CPU ==========

    @Test
    public void testAvailableProcessors() throws Exception
    {
        Object result = Jlll.eval("(available-processors)", env);
        assertTrue("available-processors should return an Integer", result instanceof Integer);
        assertTrue("available-processors should be at least 1", (Integer) result >= 1);
        assertEquals(Runtime.getRuntime().availableProcessors(), result);
    }
    // ========== Browser ==========

    @Test
    public void testOpenBrowserExists() throws Exception
    {
        // Test that open-browser function exists and returns a Boolean
        // We can't easily test actual browser opening in CI, but we can verify the function works
        // Using a file path that doesn't exist - function should still return true/false, not throw
        Object result = Jlll.eval("(open-browser \"/nonexistent/path/test.html\")", env);
        assertTrue("open-browser should return a Boolean", result instanceof Boolean);
    }

    @Test
    public void testOpenBrowserWithUrl() throws Exception
    {
        // Test with a URL - the function should return Boolean (true if it tried, false if failed)
        // Note: This may actually open a browser on developer machines, but CI headless should handle it
        Object result = Jlll.eval("(open-browser \"https://example.com\")", env);
        assertTrue("open-browser should return a Boolean", result instanceof Boolean);
    }
    // ========== Helper ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
