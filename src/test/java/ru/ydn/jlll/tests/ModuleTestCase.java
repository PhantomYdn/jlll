package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.File;
import java.io.FileWriter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.ModuleEnvironment;
import ru.ydn.jlll.common.Symbol;

/**
 * Tests for the module system: module definition, exports, and module registry.
 */
public class ModuleTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }

    @After
    public void tearDown()
    {
        // Clean up any modules created during tests
        Environment.removeModule("testmod");
        Environment.removeModule("mymath");
        Environment.removeModule("utils");
        Environment.removeModule("empty");
        Environment.removeModule("partial");
    }

    @Test
    public void testModuleBasicDefinition() throws Exception
    {
        // Define a simple module
        Jlll.eval("(module testmod (define x 42))", env);
        // Check module was registered
        ModuleEnvironment mod = Environment.getModule("testmod");
        assertNotNull("Module should be registered", mod);
        assertEquals("testmod", mod.getModuleName());
        assertTrue("Module should be marked as loaded", mod.isLoaded());
    }

    @Test
    public void testModuleExplicitExports() throws Exception
    {
        // Define module with explicit exports
        Jlll.eval("(module mymath " + "(export square) " + "(define (square x) (* x x)) "
                + "(define (private-helper x) (+ x 1)))", env);
        ModuleEnvironment mod = Environment.getModule("mymath");
        assertNotNull(mod);
        // Check export status
        assertTrue("square should be exported", mod.isExported(Symbol.intern("square")));
        assertFalse("private-helper should not be exported", mod.isExported(Symbol.intern("private-helper")));
        assertFalse("Module should not be in export-all mode", mod.isExportAll());
    }

    @Test
    public void testModuleExportMultiple() throws Exception
    {
        // Export multiple symbols at once
        Jlll.eval("(module mymath " + "(export square cube) " + "(define (square x) (* x x)) "
                + "(define (cube x) (* x x x)) " + "(define (private-fn x) x))", env);
        ModuleEnvironment mod = Environment.getModule("mymath");
        assertTrue(mod.isExported(Symbol.intern("square")));
        assertTrue(mod.isExported(Symbol.intern("cube")));
        assertFalse(mod.isExported(Symbol.intern("private-fn")));
    }

    @Test
    public void testModuleExportAll() throws Exception
    {
        // Export all bindings
        Jlll.eval("(module utils " + "(export *) " + "(define a 1) " + "(define b 2) " + "(define c 3))", env);
        ModuleEnvironment mod = Environment.getModule("utils");
        assertTrue("Module should be in export-all mode", mod.isExportAll());
        // All symbols should be exported
        assertTrue(mod.isExported(Symbol.intern("a")));
        assertTrue(mod.isExported(Symbol.intern("b")));
        assertTrue(mod.isExported(Symbol.intern("c")));
        // getExports should return all bindings
        assertEquals(3, mod.getExports().size());
    }

    @Test
    public void testModuleInternalCalls() throws Exception
    {
        // Test that module-internal functions can call each other
        Object result = Jlll.eval("(module mymath " + "(export cube) " + "(define (square x) (* x x)) "
                + "(define (cube x) (* x (square x))))", env);
        ModuleEnvironment mod = Environment.getModule("mymath");
        // The cube function should work, calling private square
        Object cubeVal = mod.lookup("cube");
        assertNotNull("cube should be defined", cubeVal);
    }

    @Test
    public void testModuleAccessesTopEnvironment() throws Exception
    {
        // Module should have access to standard primitives
        Object result = Jlll.eval("(module mymath " + "(export double) " + "(define (double x) (* x 2)))", env);
        // If we got here without error, the module could use * from top env
        ModuleEnvironment mod = Environment.getModule("mymath");
        assertNotNull(mod);
    }

    @Test
    public void testModuleReturnsLastValue() throws Exception
    {
        // Module should return the value of the last expression
        Object result = Jlll.eval("(module testmod " + "(define x 1) " + "(define y 2) " + "(+ x y))", env);
        assertEquals(3, ((Number) result).intValue());
    }

    @Test
    public void testModuleEmptyBody() throws Exception
    {
        // Empty module should work
        Jlll.eval("(module empty)", env);
        ModuleEnvironment mod = Environment.getModule("empty");
        assertNotNull(mod);
        assertTrue(mod.isLoaded());
        assertTrue(mod.getExports().isEmpty());
    }

    @Test
    public void testExportOutsideModuleFails() throws Exception
    {
        // export outside a module should fail
        try
        {
            Jlll.eval("(export foo)", env);
            fail("export outside module should throw exception");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("only be used inside a module"));
        }
    }

    @Test
    public void testModuleRegistryMethods() throws Exception
    {
        // Test the registry API
        assertFalse(Environment.hasModule("nonexistent"));
        Jlll.eval("(module testmod (define x 1))", env);
        assertTrue(Environment.hasModule("testmod"));
        assertNotNull(Environment.getModule("testmod"));
        assertTrue(Environment.getModuleNames().contains("testmod"));
    }

    @Test
    public void testModuleRedefinition() throws Exception
    {
        // Define a module
        Jlll.eval("(module partial (export a) (define a 1))", env);
        ModuleEnvironment mod1 = Environment.getModule("partial");
        assertEquals(1, ((Number) mod1.lookup("a")).intValue());
        // Redefine the same module (for partial loading support)
        Jlll.eval("(module partial (export b) (define b 2))", env);
        ModuleEnvironment mod2 = Environment.getModule("partial");
        // Should be the same module object (reused)
        assertSame(mod1, mod2);
        // Should have both bindings now
        assertEquals(1, ((Number) mod2.lookup("a")).intValue());
        assertEquals(2, ((Number) mod2.lookup("b")).intValue());
    }

    @Test
    public void testModuleToString() throws Exception
    {
        Jlll.eval("(module testmod (define x 1))", env);
        ModuleEnvironment mod = Environment.getModule("testmod");
        String str = mod.toString();
        assertTrue(str.contains("module"));
        assertTrue(str.contains("testmod"));
    }

    @Test
    public void testExportedMetadata() throws Exception
    {
        // Define module with documented function
        Jlll.eval("(module mymath " + "(export square) " + "(define (square x) :doc \"Returns x squared\" (* x x)))",
                env);
        ModuleEnvironment mod = Environment.getModule("mymath");
        // Check metadata is preserved
        Object doc = mod.getMeta(Symbol.intern("square"), Symbol.intern("doc"));
        assertEquals("Returns x squared", doc);
        // getExportedMetadata should include it
        var metaMap = mod.getExportedMetadata();
        assertTrue(metaMap.containsKey(Symbol.intern("square")));
    }
    // ========== Load primitive tests ==========

    @Test
    public void testLoadFile() throws Exception
    {
        // Create a temporary test file
        File tempFile = File.createTempFile("jlll-test-", ".jlll");
        tempFile.deleteOnExit();
        try (FileWriter writer = new FileWriter(tempFile))
        {
            writer.write("(define loaded-value 123)\n");
            writer.write("(define (loaded-fn x) (+ x 10))\n");
        }
        // Load the file
        Jlll.eval("(load \"" + tempFile.getAbsolutePath().replace("\\", "\\\\") + "\")", env);
        // Check definitions are now available
        Object value = Jlll.eval("loaded-value", env);
        assertEquals(123, ((Number) value).intValue());
        Object fnResult = Jlll.eval("(loaded-fn 5)", env);
        assertEquals(15, ((Number) fnResult).intValue());
    }

    @Test
    public void testLoadFileNotFound() throws Exception
    {
        try
        {
            Jlll.eval("(load \"/nonexistent/path/to/file.jlll\")", env);
            fail("load should throw exception for nonexistent file");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("File not found"));
        }
    }

    @Test
    public void testLoadMultipleExpressions() throws Exception
    {
        // Create a temp file with multiple expressions
        File tempFile = File.createTempFile("jlll-multi-", ".jlll");
        tempFile.deleteOnExit();
        try (FileWriter writer = new FileWriter(tempFile))
        {
            writer.write("; Comment line\n");
            writer.write("(define a 1)\n");
            writer.write("(define b 2)\n");
            writer.write("(define c (+ a b))\n");
        }
        Jlll.eval("(load \"" + tempFile.getAbsolutePath().replace("\\", "\\\\") + "\")", env);
        assertEquals(1, ((Number) Jlll.eval("a", env)).intValue());
        assertEquals(2, ((Number) Jlll.eval("b", env)).intValue());
        assertEquals(3, ((Number) Jlll.eval("c", env)).intValue());
    }
    // ========== Import primitive tests ==========

    @Test
    public void testImportBasic() throws Exception
    {
        // Define a module
        Jlll.eval("(module mymath (export square cube) " + "(define (square x) (* x x)) "
                + "(define (cube x) (* x x x)))", env);
        // Import all exports
        Jlll.eval("(import mymath)", env);
        // Now square and cube should be available
        assertEquals(25, ((Number) Jlll.eval("(square 5)", env)).intValue());
        assertEquals(27, ((Number) Jlll.eval("(cube 3)", env)).intValue());
    }

    @Test
    public void testImportWithPrefix() throws Exception
    {
        Jlll.eval("(module mymath (export square) (define (square x) (* x x)))", env);
        // Import with prefix
        Jlll.eval("(import mymath :prefix m/)", env);
        // Should be available as m/square
        assertEquals(16, ((Number) Jlll.eval("(m/square 4)", env)).intValue());
    }

    @Test
    public void testImportOnlySpecific() throws Exception
    {
        Jlll.eval("(module mymath (export square cube) " + "(define (square x) (* x x)) "
                + "(define (cube x) (* x x x)))", env);
        // Import only square
        Jlll.eval("(import mymath :only (square))", env);
        assertEquals(9, ((Number) Jlll.eval("(square 3)", env)).intValue());
        // cube should NOT be imported
        try
        {
            Jlll.eval("(cube 3)", env);
            fail("cube should not be imported");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("unbound"));
        }
    }

    @Test
    public void testImportExcept() throws Exception
    {
        Jlll.eval("(module utils (export a b c) (define a 1) (define b 2) (define c 3))", env);
        // Import all except b
        Jlll.eval("(import utils :except (b))", env);
        assertEquals(1, ((Number) Jlll.eval("a", env)).intValue());
        assertEquals(3, ((Number) Jlll.eval("c", env)).intValue());
        // b should NOT be imported
        try
        {
            Jlll.eval("b", env);
            fail("b should not be imported");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("unbound"));
        }
    }

    @Test
    public void testImportNotFoundModule() throws Exception
    {
        try
        {
            Jlll.eval("(import nonexistent)", env);
            fail("Should throw for nonexistent module");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("module not found"));
        }
    }

    @Test
    public void testImportPreservesMetadata() throws Exception
    {
        Jlll.eval("(module mymath (export square) " + "(define (square x) :doc \"Returns x squared\" (* x x)))", env);
        Jlll.eval("(import mymath)", env);
        // Check metadata was copied
        Object doc = Jlll.eval("(doc 'square)", env);
        assertEquals("Returns x squared", doc);
    }
    // ========== Qualified symbol tests ==========

    @Test
    public void testQualifiedSymbolAccess() throws Exception
    {
        Jlll.eval("(module mymath (export square) (define (square x) (* x x)))", env);
        // Access without importing using qualified name
        assertEquals(36, ((Number) Jlll.eval("(mymath/square 6)", env)).intValue());
    }

    @Test
    public void testQualifiedSymbolPrivateFails() throws Exception
    {
        Jlll.eval("(module mymath (export square) " + "(define (square x) (* x x)) " + "(define (private-fn x) x))",
                env);
        // Public symbol works
        assertEquals(4, ((Number) Jlll.eval("(mymath/square 2)", env)).intValue());
        // Private symbol should fail
        try
        {
            Jlll.eval("(mymath/private-fn 5)", env);
            fail("Should not access private symbol");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("unbound"));
        }
    }

    @Test
    public void testQualifiedSymbolNonexistentModule() throws Exception
    {
        try
        {
            Jlll.eval("(nonexistent/foo)", env);
            fail("Should throw for nonexistent module");
        }
        catch (JlllException e)
        {
            assertTrue(e.getMessage().contains("unbound"));
        }
    }
    // ========== Require primitive tests ==========

    @Test
    public void testRequireFile() throws Exception
    {
        // Create a temp file with a module
        File tempFile = File.createTempFile("mymod", ".jlll");
        tempFile.deleteOnExit();
        try (FileWriter writer = new FileWriter(tempFile))
        {
            writer.write("(module mymod (export double) (define (double x) (* x 2)))\n");
        }
        // Require the file
        Jlll.eval("(require \"" + tempFile.getAbsolutePath().replace("\\", "\\\\") + "\")", env);
        // Should have imported the module
        assertEquals(10, ((Number) Jlll.eval("(double 5)", env)).intValue());
        // Clean up
        Environment.removeModule("mymod");
    }

    @Test
    public void testRequireWithAs() throws Exception
    {
        // Create a temp file with a module
        File tempFile = File.createTempFile("mymod", ".jlll");
        tempFile.deleteOnExit();
        try (FileWriter writer = new FileWriter(tempFile))
        {
            writer.write("(module mymod (export triple) (define (triple x) (* x 3)))\n");
        }
        // Require with prefix
        Jlll.eval("(require \"" + tempFile.getAbsolutePath().replace("\\", "\\\\") + "\" :as m)", env);
        // Should be available with prefix
        assertEquals(15, ((Number) Jlll.eval("(m/triple 5)", env)).intValue());
        // Clean up
        Environment.removeModule("mymod");
    }

    @Test
    public void testRequireNonModuleFile() throws Exception
    {
        // Create a file without a module definition
        File tempFile = File.createTempFile("simple", ".jlll");
        tempFile.deleteOnExit();
        try (FileWriter writer = new FileWriter(tempFile))
        {
            writer.write("(define simple-value 42)\n");
        }
        // Require should still work, importing top-level definitions
        Jlll.eval("(require \"" + tempFile.getAbsolutePath().replace("\\", "\\\\") + "\")", env);
        assertEquals(42, ((Number) Jlll.eval("simple-value", env)).intValue());
    }
}
