package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.Null;

/**
 * Tests for metadata on bindings functionality.
 */
public class MetadataTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }

    private Object eval(String code) throws Exception
    {
        return Jlll.eval(code, env);
    }
    // === Basic Metadata on Define ===

    @Test
    public void testDefineWithDocMetadata() throws Exception
    {
        eval("(define x :doc \"test value\" 42)");
        assertEquals(42, eval("x"));
        assertEquals("test value", eval("(doc 'x)"));
    }

    @Test
    public void testDefineWithMultipleMetadata() throws Exception
    {
        eval("(define x :doc \"description\" :author \"John\" :version \"1.0\" 100)");
        assertEquals(100, eval("x"));
        assertEquals("description", eval("(doc 'x)"));
        assertEquals("John", eval("(meta 'x :author)"));
        assertEquals("1.0", eval("(meta 'x :version)"));
    }

    @Test
    public void testDefineWithoutMetadata() throws Exception
    {
        eval("(define x 42)");
        assertEquals(42, eval("x"));
        // No doc returns null (represented as () in JLLL)
        assertEquals(Null.NULL, eval("(doc 'x)"));
    }
    // === Function Definition with Metadata ===

    @Test
    public void testFunctionDefineWithDoc() throws Exception
    {
        eval("(define (add x y) :doc \"Adds two numbers\" (+ x y))");
        assertEquals(3, eval("(add 1 2)"));
        assertEquals("Adds two numbers", eval("(doc 'add)"));
    }

    @Test
    public void testFunctionDefineWithMultipleMetadata() throws Exception
    {
        eval("(define (greet name) :doc \"Greets a person\" :author \"Jane\" (concat \"Hello, \" name))");
        assertEquals("Hello, World", eval("(greet \"World\")"));
        assertEquals("Greets a person", eval("(doc 'greet)"));
        assertEquals("Jane", eval("(meta 'greet :author)"));
    }
    // === Metadata Position Flexibility ===

    @Test
    public void testMetadataBeforeValue() throws Exception
    {
        eval("(define a :doc \"before\" 1)");
        assertEquals("before", eval("(doc 'a)"));
    }

    @Test
    public void testMetadataInterleavedWithValue() throws Exception
    {
        // Keywords extracted regardless of position
        eval("(define :doc \"interleaved\" b 2)");
        assertEquals(2, eval("b"));
        assertEquals("interleaved", eval("(doc 'b)"));
    }
    // === meta Primitive ===

    @Test
    public void testMetaGetSpecificKey() throws Exception
    {
        eval("(define x :foo \"bar\" :baz 123 42)");
        assertEquals("bar", eval("(meta 'x :foo)"));
        assertEquals(123, eval("(meta 'x :baz)"));
    }

    @Test
    public void testMetaGetAllAsAlist() throws Exception
    {
        eval("(define x :a 1 :b 2 42)");
        Object result = eval("(meta 'x)");
        assertNotNull(result);
        assertTrue(result instanceof Cons);
        // Should be an alist like ((:a . 1) (:b . 2))
        String str = result.toString();
        assertTrue(str.contains(":a"));
        assertTrue(str.contains(":b"));
    }

    @Test
    public void testMetaReturnsNullForMissing() throws Exception
    {
        eval("(define x 42)");
        // Missing metadata returns null (represented as () in JLLL)
        assertEquals(Null.NULL, eval("(meta 'x :nonexistent)"));
    }
    // === set-meta! Primitive ===

    @Test
    public void testSetMetaAddsMetadata() throws Exception
    {
        eval("(define x 42)");
        assertEquals(Null.NULL, eval("(meta 'x :doc)"));
        eval("(set-meta! 'x :doc \"added later\")");
        assertEquals("added later", eval("(meta 'x :doc)"));
    }

    @Test
    public void testSetMetaUpdatesExisting() throws Exception
    {
        eval("(define x :doc \"original\" 42)");
        assertEquals("original", eval("(doc 'x)"));
        eval("(set-meta! 'x :doc \"updated\")");
        assertEquals("updated", eval("(doc 'x)"));
    }
    // === set! Preserves Metadata ===

    @Test
    public void testSetPreservesMetadata() throws Exception
    {
        eval("(define x :doc \"preserved\" 42)");
        assertEquals("preserved", eval("(doc 'x)"));
        eval("(set! x 100)");
        assertEquals(100, eval("x"));
        assertEquals("preserved", eval("(doc 'x)"));
    }
    // === define-from Primitive ===

    @Test
    public void testDefineFromCopiesValueAndMetadata() throws Exception
    {
        eval("(define x :doc \"original\" :version \"1.0\" 42)");
        eval("(define-from y 'x)");
        assertEquals(42, eval("y"));
        assertEquals("original", eval("(doc 'y)"));
        assertEquals("1.0", eval("(meta 'y :version)"));
    }

    @Test
    public void testDefineFromWithoutMetadata() throws Exception
    {
        eval("(define x 42)");
        eval("(define-from y 'x)");
        assertEquals(42, eval("y"));
        assertEquals(Null.NULL, eval("(doc 'y)"));
    }

    @Test
    public void testDefineFromIndependentMetadata() throws Exception
    {
        // Modifying source's metadata after define-from shouldn't affect copy
        eval("(define x :doc \"original\" 42)");
        eval("(define-from y 'x)");
        eval("(set-meta! 'x :doc \"modified\")");
        assertEquals("modified", eval("(doc 'x)"));
        assertEquals("original", eval("(doc 'y)"));
    }
    // === doc Primitive ===

    @Test
    public void testDocReturnsMetadataDoc() throws Exception
    {
        eval("(define x :doc \"my doc\" 42)");
        assertEquals("my doc", eval("(doc 'x)"));
    }

    @Test
    public void testDocForBuiltinPrimitive() throws Exception
    {
        // Built-in primitives now store doc as metadata
        // The doc primitive should have documentation set during construction
        Object result = eval("(doc 'doc)");
        assertNotNull(result);
        assertTrue(result.toString().contains("documentation"));
    }

    @Test
    public void testMetaForBuiltinPrimitive() throws Exception
    {
        // Built-in primitives should have :java-class metadata
        Object javaClass = eval("(meta 'doc :java-class)");
        assertNotNull(javaClass);
        assertTrue(javaClass.toString().contains("KernelLib"));
    }
    // === describe Integration ===

    @Test
    public void testDescribeIncludesMetadata() throws Exception
    {
        eval("(define x :doc \"test\" :author \"me\" 42)");
        String desc = (String) eval("(describe 'x)");
        assertTrue(desc.contains("42"));
        assertTrue(desc.contains("Metadata"));
        assertTrue(desc.contains(":doc"));
        assertTrue(desc.contains("test"));
    }
    // === env Primitive ===

    @Test
    public void testEnvReturnsNil() throws Exception
    {
        // env prints to stdout but returns nil
        Object result = eval("(env)");
        assertEquals(Null.NULL, result);
    }

    @Test
    public void testEnvWithPrefixFilter() throws Exception
    {
        // env with prefix filter should still return nil
        Object result = eval("(env \"def\")");
        assertEquals(Null.NULL, result);
    }

    @Test
    public void testEnvWithTypeFilter() throws Exception
    {
        // env with type filter should still return nil
        Object result = eval("(env :primitives)");
        assertEquals(Null.NULL, result);
        result = eval("(env :macros)");
        assertEquals(Null.NULL, result);
        result = eval("(env :procedures)");
        assertEquals(Null.NULL, result);
        result = eval("(env :variables)");
        assertEquals(Null.NULL, result);
    }

    @Test
    public void testDocPrintsAndReturns() throws Exception
    {
        // doc should return the doc string (and print it)
        eval("(define x :doc \"my documentation\" 42)");
        Object result = eval("(doc 'x)");
        assertEquals("my documentation", result);
    }

    @Test
    public void testDocReturnsNilForUndocumented() throws Exception
    {
        // doc for undocumented symbol returns nil
        eval("(define y 100)");
        Object result = eval("(doc 'y)");
        assertEquals(Null.NULL, result);
    }
    // === Edge Cases ===

    @Test
    public void testEmptyDefineThrows() throws Exception
    {
        try
        {
            eval("(define)");
            throw new AssertionError("Should have thrown");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("requires"));
        }
    }

    @Test
    public void testDefineWithOnlyKeywordsThrows() throws Exception
    {
        try
        {
            eval("(define :doc \"orphan\")");
            throw new AssertionError("Should have thrown");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("requires"));
        }
    }
}
