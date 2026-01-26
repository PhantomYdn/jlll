package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;

/**
 * Tests for the HashLib hash table functions.
 */
public class HashLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Creation ==========

    @Test
    public void testMakeHash() throws Exception
    {
        Object result = Jlll.eval("(make-hash)", env);
        assertTrue("make-hash should return a Map", result instanceof Map);
        assertEquals("make-hash should return empty map", 0, ((Map<?, ?>) result).size());
    }

    @Test
    public void testHashMap() throws Exception
    {
        Object result = Jlll.eval("(hash-map :a 1 :b 2 :c 3)", env);
        assertTrue("hash-map should return a Map", result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals(3, map.size());
        assertEquals(1, map.get(Jlll.eval(":a", env)));
        assertEquals(2, map.get(Jlll.eval(":b", env)));
        assertEquals(3, map.get(Jlll.eval(":c", env)));
    }

    @Test
    public void testHashMapEmpty() throws Exception
    {
        Object result = Jlll.eval("(hash-map)", env);
        assertTrue("hash-map with no args should return empty Map", result instanceof Map);
        assertEquals(0, ((Map<?, ?>) result).size());
    }

    @Test
    public void testHashMapWithStrings() throws Exception
    {
        Object result = Jlll.eval("(hash-map \"name\" \"Alice\" \"age\" 30)", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals(2, map.size());
        assertEquals("Alice", map.get("name"));
        assertEquals(30, map.get("age"));
    }
    // ========== Predicates ==========

    @Test
    public void testHashPredicate() throws Exception
    {
        eval(true, "(hash? (make-hash))");
        eval(true, "(hash? (hash-map :a 1))");
        eval(false, "(hash? '(1 2 3))");
        eval(false, "(hash? 42)");
        eval(false, "(hash? \"string\")");
        eval(false, "(hash? :keyword)");
    }

    @Test
    public void testHashHasKey() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2))", env);
        eval(true, "(hash-has-key? h :a)");
        eval(true, "(hash-has-key? h :b)");
        eval(false, "(hash-has-key? h :c)");
        eval(false, "(hash-has-key? h \"a\")");
    }
    // ========== Access ==========

    @Test
    public void testHashRef() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2))", env);
        eval(1, "(hash-ref h :a)");
        eval(2, "(hash-ref h :b)");
        // Missing key returns null (which becomes () in JLLL)
        Object result = Jlll.eval("(hash-ref h :missing)", env);
        assertTrue("Missing key should return null/empty",
                result == null || (result instanceof Cons && ((Cons) result).isNull()));
    }

    @Test
    public void testHashRefWithDefault() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1))", env);
        eval(1, "(hash-ref h :a 'default)");
        eval("default", "(hash-ref h :missing \"default\")");
        eval(0, "(hash-ref h :missing 0)");
        eval(false, "(hash-ref h :missing false)");
    }

    @Test
    public void testHashKeys() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2 :c 3))", env);
        Object result = Jlll.eval("(hash-keys h)", env);
        assertTrue(result instanceof Cons);
        Cons keys = (Cons) result;
        assertEquals(3, keys.length());
        // Keys should be in insertion order (LinkedHashMap)
        assertEquals(Jlll.eval(":a", env), keys.get(0));
        assertEquals(Jlll.eval(":b", env), keys.get(1));
        assertEquals(Jlll.eval(":c", env), keys.get(2));
    }

    @Test
    public void testHashKeysEmpty() throws Exception
    {
        Object result = Jlll.eval("(hash-keys (make-hash))", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car()); // empty list
    }

    @Test
    public void testHashValues() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2 :c 3))", env);
        Object result = Jlll.eval("(hash-values h)", env);
        assertTrue(result instanceof Cons);
        Cons values = (Cons) result;
        assertEquals(3, values.length());
        // Values should be in insertion order
        assertEquals(1, values.get(0));
        assertEquals(2, values.get(1));
        assertEquals(3, values.get(2));
    }

    @Test
    public void testHashCount() throws Exception
    {
        eval(0, "(hash-count (make-hash))");
        eval(1, "(hash-count (hash-map :a 1))");
        eval(3, "(hash-count (hash-map :a 1 :b 2 :c 3))");
    }
    // ========== Mutation ==========

    @Test
    public void testHashSet() throws Exception
    {
        Jlll.eval("(define h (make-hash))", env);
        eval(1, "(hash-set! h :a 1)");
        eval(1, "(hash-ref h :a)");
        eval(2, "(hash-set! h :b 2)");
        eval(2, "(hash-count h)");
        // Overwrite existing key
        eval(10, "(hash-set! h :a 10)");
        eval(10, "(hash-ref h :a)");
        eval(2, "(hash-count h)");
    }

    @Test
    public void testHashRemove() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2 :c 3))", env);
        eval(3, "(hash-count h)");
        eval(2, "(hash-remove! h :b)"); // returns removed value
        eval(2, "(hash-count h)");
        eval(false, "(hash-has-key? h :b)");
        // Removing nonexistent key returns null (which becomes () in JLLL)
        Object result = Jlll.eval("(hash-remove! h :nonexistent)", env);
        assertTrue("Removing nonexistent should return null/empty",
                result == null || (result instanceof Cons && ((Cons) result).isNull()));
    }

    @Test
    public void testHashUpdate() throws Exception
    {
        Jlll.eval("(define h (hash-map :count 0))", env);
        eval(1, "(hash-update! h :count (lambda (x) (+ x 1)))");
        eval(1, "(hash-ref h :count)");
        eval(2, "(hash-update! h :count (lambda (x) (+ x 1)))");
        eval(2, "(hash-ref h :count)");
    }

    @Test
    public void testHashUpdateWithNull() throws Exception
    {
        Jlll.eval("(define h (make-hash))", env);
        // Update on missing key starts with null/()
        // The lambda ignores x and returns 42
        eval(42, "(hash-update! h :new (lambda (x) (if (null? x) 42 x)))");
        eval(42, "(hash-ref h :new)");
    }

    @Test
    public void testHashClear() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2 :c 3))", env);
        eval(3, "(hash-count h)");
        Object result = Jlll.eval("(hash-clear! h)", env);
        assertTrue(result instanceof Map);
        eval(0, "(hash-count h)");
        eval(false, "(hash-has-key? h :a)");
    }
    // ========== Conversion ==========

    @Test
    public void testHashToAlist() throws Exception
    {
        Jlll.eval("(define h (hash-map :a 1 :b 2))", env);
        Object result = Jlll.eval("(hash->alist h)", env);
        assertTrue(result instanceof Cons);
        Cons alist = (Cons) result;
        assertEquals(2, alist.length());
        // Check first pair
        Cons first = (Cons) alist.get(0);
        assertEquals(Jlll.eval(":a", env), first.car());
        assertEquals(1, first.cdr());
        // Check second pair
        Cons second = (Cons) alist.get(1);
        assertEquals(Jlll.eval(":b", env), second.car());
        assertEquals(2, second.cdr());
    }

    @Test
    public void testHashToAlistEmpty() throws Exception
    {
        Object result = Jlll.eval("(hash->alist (make-hash))", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car()); // empty list
    }

    @Test
    public void testAlistToHash() throws Exception
    {
        Object result = Jlll.eval("(alist->hash '((:a . 1) (:b . 2) (:c . 3)))", env);
        assertTrue(result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals(3, map.size());
        assertEquals(1, map.get(Jlll.eval(":a", env)));
        assertEquals(2, map.get(Jlll.eval(":b", env)));
        assertEquals(3, map.get(Jlll.eval(":c", env)));
    }

    @Test
    public void testAlistToHashEmpty() throws Exception
    {
        Object result = Jlll.eval("(alist->hash '())", env);
        assertTrue(result instanceof Map);
        assertEquals(0, ((Map<?, ?>) result).size());
    }

    @Test
    public void testHashMerge() throws Exception
    {
        Jlll.eval("(define h1 (hash-map :a 1 :b 2))", env);
        Jlll.eval("(define h2 (hash-map :b 20 :c 3))", env);
        Object result = Jlll.eval("(hash-merge h1 h2)", env);
        assertTrue(result instanceof Map);
        Map<?, ?> merged = (Map<?, ?>) result;
        assertEquals(3, merged.size());
        assertEquals(1, merged.get(Jlll.eval(":a", env)));
        assertEquals(20, merged.get(Jlll.eval(":b", env))); // h2 value overrides
        assertEquals(3, merged.get(Jlll.eval(":c", env)));
        // Original maps unchanged
        eval(2, "(hash-ref h1 :b)");
    }
    // ========== Roundtrip Conversions ==========

    @Test
    public void testAlistHashRoundtrip() throws Exception
    {
        Jlll.eval("(define original '((:x . 10) (:y . 20)))", env);
        Jlll.eval("(define h (alist->hash original))", env);
        Jlll.eval("(define back (hash->alist h))", env);
        // Verify contents match (order should be preserved)
        eval(2, "(length back)");
        Object first = Jlll.eval("(car back)", env);
        assertTrue(first instanceof Cons);
        assertEquals(Jlll.eval(":x", env), ((Cons) first).car());
        assertEquals(10, ((Cons) first).cdr());
    }
    // ========== Mixed Key Types ==========

    @Test
    public void testMixedKeyTypes() throws Exception
    {
        Jlll.eval("(define h (hash-map \"str\" 1 :kw 2 'sym 3 42 4))", env);
        eval(4, "(hash-count h)");
        eval(1, "(hash-ref h \"str\")");
        eval(2, "(hash-ref h :kw)");
        eval(3, "(hash-ref h 'sym)");
        eval(4, "(hash-ref h 42)");
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
