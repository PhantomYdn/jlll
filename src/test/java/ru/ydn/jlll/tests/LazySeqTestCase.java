package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllDelay;
import ru.ydn.jlll.common.JlllException;

/**
 * Tests for the LazyLib lazy sequence functions.
 */
public class LazySeqTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Delay/Force ==========

    @Test
    public void testDelayCreate() throws Exception
    {
        Object result = Jlll.eval("(delay (+ 1 2))", env);
        assertTrue("delay should return a JlllDelay", result instanceof JlllDelay);
    }

    @Test
    public void testDelayNotEvaluated() throws Exception
    {
        // Define a variable to track if expression was evaluated
        Jlll.eval("(define side-effect-counter (atom 0))", env);
        Jlll.eval("(define d (delay (begin (swap! side-effect-counter (lambda (x) (+ x 1))) 42)))", env);
        // Counter should still be 0 - delay not yet forced
        eval(0, "(deref side-effect-counter)");
    }

    @Test
    public void testForceDelay() throws Exception
    {
        Jlll.eval("(define d (delay (+ 1 2 3)))", env);
        eval(6, "(force d)");
    }

    @Test
    public void testForceMemoization() throws Exception
    {
        // Force should memoize - only evaluate once
        Jlll.eval("(define counter (atom 0))", env);
        Jlll.eval("(define d (delay (begin (swap! counter (lambda (x) (+ x 1))) 42)))", env);
        eval(42, "(force d)"); // First force
        eval(1, "(deref counter)"); // Counter should be 1
        eval(42, "(force d)"); // Second force
        eval(1, "(deref counter)"); // Counter still 1 (memoized)
        eval(42, "(force d)"); // Third force
        eval(1, "(deref counter)"); // Counter still 1
    }

    @Test
    public void testDelayPredicate() throws Exception
    {
        eval(true, "(delay? (delay 42))");
        eval(false, "(delay? 42)");
        eval(false, "(delay? '(1 2 3))");
    }

    @Test
    public void testRealizedDelay() throws Exception
    {
        Jlll.eval("(define d (delay (+ 1 2)))", env);
        eval(false, "(realized? d)");
        Jlll.eval("(force d)", env);
        eval(true, "(realized? d)");
    }
    // ========== Lazy Cons ==========

    @Test
    public void testLazyCons() throws Exception
    {
        Object result = Jlll.eval("(lazy-cons 1 '(2 3))", env);
        assertTrue(result instanceof Cons);
        Cons cons = (Cons) result;
        assertEquals(1, cons.car());
    }

    @Test
    public void testLazyConsDelaysTail() throws Exception
    {
        // Track if tail is evaluated
        Jlll.eval("(define counter (atom 0))", env);
        Jlll.eval("(define lc (lazy-cons 1 (begin (swap! counter (lambda (x) (+ x 1))) '(2 3))))", env);
        // Counter should be 0 - tail not yet evaluated
        eval(0, "(deref counter)");
        // Access car - tail still not evaluated
        eval(1, "(car lc)");
        eval(0, "(deref counter)");
        // Access cdr - now tail is evaluated
        Jlll.eval("(cdr lc)", env);
        eval(1, "(deref counter)");
    }

    @Test
    public void testLazySeqPredicate() throws Exception
    {
        Jlll.eval("(define lc (lazy-cons 1 '(2 3)))", env);
        eval(true, "(lazy-seq? lc)");
        // After forcing cdr, no longer lazy
        Jlll.eval("(cdr lc)", env);
        eval(false, "(lazy-seq? lc)");
        // Regular list is not lazy
        eval(false, "(lazy-seq? '(1 2 3))");
    }

    @Test
    public void testLazyConsToString() throws Exception
    {
        // Lazy cons should show "..." for unrealized tail
        Jlll.eval("(define lc (lazy-cons 1 '(2 3)))", env);
        Object result = Jlll.eval("lc", env);
        String str = result.toString();
        assertTrue("Lazy cons should show ... for unrealized tail", str.contains("..."));
    }
    // ========== Lazy Range ==========

    @Test
    public void testLazyRangeInfinite() throws Exception
    {
        Jlll.eval("(define naturals (lazy-range 0))", env);
        // lazy-range returns Integers when in Integer range
        Object car = Jlll.eval("(car naturals)", env);
        assertEquals(0, car);
        Object cadr = Jlll.eval("(car (cdr naturals))", env);
        assertEquals(1, cadr);
        Object caddr = Jlll.eval("(car (cdr (cdr naturals)))", env);
        assertEquals(2, caddr);
    }

    @Test
    public void testLazyRangeFinite() throws Exception
    {
        Object result = Jlll.eval("(realize (lazy-range 0 5))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(4, list.get(4));
    }

    @Test
    public void testLazyRangeWithStep() throws Exception
    {
        Object result = Jlll.eval("(realize (lazy-range 0 10 2))", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(8, list.get(4));
    }

    @Test
    public void testLazyRangeNegativeStep() throws Exception
    {
        Object result = Jlll.eval("(realize (lazy-range 10 0 -2))", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(10, list.get(0));
        assertEquals(2, list.get(4));
    }
    // ========== Take with Lazy Sequences ==========

    @Test
    public void testTakeFromInfinite() throws Exception
    {
        // take signature is (take list n)
        Object result = Jlll.eval("(take (lazy-range 0) 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(4, list.get(4));
    }

    @Test
    public void testTakeFromLazyMap() throws Exception
    {
        Jlll.eval("(define squares (lazy-map (lambda (x) (* x x)) (lazy-range 0)))", env);
        Object result = Jlll.eval("(take squares 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0)); // 0^2
        assertEquals(1, list.get(1)); // 1^2
        assertEquals(4, list.get(2)); // 2^2
        assertEquals(9, list.get(3)); // 3^2
        assertEquals(16, list.get(4)); // 4^2
    }
    // ========== Iterate ==========

    @Test
    public void testIterate() throws Exception
    {
        Jlll.eval("(define powers-of-2 (iterate (lambda (x) (* x 2)) 1))", env);
        Object result = Jlll.eval("(take powers-of-2 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(1, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(8, list.get(3));
        assertEquals(16, list.get(4));
    }

    @Test
    public void testIterateWithDoubling() throws Exception
    {
        // Simple iteration: doubling
        Jlll.eval("(define doubles (iterate (lambda (x) (* x 2)) 1))", env);
        Object result = Jlll.eval("(take doubles 6)", env);
        Cons list = (Cons) result;
        assertEquals(6, list.length());
        assertEquals(1, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(8, list.get(3));
        assertEquals(16, list.get(4));
        assertEquals(32, list.get(5));
    }
    // ========== Cycle ==========

    @Test
    public void testCycle() throws Exception
    {
        Jlll.eval("(define abc (cycle '(a b c)))", env);
        Object result = Jlll.eval("(take abc 7)", env);
        Cons list = (Cons) result;
        assertEquals(7, list.length());
        assertEquals("a", list.get(0).toString());
        assertEquals("b", list.get(1).toString());
        assertEquals("c", list.get(2).toString());
        assertEquals("a", list.get(3).toString());
        assertEquals("b", list.get(4).toString());
        assertEquals("c", list.get(5).toString());
        assertEquals("a", list.get(6).toString());
    }

    @Test
    public void testCycleEmpty() throws Exception
    {
        Object result = Jlll.eval("(cycle '())", env);
        assertTrue(result instanceof Cons);
        assertTrue(((Cons) result).isNull());
    }
    // ========== Repeat ==========

    @Test
    public void testRepeat() throws Exception
    {
        Object result = Jlll.eval("(take (repeat 'x) 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        for (int i = 0; i < 5; i++)
        {
            assertEquals("x", list.get(i).toString());
        }
    }
    // ========== Lazy Map ==========

    @Test
    public void testLazyMap() throws Exception
    {
        Jlll.eval("(define doubled (lazy-map (lambda (x) (* x 2)) (lazy-range 0 5)))", env);
        Object result = Jlll.eval("(realize doubled)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(6, list.get(3));
        assertEquals(8, list.get(4));
    }

    @Test
    public void testLazyMapIsLazy() throws Exception
    {
        Jlll.eval("(define counter (atom 0))", env);
        Jlll.eval("(define mapped (lazy-map (lambda (x) (swap! counter (lambda (c) (+ c 1))) x) (lazy-range 0 100)))",
                env);
        // Counter should be 1 - lazy-map evaluates head immediately, tail is lazy
        eval(1, "(deref counter)");
        // Take 3 elements (0, 1, 2) - take may evaluate one ahead to check for more
        Jlll.eval("(take mapped 3)", env);
        // Note: take evaluates 4 elements (0,1,2,3) to know when to stop at 3
        // This is expected behavior for eager take with lazy sequences
        Object counter = Jlll.eval("(deref counter)", env);
        assertTrue("Counter should be <= 4", ((Number) counter).intValue() <= 4);
    }
    // ========== Lazy Filter ==========

    @Test
    public void testLazyFilter() throws Exception
    {
        Jlll.eval("(define evens (lazy-filter even? (lazy-range 0 10)))", env);
        Object result = Jlll.eval("(realize evens)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(6, list.get(3));
        assertEquals(8, list.get(4));
    }

    @Test
    public void testLazyFilterInfinite() throws Exception
    {
        // Filter evens from infinite range
        Jlll.eval("(define evens (lazy-filter even? (lazy-range 0)))", env);
        Object result = Jlll.eval("(take evens 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(6, list.get(3));
        assertEquals(8, list.get(4));
    }
    // ========== Take/Drop with Lazy Sequences ==========
    // Note: Standard take/drop work transparently with lazy sequences

    @Test
    public void testTakeWithLazy() throws Exception
    {
        // Standard take works with lazy sequences
        Object result = Jlll.eval("(take (lazy-range 0 10) 3)", env);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(0, list.get(0));
        assertEquals(1, list.get(1));
        assertEquals(2, list.get(2));
    }

    @Test
    public void testDropWithLazy() throws Exception
    {
        // Standard drop works with lazy sequences
        Object result = Jlll.eval("(realize (drop (lazy-range 0 6) 3))", env);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(3, list.get(0));
        assertEquals(4, list.get(1));
        assertEquals(5, list.get(2));
    }
    // ========== Take-While/Drop-While with Lazy Sequences ==========

    @Test
    public void testTakeWhileWithLazy() throws Exception
    {
        // Standard take-while works with lazy sequences
        Object result = Jlll.eval("(take-while (lambda (x) (< x 5)) (lazy-range 0))", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(4, list.get(4));
    }

    @Test
    public void testDropWhileWithLazy() throws Exception
    {
        // Standard drop-while works with lazy sequences
        Object result = Jlll.eval("(take (drop-while (lambda (x) (< x 5)) (lazy-range 0)) 3)", env);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(5, list.get(0));
        assertEquals(6, list.get(1));
        assertEquals(7, list.get(2));
    }
    // ========== Realize ==========

    @Test
    public void testRealize() throws Exception
    {
        Object result = Jlll.eval("(realize (lazy-range 0 5))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        // Should be a regular eager list now
        assertFalse(list.hasLazyCdr());
    }

    @Test(expected = JlllException.class)
    public void testRealizeInfiniteThrows() throws Exception
    {
        // Attempting to realize an infinite sequence should fail with limit exceeded
        Jlll.eval("(realize (lazy-range 0))", env);
    }
    // ========== Integration with existing functions ==========

    @Test
    public void testLazyWithRegularTake() throws Exception
    {
        // Regular take should work with lazy sequences
        Object result = Jlll.eval("(take (lazy-range 0) 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
    }

    @Test
    public void testLazyWithRegularLength() throws Exception
    {
        // Length on a realized lazy sequence
        eval(5, "(length (realize (lazy-range 0 5)))");
    }

    @Test
    public void testChainedLazyOperations() throws Exception
    {
        // Chain multiple lazy operations
        Object result = Jlll.eval("(take (lazy-filter even? (lazy-map (lambda (x) (* x 2)) (lazy-range 0))) 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        // 0*2=0, 1*2=2, 2*2=4, 3*2=6, 4*2=8 - all even
        assertEquals(0, list.get(0));
        assertEquals(2, list.get(1));
        assertEquals(4, list.get(2));
        assertEquals(6, list.get(3));
        assertEquals(8, list.get(4));
    }
    // ========== JLLL Helper Functions ==========

    @Test
    public void testLazyNaturals() throws Exception
    {
        Object result = Jlll.eval("(take (lazy-naturals) 5)", env);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(0, list.get(0));
        assertEquals(4, list.get(4));
    }

    @Test
    public void testLazyConcat() throws Exception
    {
        Object result = Jlll.eval("(realize (lazy-concat '(1 2 3) '(4 5 6)))", env);
        Cons list = (Cons) result;
        assertEquals(6, list.length());
        assertEquals(1, list.get(0));
        assertEquals(6, list.get(5));
    }

    @Test
    public void testLazyZip() throws Exception
    {
        Object result = Jlll.eval("(take (lazy-zip (lazy-range 0) '(a b c d)) 3)", env);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        // Each element is a pair (list of 2)
        Object first = list.get(0);
        assertTrue(first instanceof Cons);
        assertEquals(0, ((Cons) first).get(0));
    }

    @Test
    public void testListRefWithLazy() throws Exception
    {
        // Standard list-ref works with lazy sequences
        eval(10, "(list-ref (lazy-range 0) 10)");
        eval(20, "(list-ref (lazy-range 10) 10)");
    }

    @Test
    public void testAnyWithLazy() throws Exception
    {
        // Standard any works with lazy sequences (short-circuits)
        eval(true, "(any even? (lazy-range 1))"); // finds 2 quickly
        eval(true, "(any (lambda (x) (> x 100)) (lazy-range 0))"); // finds 101
    }

    @Test
    public void testEveryWithLazy() throws Exception
    {
        // Standard every works with lazy sequences
        // On finite lazy sequence
        eval(true, "(every positive? (lazy-range 1 10))");
        eval(false, "(every even? (lazy-range 0 5))"); // 1 is odd
    }

    @Test
    public void testFindWithLazy() throws Exception
    {
        // Standard find works with lazy sequences
        eval(10, "(find (lambda (x) (> x 9)) (lazy-range 0))");
        eval(100, "(find (lambda (x) (= x 100)) (lazy-range 0))");
    }

    @Test
    public void testReduceWithLazy() throws Exception
    {
        // Standard reduce/fold-left works with lazy sequences (finite only)
        eval(45, "(reduce + (lazy-range 0 10))"); // sum 0..9
        eval(55, "(fold-left + 0 (lazy-range 1 11))"); // sum 1..10
    }

    @Test
    public void testListToLazy() throws Exception
    {
        Jlll.eval("(define lz (list->lazy '(1 2 3)))", env);
        eval(true, "(lazy-seq? lz)");
        eval(1, "(car lz)");
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
