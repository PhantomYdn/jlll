package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllAtom;
import ru.ydn.jlll.common.JlllFuture;

/**
 * Tests for the ConcurrencyLib parallel execution functions.
 */
public class ConcurrencyLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Atoms ==========

    @Test
    public void testAtomCreate() throws Exception
    {
        Object result = Jlll.eval("(atom 42)", env);
        assertTrue("atom should return a JlllAtom", result instanceof JlllAtom);
    }

    @Test
    public void testAtomDeref() throws Exception
    {
        Jlll.eval("(define a (atom 42))", env);
        eval(42, "(deref a)");
    }

    @Test
    public void testAtomReset() throws Exception
    {
        Jlll.eval("(define a (atom 0))", env);
        eval(0, "(deref a)");
        eval(100, "(reset! a 100)");
        eval(100, "(deref a)");
    }

    @Test
    public void testAtomSwap() throws Exception
    {
        Jlll.eval("(define a (atom 0))", env);
        eval(1, "(swap! a (lambda (x) (+ x 1)))");
        eval(1, "(deref a)");
        eval(2, "(swap! a (lambda (x) (+ x 1)))");
        eval(2, "(deref a)");
    }

    @Test
    public void testAtomSwapWithMultiplier() throws Exception
    {
        Jlll.eval("(define a (atom 5))", env);
        eval(10, "(swap! a (lambda (x) (* x 2)))");
        eval(10, "(deref a)");
    }

    @Test
    public void testAtomCompareAndSetSuccess() throws Exception
    {
        Jlll.eval("(define a (atom 42))", env);
        eval(true, "(compare-and-set! a 42 100)");
        eval(100, "(deref a)");
    }

    @Test
    public void testAtomCompareAndSetFailure() throws Exception
    {
        Jlll.eval("(define a (atom 42))", env);
        eval(false, "(compare-and-set! a 0 100)");
        eval(42, "(deref a)"); // unchanged
    }

    @Test
    public void testJlllAtomPredicate() throws Exception
    {
        eval(true, "(jlll-atom? (atom 0))");
        eval(false, "(jlll-atom? 42)");
        eval(false, "(jlll-atom? '(1 2 3))");
        eval(false, "(jlll-atom? \"string\")");
    }
    // ========== Futures ==========

    @Test
    public void testFutureCreate() throws Exception
    {
        Object result = Jlll.eval("(future (+ 1 2))", env);
        assertTrue("future should return a JlllFuture", result instanceof JlllFuture);
    }

    @Test
    public void testFutureDeref() throws Exception
    {
        Jlll.eval("(define f (future (+ 1 2 3)))", env);
        eval(6, "(deref f)");
    }

    @Test
    public void testFutureRealized() throws Exception
    {
        Jlll.eval("(define f (future (+ 1 2)))", env);
        // After deref, should be realized
        Jlll.eval("(deref f)", env);
        eval(true, "(realized? f)");
    }

    @Test
    public void testFutureDerefWithTimeout() throws Exception
    {
        // Simple computation that completes quickly
        Jlll.eval("(define f (future (* 2 21)))", env);
        eval(42, "(deref f 5000 'timeout)");
    }

    @Test
    public void testFuturePredicate() throws Exception
    {
        eval(true, "(future? (future 42))");
        eval(false, "(future? 42)");
        eval(false, "(future? (atom 0))");
        eval(false, "(future? '(1 2 3))");
    }

    @Test
    public void testRealizedOnAtom() throws Exception
    {
        eval(true, "(realized? (atom 0))"); // Atoms are always realized
    }
    // ========== Parallel Map ==========

    @Test
    public void testPmap() throws Exception
    {
        Object result = Jlll.eval("(pmap (lambda (x) (* x 2)) '(1 2 3 4 5))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(5, list.length());
        assertEquals(2, list.get(0));
        assertEquals(4, list.get(1));
        assertEquals(6, list.get(2));
        assertEquals(8, list.get(3));
        assertEquals(10, list.get(4));
    }

    @Test
    public void testPmapEmpty() throws Exception
    {
        Object result = Jlll.eval("(pmap (lambda (x) x) '())", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car()); // empty list
    }

    @Test
    public void testPmapPreservesOrder() throws Exception
    {
        // Even with parallel execution, order should be preserved
        Object result = Jlll.eval("(pmap (lambda (x) (+ x 100)) '(1 2 3 4 5))", env);
        Cons list = (Cons) result;
        assertEquals(101, list.get(0));
        assertEquals(102, list.get(1));
        assertEquals(103, list.get(2));
        assertEquals(104, list.get(3));
        assertEquals(105, list.get(4));
    }
    // ========== Parallel For-Each ==========

    @Test
    public void testPforEach() throws Exception
    {
        // Use atom to track side effects
        Jlll.eval("(define sum (atom 0))", env);
        // Note: pfor-each is not atomic, so we use swap! to ensure atomic updates
        Jlll.eval("(pfor-each (lambda (x) (swap! sum (lambda (s) (+ s x)))) '(1 2 3 4 5))", env);
        eval(15, "(deref sum)"); // 1+2+3+4+5 = 15
    }

    @Test
    public void testPforEachEmpty() throws Exception
    {
        Object result = Jlll.eval("(pfor-each (lambda (x) x) '())", env);
        // JLLL converts null to () (empty list)
        assertTrue("pfor-each should return null or empty list",
                result == null || (result instanceof Cons && ((Cons) result).isNull()));
    }

    @Test
    public void testPforEachReturnsNull() throws Exception
    {
        Object result = Jlll.eval("(pfor-each (lambda (x) (* x 2)) '(1 2 3))", env);
        // JLLL converts null to () (empty list)
        assertTrue("pfor-each should return null or empty list",
                result == null || (result instanceof Cons && ((Cons) result).isNull()));
    }
    // ========== Pcalls ==========

    @Test
    public void testPcalls() throws Exception
    {
        Object result = Jlll.eval("(pcalls (lambda () (+ 1 2)) (lambda () (* 3 4)) (lambda () (- 10 5)))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(3, list.get(0)); // 1+2
        assertEquals(12, list.get(1)); // 3*4
        assertEquals(5, list.get(2)); // 10-5
    }

    @Test
    public void testPcallsEmpty() throws Exception
    {
        Object result = Jlll.eval("(pcalls)", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car()); // empty list
    }

    @Test
    public void testPcallsSingleThunk() throws Exception
    {
        Object result = Jlll.eval("(pcalls (lambda () 42))", env);
        Cons list = (Cons) result;
        assertEquals(1, list.length());
        assertEquals(42, list.get(0));
    }
    // ========== Integration Tests ==========

    @Test
    public void testAtomWithFutures() throws Exception
    {
        // Multiple futures updating the same atom
        Jlll.eval("(define counter (atom 0))", env);
        Jlll.eval("(define f1 (future (swap! counter (lambda (x) (+ x 10)))))", env);
        Jlll.eval("(define f2 (future (swap! counter (lambda (x) (+ x 20)))))", env);
        Jlll.eval("(define f3 (future (swap! counter (lambda (x) (+ x 30)))))", env);
        // Wait for all to complete
        Jlll.eval("(deref f1)", env);
        Jlll.eval("(deref f2)", env);
        Jlll.eval("(deref f3)", env);
        // Total should be 60 (10+20+30)
        eval(60, "(deref counter)");
    }

    @Test
    public void testPmapWithExpensiveComputation() throws Exception
    {
        // Test with slightly more complex computation
        Jlll.eval("(define (square x) (* x x))", env);
        Object result = Jlll.eval("(pmap square '(1 2 3 4 5 6 7 8 9 10))", env);
        Cons list = (Cons) result;
        assertEquals(10, list.length());
        assertEquals(1, list.get(0));
        assertEquals(4, list.get(1));
        assertEquals(9, list.get(2));
        assertEquals(16, list.get(3));
        assertEquals(25, list.get(4));
        assertEquals(100, list.get(9));
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
