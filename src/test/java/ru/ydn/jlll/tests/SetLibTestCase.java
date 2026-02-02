package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;

/**
 * Tests for the SetLib set data structure functions.
 */
public class SetLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Creation ==========

    @Test
    public void testMakeSet() throws Exception
    {
        Object result = Jlll.eval("(make-set)", env);
        assertTrue("make-set should return a Set", result instanceof Set);
        assertEquals(0, ((Set<?>) result).size());
    }

    @Test
    public void testSet() throws Exception
    {
        Object result = Jlll.eval("(set 1 2 3)", env);
        assertTrue("set should return a Set", result instanceof Set);
        Set<?> set = (Set<?>) result;
        assertEquals(3, set.size());
        assertTrue(set.contains(1));
        assertTrue(set.contains(2));
        assertTrue(set.contains(3));
    }

    @Test
    public void testSetDeduplication() throws Exception
    {
        Object result = Jlll.eval("(set 1 2 2 3 3 3)", env);
        Set<?> set = (Set<?>) result;
        assertEquals("set should deduplicate", 3, set.size());
    }

    @Test
    public void testSetEmpty() throws Exception
    {
        Object result = Jlll.eval("(set)", env);
        assertTrue(result instanceof Set);
        assertEquals(0, ((Set<?>) result).size());
    }

    @Test
    public void testListToSet() throws Exception
    {
        Object result = Jlll.eval("(list->set '(1 2 2 3 3 3))", env);
        assertTrue(result instanceof Set);
        Set<?> set = (Set<?>) result;
        assertEquals(3, set.size());
    }

    @Test
    public void testListToSetEmpty() throws Exception
    {
        Object result = Jlll.eval("(list->set '())", env);
        assertTrue(result instanceof Set);
        assertEquals(0, ((Set<?>) result).size());
    }
    // ========== Predicates ==========

    @Test
    public void testSetPredicate() throws Exception
    {
        eval(true, "(set? (make-set))");
        eval(true, "(set? (set 1 2 3))");
        eval(false, "(set? '(1 2 3))");
        eval(false, "(set? 42)");
        eval(false, "(set? (hash-map))");
    }

    @Test
    public void testSetEmptyPredicate() throws Exception
    {
        eval(true, "(set-empty? (make-set))");
        eval(false, "(set-empty? (set 1))");
    }

    @Test
    public void testSetContains() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        eval(true, "(set-contains? s 1)");
        eval(true, "(set-contains? s 2)");
        eval(true, "(set-contains? s 3)");
        eval(false, "(set-contains? s 4)");
        eval(false, "(set-contains? s \"1\")"); // different type
    }
    // ========== Access ==========

    @Test
    public void testSetCount() throws Exception
    {
        eval(0, "(set-count (make-set))");
        eval(1, "(set-count (set 1))");
        eval(3, "(set-count (set 1 2 3))");
    }

    @Test
    public void testSetToList() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        Object result = Jlll.eval("(set->list s)", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
    }

    @Test
    public void testSetToListEmpty() throws Exception
    {
        Object result = Jlll.eval("(set->list (make-set))", env);
        assertTrue(result instanceof Cons);
        assertNull(((Cons) result).car()); // empty list
    }
    // ========== Mutation ==========

    @Test
    public void testSetAdd() throws Exception
    {
        Jlll.eval("(define s (make-set))", env);
        Jlll.eval("(set-add! s 1)", env);
        eval(true, "(set-contains? s 1)");
        eval(1, "(set-count s)");
        Jlll.eval("(set-add! s 2)", env);
        eval(2, "(set-count s)");
        // Adding duplicate should not increase count
        Jlll.eval("(set-add! s 1)", env);
        eval(2, "(set-count s)");
    }

    @Test
    public void testSetRemove() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        eval(3, "(set-count s)");
        Jlll.eval("(set-remove! s 2)", env);
        eval(2, "(set-count s)");
        eval(false, "(set-contains? s 2)");
        // Removing nonexistent element should be ok
        Jlll.eval("(set-remove! s 99)", env);
        eval(2, "(set-count s)");
    }

    @Test
    public void testSetClear() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        eval(3, "(set-count s)");
        Jlll.eval("(set-clear! s)", env);
        eval(0, "(set-count s)");
        eval(true, "(set-empty? s)");
    }
    // ========== Set Operations ==========

    @Test
    public void testSetUnion() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 3 4 5))", env);
        Jlll.eval("(define u (set-union s1 s2))", env);
        eval(5, "(set-count u)");
        eval(true, "(set-contains? u 1)");
        eval(true, "(set-contains? u 5)");
        // Original sets unchanged
        eval(3, "(set-count s1)");
        eval(3, "(set-count s2)");
    }

    @Test
    public void testSetIntersection() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 2 3 4))", env);
        Jlll.eval("(define i (set-intersection s1 s2))", env);
        eval(2, "(set-count i)");
        eval(true, "(set-contains? i 2)");
        eval(true, "(set-contains? i 3)");
        eval(false, "(set-contains? i 1)");
        eval(false, "(set-contains? i 4)");
    }

    @Test
    public void testSetIntersectionEmpty() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2))", env);
        Jlll.eval("(define s2 (set 3 4))", env);
        Jlll.eval("(define i (set-intersection s1 s2))", env);
        eval(0, "(set-count i)");
    }

    @Test
    public void testSetDifference() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 2 3 4))", env);
        Jlll.eval("(define d (set-difference s1 s2))", env);
        eval(1, "(set-count d)");
        eval(true, "(set-contains? d 1)");
        eval(false, "(set-contains? d 2)");
    }

    @Test
    public void testSetSymmetricDifference() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 2 3 4))", env);
        Jlll.eval("(define sd (set-symmetric-difference s1 s2))", env);
        eval(2, "(set-count sd)");
        eval(true, "(set-contains? sd 1)");
        eval(true, "(set-contains? sd 4)");
        eval(false, "(set-contains? sd 2)");
        eval(false, "(set-contains? sd 3)");
    }
    // ========== Set Predicates ==========

    @Test
    public void testSetSubset() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2))", env);
        Jlll.eval("(define s2 (set 1 2 3))", env);
        eval(true, "(set-subset? s1 s2)");
        eval(false, "(set-subset? s2 s1)");
        eval(true, "(set-subset? s1 s1)"); // set is subset of itself
    }

    @Test
    public void testSetSuperset() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 1 2))", env);
        eval(true, "(set-superset? s1 s2)");
        eval(false, "(set-superset? s2 s1)");
    }

    @Test
    public void testSetDisjoint() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2))", env);
        Jlll.eval("(define s2 (set 3 4))", env);
        Jlll.eval("(define s3 (set 2 3))", env);
        eval(true, "(set-disjoint? s1 s2)");
        eval(false, "(set-disjoint? s1 s3)");
    }

    @Test
    public void testSetEqual() throws Exception
    {
        Jlll.eval("(define s1 (set 1 2 3))", env);
        Jlll.eval("(define s2 (set 3 2 1))", env);
        Jlll.eval("(define s3 (set 1 2))", env);
        eval(true, "(set-equal? s1 s2)");
        eval(false, "(set-equal? s1 s3)");
    }
    // ========== Iteration ==========

    @Test
    public void testSetForEach() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        Jlll.eval("(define sum (atom 0))", env);
        Jlll.eval("(set-for-each (lambda (x) (swap! sum (lambda (s) (+ s x)))) s)", env);
        eval(6, "(deref sum)");
    }

    @Test
    public void testSetMap() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3))", env);
        Jlll.eval("(define doubled (set-map (lambda (x) (* x 2)) s))", env);
        eval(3, "(set-count doubled)");
        eval(true, "(set-contains? doubled 2)");
        eval(true, "(set-contains? doubled 4)");
        eval(true, "(set-contains? doubled 6)");
    }

    @Test
    public void testSetMapDeduplication() throws Exception
    {
        // If mapping produces duplicates, they should be deduplicated
        Jlll.eval("(define s (set 1 2 3 4))", env);
        // All elements mapped to same value
        Jlll.eval("(define mapped (set-map (lambda (x) 0) s))", env);
        eval(1, "(set-count mapped)");
    }

    @Test
    public void testSetFilter() throws Exception
    {
        Jlll.eval("(define s (set 1 2 3 4 5))", env);
        Jlll.eval("(define evens (set-filter even? s))", env);
        eval(2, "(set-count evens)");
        eval(true, "(set-contains? evens 2)");
        eval(true, "(set-contains? evens 4)");
        eval(false, "(set-contains? evens 1)");
    }
    // ========== Mixed Key Types ==========

    @Test
    public void testMixedTypes() throws Exception
    {
        Jlll.eval("(define s (set 1 \"hello\" :keyword 'symbol))", env);
        eval(4, "(set-count s)");
        eval(true, "(set-contains? s 1)");
        eval(true, "(set-contains? s \"hello\")");
        eval(true, "(set-contains? s :keyword)");
        eval(true, "(set-contains? s 'symbol)");
    }
    // ========== Helper ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
