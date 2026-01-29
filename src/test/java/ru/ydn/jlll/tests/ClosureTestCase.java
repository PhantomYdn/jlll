package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;

/**
 * Tests for lexical closure support in JLLL.
 *
 * <p>
 * These tests verify that lambda expressions properly capture their lexical environment,
 * enabling closures to work correctly.
 * </p>
 */
public class ClosureTestCase
{
    private Environment env;

    @Before
    public void setUp() throws Exception
    {
        env = new Environment(Environment.top);
    }

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
    // ==================== Basic Closure Tests ====================

    @Test
    public void testBasicClosure() throws Exception
    {
        // Classic make-adder example
        Jlll.eval("(define (make-adder n) (lambda (x) (+ x n)))", env);
        Jlll.eval("(define add5 (make-adder 5))", env);
        eval(15, "(add5 10)");
    }

    @Test
    public void testClosureWithMultipleBindings() throws Exception
    {
        // Closure captures multiple variables
        Jlll.eval("(define (make-linear a b) (lambda (x) (+ (* a x) b)))", env);
        Jlll.eval("(define f (make-linear 2 3))", env);
        eval(13, "(f 5)"); // 2*5 + 3 = 13
    }

    @Test
    public void testClosureFromLet() throws Exception
    {
        // Closure from let binding
        Jlll.eval("(define get-x (let ((x 42)) (lambda () x)))", env);
        eval(42, "(get-x)");
    }
    // ==================== Stateful Closure Tests ====================

    @Test
    public void testClosureWithState() throws Exception
    {
        // Counter using mutable state
        Jlll.eval("(define (make-counter) (let ((count 0)) (lambda () (set! count (+ count 1)) count)))", env);
        Jlll.eval("(define c (make-counter))", env);
        eval(1, "(c)");
        eval(2, "(c)");
        eval(3, "(c)");
    }

    @Test
    public void testMultipleClosuresIndependent() throws Exception
    {
        // Multiple closures have independent state
        Jlll.eval("(define (make-counter) (let ((count 0)) (lambda () (set! count (+ count 1)) count)))", env);
        Jlll.eval("(define c1 (make-counter))", env);
        Jlll.eval("(define c2 (make-counter))", env);
        eval(1, "(c1)");
        eval(1, "(c2)");
        eval(2, "(c1)");
        eval(2, "(c2)");
    }

    @Test
    public void testClosureMutatesOuterVariable() throws Exception
    {
        // Accumulator pattern
        Jlll.eval("(define (make-accumulator) (let ((sum 0)) (lambda (n) (set! sum (+ sum n)) sum)))", env);
        Jlll.eval("(define acc (make-accumulator))", env);
        eval(5, "(acc 5)");
        eval(15, "(acc 10)");
        eval(18, "(acc 3)");
    }
    // ==================== Currying and Higher-Order Tests ====================

    @Test
    public void testCurrying() throws Exception
    {
        // Currying captures function and first argument
        Jlll.eval("(define (curry f x) (lambda (y) (f x y)))", env);
        Jlll.eval("(define add10 (curry + 10))", env);
        eval(15, "(add10 5)");
    }

    @Test
    public void testNestedClosures() throws Exception
    {
        // Nested closures - each level captures its environment
        Jlll.eval("(define (make-multiplier-factory base) (lambda (factor) (lambda (x) (* base factor x))))", env);
        Jlll.eval("(define times10 (make-multiplier-factory 10))", env);
        Jlll.eval("(define times10x2 (times10 2))", env);
        eval(60, "(times10x2 3)"); // 10 * 2 * 3 = 60
    }

    @Test
    public void testPartialApplication() throws Exception
    {
        // Partial application pattern
        Jlll.eval("(define (partial f . args1) (lambda args2 (apply f (append args1 args2))))", env);
        Jlll.eval("(define add-abc (partial + 1 2 3))", env);
        eval(10, "(add-abc 4)"); // 1 + 2 + 3 + 4 = 10
    }
    // ==================== Constantly and Complement Tests ====================

    @Test
    public void testConstantlyWithClosure() throws Exception
    {
        // After closure fix, constantly can be pure JLLL
        Jlll.eval("(define (my-constantly v) (lambda args v))", env);
        Jlll.eval("(define always42 (my-constantly 42))", env);
        eval(42, "(always42)");
        eval(42, "(always42 'ignored 'args)");
    }

    @Test
    public void testComplementWithClosure() throws Exception
    {
        // After closure fix, complement can be pure JLLL
        // Use a user-defined predicate to avoid issues with special primitives
        Jlll.eval("(define (my-complement pred) (lambda (x) (not (pred x))))", env);
        Jlll.eval("(define (positive-num? x) (> x 0))", env);
        Jlll.eval("(define not-positive? (my-complement positive-num?))", env);
        eval(true, "(not-positive? -5)");
        eval(false, "(not-positive? 5)");
    }
    // ==================== Edge Cases ====================

    @Test
    public void testClosureWithNoCaptures() throws Exception
    {
        // Lambda with no captured variables should still work
        Jlll.eval("(define (make-id) (lambda (x) x))", env);
        Jlll.eval("(define id (make-id))", env);
        eval(42, "(id 42)");
    }

    @Test
    public void testClosureWithShadowing() throws Exception
    {
        // Parameter shadows captured variable
        Jlll.eval("(define x 100)", env);
        Jlll.eval("(define (make-fn) (let ((x 42)) (lambda (x) x)))", env);
        Jlll.eval("(define fn (make-fn))", env);
        eval(5, "(fn 5)"); // Parameter x shadows captured x
    }

    @Test
    public void testClosureInMap() throws Exception
    {
        // Use closure with higher-order functions
        Jlll.eval("(define (make-adder n) (lambda (x) (+ x n)))", env);
        Jlll.eval("(define add3 (make-adder 3))", env);
        Object result = Jlll.eval("(map add3 '(1 2 3))", env);
        assertEquals("(4 5 6)", result.toString());
    }

    @Test
    public void testClosureInFilter() throws Exception
    {
        // Use closure with filter
        Jlll.eval("(define (make-greater-than n) (lambda (x) (> x n)))", env);
        Jlll.eval("(define gt5 (make-greater-than 5))", env);
        Object result = Jlll.eval("(filter gt5 '(1 3 5 7 9))", env);
        assertEquals("(7 9)", result.toString());
    }
    // ==================== Shared State Tests ====================

    @Test
    public void testMultipleClosuresSharingState() throws Exception
    {
        // Multiple closures can share the same captured variable
        Jlll.eval("(define (make-bank-account initial) " + "  (let ((balance initial)) " + "    (list "
                + "      (lambda (amount) (set! balance (+ balance amount)) balance) "
                + "      (lambda (amount) (set! balance (- balance amount)) balance) " + "      (lambda () balance))))",
                env);
        Jlll.eval("(define account (make-bank-account 100))", env);
        Jlll.eval("(define deposit (car account))", env);
        Jlll.eval("(define withdraw (cadr account))", env);
        Jlll.eval("(define check-balance (caddr account))", env);
        eval(150, "(deposit 50)");
        eval(120, "(withdraw 30)");
        eval(120, "(check-balance)");
    }
}
