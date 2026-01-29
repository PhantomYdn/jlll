package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Test;
import ru.ydn.jlll.common.CapturingConsole;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.TraceContext;

/**
 * Tests for the debugging and development tools.
 */
public class DebugTestCase
{
    private final Environment env;

    public DebugTestCase()
    {
        env = new Environment(Environment.top);
    }

    @After
    public void tearDown()
    {
        // Ensure tracing is disabled after each test
        TraceContext.disable();
    }
    // ========== Functional Utilities ==========

    @Test
    public void testIdentity() throws Exception
    {
        eval(42, "(identity 42)");
        eval("hello", "(identity \"hello\")");
        eval(true, "(identity true)");
        eval(false, "(identity false)");
        // Identity with list
        Object result = Jlll.eval("(identity '(1 2 3))", env);
        assertTrue(result instanceof Cons);
        assertEquals(3, ((Cons) result).length());
        // Identity with null returns Null.NULL (empty list)
        Object nullResult = Jlll.eval("(identity null)", env);
        assertTrue("null should be Null instance", nullResult instanceof ru.ydn.jlll.common.Null);
    }

    @Test
    public void testIdentityInFilter() throws Exception
    {
        // In JLLL, false is treated as truthy (only null/() is falsy)
        // So (filter identity '(1 false 2 null 3)) keeps all non-null values
        Object result = Jlll.eval("(filter identity '(1 false 2 3))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        // All values are kept because identity returns them as-is
        // and filter keeps truthy values (everything except null/())
        assertEquals(4, list.length());
    }

    @Test
    public void testConstantly() throws Exception
    {
        // Basic constantly - returns same value regardless of args
        eval(42, "((constantly 42))");
        eval(42, "((constantly 42) 1 2 3)");
        eval(42, "((constantly 42) 'ignored 'args 'here)");
        // With different types
        eval("hello", "((constantly \"hello\") 1 2 3)");
        // null is also valid
        Object nullResult = Jlll.eval("((constantly null) 1)", env);
        assertTrue(nullResult instanceof ru.ydn.jlll.common.Null);
    }

    @Test
    public void testConstantlyWithMap() throws Exception
    {
        // (map (constantly 0) '(a b c)) should return (0 0 0)
        Object result = Jlll.eval("(map (constantly 0) '(a b c))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(0, list.get(0));
        assertEquals(0, list.get(1));
        assertEquals(0, list.get(2));
    }

    @Test
    public void testComplement() throws Exception
    {
        // (complement null?) is like not-null?
        eval(true, "((complement null?) '(1 2 3))");
        eval(false, "((complement null?) '())");
        eval(false, "((complement null?) null)");
        // With positive? - note: positive? needs to be defined or use a lambda
        Jlll.eval("(define (positive? x) (> x 0))", env);
        eval(false, "((complement positive?) 5)");
        eval(true, "((complement positive?) -5)");
        eval(true, "((complement positive?) 0)");
    }

    @Test
    public void testComplementInFilter() throws Exception
    {
        // Note: In a quoted list, 'null' is the symbol null, not the null value
        // So (filter (complement null?) '(1 null 2 () 3)) keeps 1, null (symbol), 2, 3
        // and filters out () (empty list is null)
        Object result = Jlll.eval("(filter (complement null?) '(1 null 2 () 3))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        // Only () is filtered out, 'null symbol stays
        assertEquals(4, list.length());
    }
    // ========== type-of ==========

    @Test
    public void testTypeOf() throws Exception
    {
        eval("Number", "(type-of 42)");
        eval("Number", "(type-of 3.14)");
        eval("String", "(type-of \"hello\")");
        eval("Boolean", "(type-of true)");
        eval("Boolean", "(type-of false)");
        eval("Symbol", "(type-of 'foo)");
        eval("List", "(type-of '(1 2 3))");
        // Both '() and null are Null.NULL in JLLL
        eval("Nil", "(type-of '())");
        eval("Nil", "(type-of null)");
        eval("Keyword", "(type-of :keyword)");
    }

    @Test
    public void testTypeOfProcedures() throws Exception
    {
        eval("Procedure", "(type-of (lambda (x) x))");
        eval("Primitive", "(type-of car)");
        // Macro
        Jlll.eval("(defmacro (test-macro x) x)", env);
        eval("Macro", "(type-of test-macro)");
    }
    // ========== tap ==========

    @Test
    public void testTapReturnsValue() throws Exception
    {
        // tap should return its first argument
        eval(42, "(tap 42)");
        eval("hello", "(tap \"hello\")");
    }

    @Test
    public void testTapInPipeline() throws Exception
    {
        // tap can be used in a pipeline to inspect values
        // (map (lambda (x) (tap (* x 2))) '(1 2 3)) should return (2 4 6)
        // The tap just prints and passes through
        Object result = Jlll.eval("(map (lambda (x) (tap (* x 2))) '(1 2 3))", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(2, list.get(0));
        assertEquals(4, list.get(1));
        assertEquals(6, list.get(2));
    }
    // ========== assert ==========

    @Test
    public void testAssertPasses() throws Exception
    {
        // Assert with true condition should pass and return true
        eval(true, "(assert true)");
        eval(true, "(assert (> 5 3))");
        eval(true, "(assert (equal? 2 2))");
        eval(true, "(assert '(non-empty-list))");
    }

    @Test
    public void testAssertFailsSimple() throws Exception
    {
        try
        {
            Jlll.eval("(assert false)", env);
            fail("Expected assertion to fail");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("Assertion failed"));
        }
    }

    @Test
    public void testAssertFailsWithMessage() throws Exception
    {
        try
        {
            Jlll.eval("(assert false \"custom message\")", env);
            fail("Expected assertion to fail");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("custom message"));
        }
    }

    @Test
    public void testAssertFailsWithConcatenatedMessage() throws Exception
    {
        Jlll.eval("(define x -5)", env);
        try
        {
            Jlll.eval("(assert (> x 0) \"x must be positive, got: \" x)", env);
            fail("Expected assertion to fail");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("x must be positive"));
            assertTrue(e.getMessage().contains("-5"));
        }
    }

    @Test
    public void testAssertWithNil() throws Exception
    {
        // nil/null should fail assertion
        try
        {
            Jlll.eval("(assert null)", env);
            fail("Expected assertion to fail for null");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("Assertion failed"));
        }
        // Empty list should also fail
        try
        {
            Jlll.eval("(assert '())", env);
            fail("Expected assertion to fail for empty list");
        }
        catch (Exception e)
        {
            assertTrue(e.getMessage().contains("Assertion failed"));
        }
    }
    // ========== inspect ==========

    @Test
    public void testInspectNumber() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        // Inspect returns the value unchanged
        Object result = Jlll.eval("(inspect 42)", env);
        assertEquals(42, result);
        // Check output contains type info
        String output = capture.getCapturedOutput();
        assertTrue("Should show type", output.contains("Type: Number"));
        assertTrue("Should show value", output.contains("42"));
    }

    @Test
    public void testInspectList() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        Object result = Jlll.eval("(inspect '(1 2 3))", env);
        assertTrue(result instanceof Cons);
        String output = capture.getCapturedOutput();
        assertTrue("Should show type", output.contains("Type: List"));
        assertTrue("Should show length", output.contains("Length: 3"));
    }

    @Test
    public void testInspectProcedure() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        Jlll.eval("(define (my-func x) (* x 2))", env);
        Jlll.eval("(inspect my-func)", env);
        String output = capture.getCapturedOutput();
        assertTrue("Should show type", output.contains("Type: Procedure"));
        assertTrue("Should show description", output.contains("Description:"));
    }

    @Test
    public void testInspectJavaObject() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        Jlll.eval("(inspect (new 'java.util.ArrayList))", env);
        String output = capture.getCapturedOutput();
        assertTrue("Should show Java class", output.contains("Java Class:"));
        assertTrue("Should show ArrayList", output.contains("ArrayList"));
        assertTrue("Should show methods", output.contains("Methods:"));
        assertTrue("Should include add method", output.contains("add"));
    }

    @Test
    public void testInspectNull() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        Jlll.eval("(inspect null)", env);
        String output = capture.getCapturedOutput();
        assertTrue("Should show Nil type", output.contains("Type: Nil"));
        assertTrue("Should show null value", output.contains("Value: null"));
    }
    // ========== trace ==========

    @Test
    public void testTraceEnableDisable() throws Exception
    {
        // Initially tracing is disabled
        eval(false, "(traced?)");
        // Enable tracing
        eval(true, "(trace)");
        eval(true, "(traced?)");
        // Disable tracing
        eval(false, "(untrace)");
        eval(false, "(traced?)");
    }

    @Test
    public void testTraceOutput() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        // Define a simple function
        Jlll.eval("(define (square x) (* x x))", env);
        // Enable tracing and call the function
        Jlll.eval("(trace)", env);
        Jlll.eval("(square 5)", env);
        Jlll.eval("(untrace)", env);
        // Check trace output
        String output = capture.getCapturedOutput();
        assertTrue("Trace should show (square 5) entry", output.contains("TRACE: (square 5)"));
        assertTrue("Trace should show square => result", output.contains("square => 25"));
        // Should also trace the internal (* x x) call
        assertTrue("Trace should show (* x x) call", output.contains("TRACE: (* x x)"));
    }

    @Test
    public void testTraceIndentation() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        // Define nested functions
        Jlll.eval("(define (double x) (* x 2))", env);
        Jlll.eval("(define (quad x) (double (double x)))", env);
        // Enable tracing and call
        Jlll.eval("(trace)", env);
        Jlll.eval("(quad 3)", env);
        Jlll.eval("(untrace)", env);
        // Check that inner calls are indented
        String output = capture.getCapturedOutput();
        assertTrue("Trace output should contain indentation", output.contains("  TRACE:"));
    }

    @Test
    public void testTraceRecursion() throws Exception
    {
        // Set up capturing console
        CapturingConsole capture = new CapturingConsole();
        env.addBinding(Symbol.CONSOLE, capture);
        // Define factorial
        Jlll.eval("(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))", env);
        // Enable tracing and call
        Jlll.eval("(trace)", env);
        Object result = Jlll.eval("(fact 4)", env);
        Jlll.eval("(untrace)", env);
        // Check result
        assertEquals(24, result);
        // Check that we have multiple fact calls in trace
        String output = capture.getCapturedOutput();
        int factCount = output.split("TRACE: \\(fact").length - 1;
        assertTrue("Should trace multiple recursive fact calls", factCount >= 4);
    }
    // ========== Helper methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
