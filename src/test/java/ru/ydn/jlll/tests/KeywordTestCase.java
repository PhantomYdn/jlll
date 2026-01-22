package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Symbol;

/**
 * Tests for Keyword type and related functionality.
 */
public class KeywordTestCase
{
    private final Environment env;

    public KeywordTestCase()
    {
        env = new Environment(Environment.top);
    }

    private Object eval(String code) throws Exception
    {
        return Jlll.eval(code, env);
    }
    // === Keyword Class Tests ===

    @Test
    public void testKeywordIntern() throws JlllException
    {
        Keyword k1 = Keyword.intern("foo");
        Keyword k2 = Keyword.intern("foo");
        assertEquals("Same name should return same instance", k1, k2);
    }

    @Test
    public void testKeywordName() throws JlllException
    {
        Keyword k = Keyword.intern("test");
        assertEquals("test", k.getName());
    }

    @Test
    public void testKeywordToString() throws JlllException
    {
        Keyword k = Keyword.intern("foo");
        assertEquals(":foo", k.toString());
    }

    @Test
    public void testKeywordToSymbol() throws JlllException
    {
        Keyword k = Keyword.intern("foo");
        Symbol s = k.toSymbol();
        assertEquals("foo", s.getName());
    }

    @Test
    public void testKeywordFromSymbol() throws JlllException
    {
        Symbol s = Symbol.intern("bar");
        Keyword k = Keyword.fromSymbol(s);
        assertEquals("bar", k.getName());
    }

    @Test
    public void testKeywordNotEqualSymbol() throws JlllException
    {
        Keyword k = Keyword.intern("foo");
        Symbol s = Symbol.intern("foo");
        assertNotEquals("Keyword should not equal Symbol", k, s);
    }

    @Test(expected = JlllException.class)
    public void testKeywordEmptyNameThrows() throws JlllException
    {
        Keyword.intern("");
    }

    @Test(expected = JlllException.class)
    public void testKeywordDoubleColonThrows() throws JlllException
    {
        Keyword.intern(":foo");
    }
    // === Tokenizer Tests ===

    @Test
    public void testParseKeyword() throws Exception
    {
        Object result = Jlll.prepare(":foo");
        assertTrue("Should parse as Keyword", result instanceof Keyword);
        assertEquals("foo", ((Keyword) result).getName());
    }

    @Test
    public void testParseKeywordWithNumbers() throws Exception
    {
        Object result = Jlll.prepare(":123");
        assertTrue("Should parse :123 as Keyword", result instanceof Keyword);
        assertEquals("123", ((Keyword) result).getName());
    }

    @Test
    public void testParseKeywordWithHyphen() throws Exception
    {
        Object result = Jlll.prepare(":foo-bar");
        assertTrue("Should parse as Keyword", result instanceof Keyword);
        assertEquals("foo-bar", ((Keyword) result).getName());
    }
    // === Evaluator Tests ===

    @Test
    public void testKeywordSelfEvaluating() throws Exception
    {
        Object result = eval(":foo");
        assertTrue("Keyword should be self-evaluating", result instanceof Keyword);
        assertEquals(":foo", result.toString());
    }

    @Test
    public void testKeywordInList() throws Exception
    {
        Object result = eval("(list :a :b :c)");
        assertEquals("(:a :b :c)", result.toString());
    }

    @Test
    public void testKeywordEquality() throws Exception
    {
        // Test via Java since there's no eq? in JLLL
        Keyword k1 = (Keyword) eval(":foo");
        Keyword k2 = (Keyword) eval(":foo");
        assertEquals(k1, k2);
    }

    @Test
    public void testKeywordNotEqualToSymbol() throws Exception
    {
        // Test via Java since there's no eq? in JLLL
        Object k = eval(":foo");
        Object s = eval("'foo");
        assertNotEquals(k, s);
    }
    // === Predicate Tests ===

    @Test
    public void testKeywordPredicate() throws Exception
    {
        assertTrue((Boolean) eval("(keyword? :foo)"));
        assertFalse((Boolean) eval("(keyword? 'foo)"));
        assertFalse((Boolean) eval("(keyword? \"foo\")"));
        assertFalse((Boolean) eval("(keyword? 123)"));
    }

    @Test
    public void testKeywordToSymbolPrimitive() throws Exception
    {
        Object result = eval("(keyword->symbol :test)");
        assertTrue(result instanceof Symbol);
        assertEquals("test", ((Symbol) result).getName());
    }

    @Test
    public void testSymbolToKeywordPrimitive() throws Exception
    {
        Object result = eval("(symbol->keyword 'test)");
        assertTrue(result instanceof Keyword);
        assertEquals("test", ((Keyword) result).getName());
    }

    @Test
    public void testKeywordNamePrimitive() throws Exception
    {
        Object result = eval("(keyword-name :hello)");
        assertEquals("hello", result);
    }
    // === Exlamation (Immediate Evaluation) Tests ===

    @Test
    public void testExlamationEvaluatesImmediately() throws Exception
    {
        Object result = eval("!(+ 1 2)");
        assertEquals(3, result);
    }

    @Test
    public void testExlamationInQuote() throws Exception
    {
        // Quote wraps exlamation, so it becomes data
        Object result = eval("'!(+ 1 2)");
        assertEquals("!(+ 1 2)", result.toString());
    }

    @Test
    public void testExlamationNestedInList() throws Exception
    {
        Object result = eval("(list 'a !(+ 1 2) 'b)");
        assertEquals("(a 3 b)", result.toString());
    }
    // === Optional Parameters with Defaults ===

    @Test
    public void testOptionalParameterWithDefault() throws Exception
    {
        eval("(define (greet (name \"World\")) (concat \"Hello, \" name \"!\"))");
        assertEquals("Hello, World!", eval("(greet)"));
        assertEquals("Hello, Alice!", eval("(greet \"Alice\")"));
    }

    @Test
    public void testMultipleOptionalParameters() throws Exception
    {
        eval("(define (point (x 0) (y 0)) (list x y))");
        assertEquals("(0 0)", eval("(point)").toString());
        assertEquals("(10 0)", eval("(point 10)").toString());
        assertEquals("(10 20)", eval("(point 10 20)").toString());
    }

    @Test
    public void testMixedRequiredAndOptional() throws Exception
    {
        eval("(define (format-name first (last \"Doe\")) (concat first \" \" last))");
        assertEquals("John Doe", eval("(format-name \"John\")"));
        assertEquals("John Smith", eval("(format-name \"John\" \"Smith\")"));
    }
    // === Keyword Arguments at Call Site ===

    @Test
    public void testKeywordArgumentBasic() throws Exception
    {
        eval("(define (greet (name \"World\")) (concat \"Hello, \" name \"!\"))");
        assertEquals("Hello, Bob!", eval("(greet :name \"Bob\")"));
    }

    @Test
    public void testKeywordArgumentWithMultipleParams() throws Exception
    {
        eval("(define (point (x 0) (y 0)) (list x y))");
        assertEquals("(10 0)", eval("(point :x 10)").toString());
        assertEquals("(0 20)", eval("(point :y 20)").toString());
        assertEquals("(10 20)", eval("(point :x 10 :y 20)").toString());
        assertEquals("(10 20)", eval("(point :y 20 :x 10)").toString());
    }

    @Test
    public void testMixedPositionalAndKeyword() throws Exception
    {
        eval("(define (foo a (b 0)) (list a b))");
        assertEquals("(1 2)", eval("(foo 1 2)").toString());
        assertEquals("(1 0)", eval("(foo 1)").toString());
        assertEquals("(1 2)", eval("(foo 1 :b 2)").toString());
        assertEquals("(1 2)", eval("(foo :a 1 :b 2)").toString());
    }

    @Test
    public void testKeywordOverridesPositional() throws Exception
    {
        // Keywords override positional - keyword must come before any conflicting positional
        eval("(define (foo (a 0) (b 0)) (list a b))");
        // Keyword overrides default
        assertEquals("(5 0)", eval("(foo :a 5)").toString());
        // Keyword with positional for other param
        assertEquals("(5 2)", eval("(foo :a 5 2)").toString());
        // Both keywords
        assertEquals("(5 7)", eval("(foo :a 5 :b 7)").toString());
        // Positional only
        assertEquals("(1 2)", eval("(foo 1 2)").toString());
    }
    // === Definition-Time vs Invocation-Time Defaults ===

    @Test
    public void testInvocationTimeDefault() throws Exception
    {
        // Default evaluated at each call
        eval("(define counter 0)");
        eval("(define (next-val (v counter)) (begin (set! counter (+ counter 1)) v))");
        assertEquals(0, eval("(next-val)"));
        assertEquals(1, eval("(next-val)"));
        assertEquals(2, eval("(next-val)"));
    }

    @Test
    public void testDefinitionTimeDefault() throws Exception
    {
        // Default evaluated once at definition
        eval("(define counter 0)");
        eval("(define (fixed-val (v !counter)) v)");
        eval("(set! counter 100)");
        assertEquals(0, eval("(fixed-val)")); // Still 0 from definition time
    }
    // === Parameter-Referencing Defaults ===

    @Test
    public void testParameterReferencingDefault() throws Exception
    {
        eval("(define (make-range start (end (+ start 10))) (list start end))");
        assertEquals("(5 15)", eval("(make-range 5)").toString());
        assertEquals("(5 50)", eval("(make-range 5 50)").toString());
    }
    // === Unknown Keywords ===

    @Test
    public void testUnknownKeywordsAddedToEnvironment() throws Exception
    {
        eval("(define (flexible x) (if (jlll-bound? 'extra) (+ x extra) x))");
        assertEquals(10, eval("(flexible 10)"));
        assertEquals(15, eval("(flexible 10 :extra 5)"));
    }
    // === Rest Parameters with Keywords ===

    @Test
    public void testRestParametersBasic() throws Exception
    {
        // Note: (apply + '()) throws in JLLL, so test with at least one arg
        eval("(define (sum . nums) (apply + nums))");
        assertEquals(6, eval("(sum 1 2 3)"));
        assertEquals(10, eval("(sum 1 2 3 4)"));
    }

    @Test
    public void testRestParametersWithKeyword() throws Exception
    {
        eval("(define (process a . rest) (list a rest))");
        assertEquals("(1 (2 3))", eval("(process 1 2 3)").toString());
        assertEquals("(10 (2 3))", eval("(process :a 10 2 3)").toString());
    }
}
