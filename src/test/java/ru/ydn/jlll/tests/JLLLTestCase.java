package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Iterator;
import org.junit.Test;
import junit.framework.AssertionFailedError;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Eof;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.PlainConsole;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionPrimitive;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.io.Marshaller;
import ru.ydn.jlll.util.ListUtil;

public class JLLLTestCase
{
    private final Environment env;

    public static class ReflectionClass
    {
        public int fieldA = 1;
        public static int fieldStaticA = 1;

        public int getFieldA()
        {
            return fieldA;
        }
    }

    public static class ReflectionPrimitiveClass
    {
        @JlllName("testMethod1")
        public int testMethod1()
        {
            return 0;
        }

        @JlllName("testMethod2")
        public int testMethod2(Environment env)
        {
            return 0;
        }

        @JlllName("testMethod3")
        public String testMethod3(Environment env, String toEcho)
        {
            return toEcho;
        }

        @JlllName("testMethod4")
        public int testMethod4(Environment env, String... strings)
        {
            return strings.length;
        }
    }

    public JLLLTestCase()
    {
        env = new Environment(Environment.top);
    }

    /**
     * Helper to bind a PlainConsole with test input to *console*.
     */
    private void bindTestInput(String input)
    {
        env.addBinding(Symbol.CONSOLE,
                new PlainConsole(new PrintWriter(new StringWriter()), new BufferedReader(new StringReader(input))));
    }

    @Test
    public void testTokenizer() throws Exception
    {
        assertEquals(Jlll.prepare("a"), Symbol.intern("a"));
        assertEquals(Jlll.prepare("(a)"), new Cons(Symbol.intern("a")));
        assertEquals(Jlll.prepare("(a b)"), new Cons(Symbol.intern("a"), new Cons(Symbol.intern("b"))));
        assertEquals(Jlll.prepare("(a . b)"), new Cons(Symbol.intern("a"), Symbol.intern("b")));
        assertEquals(Jlll.prepare("'a"), new Cons(Symbol.QUOTE, new Cons(Symbol.intern("a"))));
        assertEquals(Jlll.prepare("`a"), new Cons(Symbol.QUASIQUOTE, new Cons(Symbol.intern("a"))));
        assertEquals(Jlll.prepare(",a"), new Cons(Symbol.UNQUOTE, new Cons(Symbol.intern("a"))));
        assertEquals(Jlll.prepare("(1 () 2)"), ListUtil.toCons(1, Null.NULL, 2));
        assertEquals(Jlll.prepare("(1 ())"), ListUtil.toCons(1, Null.NULL));
        assertEquals(Jlll.prepare("(() ())"), ListUtil.toCons(Null.NULL, Null.NULL));
        assertEquals(Jlll.prepare("\"str\""), "str");
        assertEquals(Jlll.prepare("\"str\\nstr\""), "str\nstr");
        assertEquals(Jlll.prepare("\"str\\\"sstr\""), "str\"sstr");
    }

    @Test
    public void testCons() throws Exception
    {
        assertEquals(new Cons(), Null.NULL);
        assertEquals(new Cons(Symbol.intern("a"), new Cons(Symbol.intern("b"))),
                ListUtil.toCons(Symbol.intern("a"), Symbol.intern("b")));
        assertEquals(new Cons(Null.NULL, new Cons(Null.NULL)), ListUtil.toCons(Null.NULL, Null.NULL));
        Cons cons = (Cons) Jlll.prepare("(a 1 ())");
        Iterator<?> it = cons.iterator();
        assertEquals(it.next(), Symbol.intern("a"));
        assertEquals(it.next(), 1);
        assertEquals(it.next(), Null.NULL);
        cons = (Cons) Jlll.prepare("(() ())");
        assertNotNull(cons.car());
        assertEquals(Null.NULL, cons.car());
        it = cons.iterator();
        assertTrue(it.hasNext());
        assertEquals(Null.NULL, it.next());
        assertTrue(it.hasNext());
        assertEquals(Null.NULL, it.next());
        assertFalse(it.hasNext());
    }
    /*
     * @Test
     * public void testAppend() throws Exception
     * {
     * Cons a = new Cons("a");
     * Cons ab = new Cons("a", new Cons("b"));
     * assertFalse(a.equals(ab));
     * ListUtil.append(a, "b");
     * assertEquals(ab, a);
     *
     * Cons li = Null.NULL;
     * ListUtil.append(li, "a");
     * assertEquals(new Cons("a"), li);
     * assertEquals("(\"a\")", li.toString());
     * }
     */

    @Test
    public void testToString() throws Exception
    {
        assertEquals(Symbol.intern("a").toString(), "a");
        assertEquals(new Cons(Symbol.intern("a")).toString(), "(a)");
        assertEquals(new Cons(Symbol.intern("a"), new Cons(Symbol.intern("b"))).toString(), "(a b)");
        assertEquals(new Cons(Symbol.intern("a"), Symbol.intern("b")).toString(), "(a . b)");
        assertEquals(new Cons("string").toString(), "(\"string\")");
        assertEquals(new Cons("string", "string2").toString(), "(\"string\" . \"string2\")");
        assertEquals(new Cons(Symbol.intern("a"), Null.NULL).toString(), "(a)");
        assertEquals(Jlll.prepare("(1 . ())").toString(), "(1)");
    }

    @Test
    public void testSymbolNaming() throws Exception
    {
        //test in
        assertEquals("name", ((Symbol) Jlll.prepare("name")).getName());
        assertEquals("str str", ((Symbol) Jlll.prepare("str\\sstr")).getName());
        assertEquals("str\nstr", ((Symbol) Jlll.prepare("str\\nstr")).getName());
        assertEquals("str\\str", ((Symbol) Jlll.prepare("str\\\\str")).getName());
        assertEquals("str\\sstr", ((Symbol) Jlll.prepare("str\\\\sstr")).getName());
        assertEquals("str\\ str", ((Symbol) Jlll.prepare("str\\\\\\sstr")).getName());
        //test out
        assertEquals("name", Symbol.intern("name").toString());
        assertEquals("str\\sstr", Symbol.intern("str str").toString());
        assertEquals("str\\nstr", Symbol.intern("str\nstr").toString());
        assertEquals("str\\\\str", Symbol.intern("str\\str").toString());
        assertEquals("str\\\\\\\\str", Symbol.intern("str\\\\str").toString());
    }

    @Test
    public void testComplexToString() throws Exception
    {
        String[] commands = new String[]
        {"()", "(1 () 2)", "a", "(a)", "(a b)", "(a . b)", "(\"string\")", "(\"string\" . \"string2\")",
                "(\"str str\")", "str\\sstr", "str\\nstr", "'a", "`a", ",a", "'\"a\"", "`\"a\"", "!a", "#a", "'!a",
                "!#a", "(quote . a)", "(quote a b)", "(\"str\\\"str\")", "(\"str\\nstr\")", "'(a)", "'(a)", "'(a ,b)",
                "'(a ,@b)", "!a", "#a"};
        for (int i = 0; i < commands.length; i++)
        {
            String string = commands[i];
            Object prepared = Jlll.prepare(string);
            String toString = prepared.toString();
            assertEquals(string, toString);
        }
    }

    @Test
    public void testObjectEval() throws Exception
    {
        eval(1, "1");
        eval(1.0, "1.0");
        eval("string", "\"string\"");
        Object obj = new Object();
        assertEquals(Jlll.eval(obj, env), obj);
    }

    @Test
    public void testReflectionPrimitive() throws Exception
    {
        ReflectionPrimitiveClass tc = new ReflectionPrimitiveClass();
        ReflectionPrimitive.createReflectionPrimitive(env, tc, getMethod(tc.getClass(), "testMethod1"));
        ReflectionPrimitive.createReflectionPrimitive(env, tc, getMethod(tc.getClass(), "testMethod2"));
        eval(0, "(testMethod2)");
        assertException("(testMethod2 123)", JlllException.class);
        ReflectionPrimitive.createReflectionPrimitive(env, tc, getMethod(tc.getClass(), "testMethod3"));
        eval("echo", "(testMethod3 \"echo\")");
        assertException("(testMethod3)", JlllException.class);
        ReflectionPrimitive.createReflectionPrimitive(env, tc, getMethod(tc.getClass(), "testMethod4"));
        eval(0, "(testMethod4)");
        eval(1, "(testMethod4 \"echo\")");
        eval(2, "(testMethod4 \"echo\" \"echo\")");
        assertException("(testMethod4 \"echo\" 2", JlllException.class);
    }

    private Method getMethod(Class<? extends ReflectionPrimitiveClass> clss, String name) throws Exception
    {
        Method[] methods = clss.getMethods();
        for (int i = 0; i < methods.length; i++)
        {
            Method method = methods[i];
            if (name.equals(method.getName()))
                return method;
        }
        return null;
    }

    @Test
    public void testPredefined() throws Exception
    {
        eval(true, "true");
        //        eval("#t", true);
        eval(false, "false");
        //        eval("#f", false);
        //eval("null",null);
        eval(Null.NULL, "null");
    }

    @Test
    public void testKernel() throws Exception
    {
        assertException("a", JlllException.class);
        eval(1, "(define a 1) a");
        eval(1, "(define (pl x) x) (pl 1)");
        assertException("(define (pl x) x) (pl)", JlllException.class);
        eval(6, "(apply + '(1 2 3))");
        eval("abc", "(apply to-string '(abc))");
        eval(Symbol.intern("a"), "'a");
        eval(1, "(defmacro (d x y) `(define ,x ,y)) (d a 1) a");
        eval(1, "(if true 1 2)");
        eval(2, "(if false 1 2)");
        eval(1, "((lambda (x) x) 1)");
        eval(new Cons(1, 2), "(cons 1 2)");
        eval(1, "(begin 1)");
        eval(Symbol.intern("a"), "(car '(a b))");
        eval(new Cons(Symbol.intern("b")), "(cdr '(a b))");
        eval(Symbol.intern("b"), "(cdr '(a . b))");
        eval(Null.NULL, "(cdr '(a))");
        eval(1, "(define a 1) (eval 'a)");
        eval("test", "(concat \"t\" \"e\" \"s\" \"t\")");
        eval(Symbol.intern("greater"), "(cond ((> 3 2) 'greater)((< 3 2) 'less))");
        eval(Symbol.intern("equal"), "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))");
        eval(Symbol.intern("composite"), "(case (* 2 3) ((2 3 5 7) 'prime)((1 4 6 8 9) 'composite))");
        eval(Symbol.intern("consonant"), "(case (car '(c d)) ((a e i o u) 'vowel)(else 'consonant))");
        eval(Jlll.prepare("(\"a\" \"b\")"), "(map to-string '(a b))");
        eval(Jlll.prepare("(a b c)"), "(map (lambda (x) x) '(a b c))");
        eval(Jlll.prepare("(2 3 (4 (5)))"), "(mapall (lambda (x) (+ x 1)) '(1 2 (3 (4))))");
        eval(Jlll.prepare("(3 4)"), "(filter (lambda (x) (> x 2)) '(1 2 3 4))");
        eval("long", "(define (testf x . y) (if (nil? y) \"short\" \"long\")) (testf 1 2)");
        eval("short", "(define (testf x . y) (if (nil? y) \"short\" \"long\")) (testf 1)");
    }

    @Test
    public void testNullHandling() throws Exception
    {
        eval(Null.NULL, "(define a '()) a");
        eval(Jlll.prepare("(())"), "(define (f1 x) (list x)) (f1 ())");
        eval(Jlll.prepare("(() ())"), "(define (f2 x y) (list x y)) (f2 () ())");
        eval(Jlll.prepare("(() ())"), "(list () ())");
        eval(Null.NULL, "()");
        eval(Null.NULL, "'()");
    }

    @Test
    public void testReflection() throws Exception
    {
        eval(new Cons(Symbol.intern("a"), Symbol.intern("b")), "(new 'ru.ydn.jlll.common.Cons 'a 'b)");
        eval(Symbol.intern("b"), "(invoke '(a b c) 'get 1)");
        eval(Symbol.intern("a"), "(invoke-static 'ru.ydn.jlll.common.Symbol 'intern \"a\")");
        eval(1, "(peek (new 'ru.ydn.jlll.tests.JLLLTestCase$ReflectionClass) 'fieldA)");
        eval(1, "(peek-static 'ru.ydn.jlll.tests.JLLLTestCase$ReflectionClass 'fieldStaticA)");
        eval(2, "(define o (new 'ru.ydn.jlll.tests.JLLLTestCase$ReflectionClass)) "
                + "(poke o 'fieldA 2) (peek o 'fieldA)");
        eval(2, "(poke-static 'ru.ydn.jlll.tests.JLLLTestCase$ReflectionClass 'fieldStaticA 2)"
                + "(peek-static 'ru.ydn.jlll.tests.JLLLTestCase$ReflectionClass 'fieldStaticA)");
        eval(true, "(instanceof? '(a b) 'ru.ydn.jlll.common.Cons)");
        eval(false, "(instanceof? '(a b) 'java.util.Date)");
        eval(true, "(instanceof? (new 'java.util.HashMap) 'java.util.Map)");
    }

    @Test
    public void testMarshaller() throws Exception
    {
        Object[] tests = new Object[]
        {"test", new BigInteger("1"), Symbol.intern("symbol"), new Cons(Symbol.intern("cons")),
                new Cons(Symbol.intern("sym1"), Symbol.intern("sym2")), Null.NULL};
        for (int i = 0; i < tests.length; i++)
        {
            Object next = tests[i];
            assertEquals(next, Marshaller.unmarshall(Marshaller.marshall(next)).get(0));
        }
    }

    @Test
    public void testReflectPrimitives() throws Exception
    {
        Environment env = Environment.top;
        assertEquals(((Procedure) env.lookup("new")).apply(env, Cons.class), new Cons());
        assertEquals(
                ((Primitive) env.lookup("new")).applyEvaluated(env, Cons.class, Symbol.intern("a"), Symbol.intern("b")),
                new Cons(Symbol.intern("a"), Symbol.intern("b")));
    }

    @Test
    public void testMath() throws Exception
    {
        Environment env = Environment.top;
        assertEquals(((Procedure) env.lookup("+")).apply(env, 2, 2), 4);
        eval(true, "(< 2 3)");
        eval(false, "(< 3 2)");
        eval(true, "(and (= 2 2) (> 2 1))");
        eval(false, "(and (= 2 2) (< 2 1))");
        eval(Cons.list(Symbol.intern("f"), Symbol.intern("g")), "(and 1 2 'c '(f g))");
        eval(true, "(and)");
        eval(true, "(or (= 2 2) (> 2 1))");
        eval(true, "(or (= 2 2) (< 2 1))");
        eval(false, "(or false false false)");
        eval(false, "(or)");
        eval(5, "(min 5 7 10 15)");
        eval(5, "(max 1 3 4 5)");
    }

    @Test
    public void testInvokeEval() throws Exception
    {
        assertEquals(Jlll.invoke("(+ $1 $2)", env, 2, 2), 4);
        assertEquals(Jlll.invoke("(define a $1)", env, 2), 2);
        assertEquals(Jlll.invoke("$0", env, 1, 2), Cons.list(1, 2));
    }

    @Test
    public void testCnt() throws Exception
    {
        //System.out.println("cnt"+((Procedure)env.lookup("define")).cnt);
        Jlll.eval("(define (nop) false)", env);
        for (int i = 0; i < 10; i++)
        {
            assertEquals(i, ((Procedure) env.lookup("nop")).cnt);
            eval(false, "(nop)");
        }
    }

    @Test
    public void testBetween() throws Exception
    {
        eval(true, "(between 2 5 3)");
        eval(true, "(between 2 5 2)");
        eval(true, "(between 2 5 5)");
        eval(false, "(between 2 5 1)");
        eval(false, "(between 2 5 6)");
    }

    @Test
    public void testComparisonOperators() throws Exception
    {
        // <= tests
        eval(true, "(<= 1 2)");
        eval(true, "(<= 2 2)");
        eval(false, "(<= 3 2)");
        eval(true, "(<= 1.0 2.0)");
        eval(true, "(<= 1.5 1.5)");
        // >= tests
        eval(true, "(>= 2 1)");
        eval(true, "(>= 2 2)");
        eval(false, "(>= 1 2)");
        eval(true, "(>= 2.0 1.0)");
        eval(true, "(>= 1.5 1.5)");
    }

    @Test
    public void testIntegerDivision() throws Exception
    {
        // quotient - truncates toward zero
        eval(3, "(quotient 13 4)");
        eval(-3, "(quotient -13 4)");
        eval(-3, "(quotient 13 -4)");
        eval(3, "(quotient -13 -4)");
        // remainder - sign follows dividend (like Java %)
        eval(1, "(remainder 13 4)");
        eval(-1, "(remainder -13 4)");
        eval(1, "(remainder 13 -4)");
        eval(-1, "(remainder -13 -4)");
        // modulo - sign follows divisor (mathematical modulo)
        eval(1, "(modulo 13 4)");
        eval(3, "(modulo -13 4)");
        eval(-3, "(modulo 13 -4)");
        eval(-1, "(modulo -13 -4)");
    }

    @Test
    public void testExpt() throws Exception
    {
        eval(1024.0, "(expt 2 10)");
        eval(8.0, "(expt 2 3)");
        eval(1.0, "(expt 5 0)");
        eval(0.5, "(expt 2 -1)");
        eval(27.0, "(expt 3 3)");
    }

    @Test
    public void testGcdLcm() throws Exception
    {
        // gcd tests
        eval(6, "(gcd 12 18)");
        eval(1, "(gcd 7 13)");
        eval(5, "(gcd 15 25)");
        eval(6, "(gcd -12 18)");
        eval(6, "(gcd 12 -18)");
        // lcm tests
        eval(12, "(lcm 4 6)");
        eval(60, "(lcm 12 20)");
        eval(91, "(lcm 7 13)");
        eval(0, "(lcm 0 5)");
        eval(0, "(lcm 5 0)");
    }

    @Test
    public void testNumericPredicates() throws Exception
    {
        // zero?
        eval(true, "(zero? 0)");
        eval(true, "(zero? 0.0)");
        eval(false, "(zero? 1)");
        eval(false, "(zero? -1)");
        // positive?
        eval(true, "(positive? 5)");
        eval(true, "(positive? 0.1)");
        eval(false, "(positive? 0)");
        eval(false, "(positive? -1)");
        // negative?
        eval(true, "(negative? -3)");
        eval(true, "(negative? -0.1)");
        eval(false, "(negative? 0)");
        eval(false, "(negative? 1)");
        // even?
        eval(true, "(even? 0)");
        eval(true, "(even? 2)");
        eval(true, "(even? 4)");
        eval(true, "(even? -2)");
        eval(false, "(even? 1)");
        eval(false, "(even? 3)");
        eval(false, "(even? -1)");
        // odd?
        eval(true, "(odd? 1)");
        eval(true, "(odd? 3)");
        eval(true, "(odd? -1)");
        eval(false, "(odd? 0)");
        eval(false, "(odd? 2)");
        eval(false, "(odd? -2)");
    }

    @Test
    public void testTypePredicates() throws Exception
    {
        // number?
        eval(true, "(number? 42)");
        eval(true, "(number? 3.14)");
        eval(false, "(number? \"42\")");
        eval(false, "(number? 'symbol)");
        // integer?
        eval(true, "(integer? 42)");
        eval(false, "(integer? 3.14)");
        eval(false, "(integer? \"42\")");
        // pair? - true for non-empty cons cells
        eval(true, "(pair? '(a . b))");
        eval(true, "(pair? '(a b c))");
        eval(true, "(pair? '(1))");
        eval(true, "(pair? (cons 1 2))");
        eval(false, "(pair? '())");
        eval(false, "(pair? null)");
        eval(false, "(pair? 42)");
        eval(false, "(pair? \"string\")");
        eval(false, "(pair? 'symbol)");
        // atom? - true for non-pairs (including empty list)
        eval(true, "(atom? 'symbol)");
        eval(true, "(atom? 42)");
        eval(true, "(atom? 3.14)");
        eval(true, "(atom? \"string\")");
        eval(true, "(atom? '())");
        eval(true, "(atom? null)");
        eval(false, "(atom? '(a b))");
        eval(false, "(atom? '(a . b))");
        eval(false, "(atom? (cons 1 2))");
    }

    @Test
    public void testSymbolUtilities() throws Exception
    {
        // gensym - generates unique symbols
        Object sym1 = Jlll.eval("(gensym)", env);
        assertTrue(sym1 instanceof Symbol);
        assertTrue(((Symbol) sym1).getName().startsWith("G__"));
        Object sym2 = Jlll.eval("(gensym)", env);
        assertFalse(sym1.equals(sym2)); // Each gensym is unique
        // gensym with prefix
        Object sym3 = Jlll.eval("(gensym \"temp\")", env);
        assertTrue(sym3 instanceof Symbol);
        assertTrue(((Symbol) sym3).getName().startsWith("temp__"));
        // symbol=?
        eval(true, "(symbol=? 'foo 'foo)");
        eval(false, "(symbol=? 'foo 'bar)");
        eval(false, "(symbol=? (gensym \"x\") (gensym \"x\"))"); // Different symbols, each gensym is unique
    }

    @Test
    public void testMathConstants() throws Exception
    {
        // pi
        Object pi = Jlll.eval("pi", env);
        assertTrue(pi instanceof Double);
        assertEquals(Math.PI, ((Double) pi).doubleValue(), 0.0001);
        // e
        Object e = Jlll.eval("e", env);
        assertTrue(e instanceof Double);
        assertEquals(Math.E, ((Double) e).doubleValue(), 0.0001);
        // Use in expressions
        eval(true, "(> pi 3.14)");
        eval(true, "(< pi 3.15)");
        eval(true, "(> e 2.71)");
        eval(true, "(< e 2.72)");
    }

    @Test
    public void testRoundTruncateSign() throws Exception
    {
        // round - Java Math.round uses "round half up" (toward positive infinity)
        eval(4L, "(round 3.5)");
        eval(4L, "(round 3.6)");
        eval(3L, "(round 3.4)");
        eval(-3L, "(round -3.5)"); // -3.5 rounds to -3 (toward positive infinity)
        eval(-3L, "(round -3.4)");
        eval(0L, "(round 0)");
        // truncate - toward zero
        eval(3L, "(truncate 3.7)");
        eval(3L, "(truncate 3.2)");
        eval(-3L, "(truncate -3.7)");
        eval(-3L, "(truncate -3.2)");
        eval(0L, "(truncate 0)");
        eval(0L, "(truncate 0.9)");
        eval(0L, "(truncate -0.9)");
        // sign
        eval(1, "(sign 5)");
        eval(1, "(sign 0.1)");
        eval(0, "(sign 0)");
        eval(0, "(sign 0.0)");
        eval(-1, "(sign -5)");
        eval(-1, "(sign -0.1)");
    }

    @Test
    public void testExceptionHandling() throws Exception
    {
        // === raise and error ===
        assertException("(raise \"test error\")", JlllException.class);
        assertException("(error \"test\" \" error\")", JlllException.class);
        // === exception? predicate ===
        eval(false, "(exception? 42)");
        eval(false, "(exception? \"string\")");
        eval(false, "(exception? 'symbol)");
        // === Basic try/catch ===
        eval("caught", "(try (raise \"error\") (catch e \"caught\"))");
        eval("no error", "(try \"no error\" (catch e \"caught\"))");
        eval("test error", "(try (raise \"test error\") (catch e (exception-message e)))");
        // === try with finally ===
        eval(1, "(define finally-ran 0) (try 1 (finally (set! finally-ran 1))) finally-ran");
        eval(1, "(define finally-ran2 0) (try (raise \"e\") (catch e 1) (finally (set! finally-ran2 1))) finally-ran2");
        // === try without matching catch re-raises ===
        assertException("(try (raise \"error\") (catch \"java.io.IOException\" e \"io\"))", JlllException.class);
        // === Multiple catch clauses ===
        eval("jlll", "(try (raise \"error\") " + "(catch \"java.io.IOException\" e \"io\") "
                + "(catch \"ru.ydn.jlll.common.JlllException\" e \"jlll\") " + "(catch e \"other\"))");
        eval("catchall",
                "(try (raise \"error\") " + "(catch \"java.io.IOException\" e \"io\") " + "(catch e \"catchall\"))");
        // === catch with predicate ===
        eval("matched",
                "(try (raise \"timeout error\") "
                        + "(catch (lambda (e) (string-contains? (exception-message e) \"timeout\")) e \"matched\") "
                        + "(catch e \"other\"))");
        eval("other",
                "(try (raise \"network error\") "
                        + "(catch (lambda (e) (string-contains? (exception-message e) \"timeout\")) e \"matched\") "
                        + "(catch e \"other\"))");
        // === guard basic ===
        eval("caught", "(guard (err (else \"caught\")) (raise \"error\"))");
        eval("no error", "(guard (err (else \"caught\")) \"no error\")");
        // === guard with clauses ===
        eval("string error",
                "(guard (err " + "((string-contains? (exception-message err) \"string\") \"string error\") "
                        + "(else \"other\")) " + "(raise \"string test\"))");
        // === guard re-raises when no match ===
        assertException("(guard (err ((= 1 2) \"never\")) (raise \"error\"))", JlllException.class);
        // === Nested try/catch ===
        eval("inner", "(try " + "(try (raise \"inner\") (catch e \"inner\")) " + "(catch e \"outer\"))");
        eval("outer",
                "(try " + "(try (raise \"inner\") (catch \"java.io.IOException\" e \"io\")) " + "(catch e \"outer\"))");
        // === exception-cause returns null for simple raise ===
        eval(Null.NULL, "(try (raise \"no cause\") (catch e (exception-cause e)))");
        // === exception? in catch handler ===
        eval(true, "(try (raise \"error\") (catch e (exception? e)))");
        // === error with multiple args ===
        eval("Error: 42", "(try (error \"Error: \" 42) (catch e (exception-message e)))");
        // === finally runs even when exception not caught ===
        eval(1, "(define finally-ran3 0) " + "(try " + "  (try (raise \"e\") (finally (set! finally-ran3 1))) "
                + "  (catch e \"caught\")) " + "finally-ran3");
    }

    @Test
    public void testCallCC() throws Exception
    {
        // === Basic escape ===
        eval(4, "(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))");
        // === No escape - normal return ===
        eval(5, "(+ 1 (call/cc (lambda (k) (+ 2 2))))");
        // === Direct return from call/cc ===
        eval(10, "(call/cc (lambda (k) (k 10)))");
        // === Nested arithmetic with escape ===
        eval(7, "(+ 1 (call/cc (lambda (k) (* 2 (k 6)))))");
        // === Value not used after escape ===
        eval(3, "(call/cc (lambda (k) (k 3) (k 5)))");
        // === Escape from nested expressions - continuation returns to call/cc point ===
        // (k 42) returns 42 from call/cc, then (* 2 42) = 84, then (+ 1 84) = 85
        eval(85, "(+ 1 (* 2 (call/cc (lambda (k) (+ 3 (k 42))))))");
        // === Escape to outer continuation skips intermediate computation ===
        eval(42, "(call/cc (lambda (escape) (+ 1 (* 2 (call/cc (lambda (k) (escape 42)))))))");
        // === call-with-current-continuation alias ===
        eval(4, "(+ 1 (call-with-current-continuation (lambda (k) (k 3))))");
    }

    @Test
    public void testSavedContinuation() throws Exception
    {
        // === Save and call - first call inside call/cc ===
        eval(3, "(define saved-k false) " + "(+ 1 (call/cc (lambda (k) (set! saved-k k) (k 2))))");
        // === After first call, continuation is captured - call again ===
        eval(11, "(define saved-k2 false) " + "(define r1 (+ 1 (call/cc (lambda (k) (set! saved-k2 k) (k 2))))) "
                + "(saved-k2 10)");
        // === Call saved continuation multiple times ===
        eval(101, "(define saved-k3 false) " + "(define r1 (+ 1 (call/cc (lambda (k) (set! saved-k3 k) (k 2))))) "
                + "(define r2 (saved-k3 10)) " + "(saved-k3 100)");
    }

    @Test
    public void testNestedCallCC() throws Exception
    {
        // === Outer escape ===
        eval(42, "(call/cc (lambda (outer) " + "  (+ 1 (call/cc (lambda (inner) " + "    (outer 42))))))");
        // === Inner escape ===
        eval(10, "(call/cc (lambda (outer) " + "  (+ 1 (call/cc (lambda (inner) " + "    (inner 9))))))");
        // === Both escape and return ===
        eval(6, "(+ (call/cc (lambda (k1) (k1 2))) " + "   (call/cc (lambda (k2) (k2 4))))");
    }

    @Test
    public void testCallCCWithTryCatch() throws Exception
    {
        // === Continuation should pass through try/catch without being caught ===
        eval(42, "(+ 1 (try " + "  (call/cc (lambda (k) (k 41))) " + "  (catch e \"should not catch continuation\")))");
        // === Error inside call/cc body should be catchable ===
        eval("caught", "(call/cc (lambda (k) " + "  (try " + "    (raise \"error\") " + "  (catch e \"caught\"))))");
        // === Continuation escapes try, finally still runs ===
        eval(1, "(define finally-ran-cc 0) " + "(+ 1 (try " + "  (call/cc (lambda (k) (k 0))) "
                + "  (finally (set! finally-ran-cc 1)))) " + "finally-ran-cc");
    }

    @Test
    public void testCallCCWithGuard() throws Exception
    {
        // === Continuation should pass through guard without being caught ===
        eval(42, "(+ 1 (guard (err (else \"should not catch\")) " + "  (call/cc (lambda (k) (k 41)))))");
    }

    @Test
    public void testLet() throws Exception
    {
        // Basic let binding
        eval(3, "(let ((x 1) (y 2)) (+ x y))");
        eval(10, "(let ((x 10)) x)");
        // Nested let
        eval(1, "(let ((x 1)) (let ((y 2)) x))");
        eval(3, "(let ((x 1)) (let ((y 2)) (+ x y)))");
        // Empty body not allowed - need at least one expression
        assertException("(let ((x 1)))", JlllException.class);
        // Parallel binding semantics - x not visible to y's binding
        assertException("(let ((x 1) (y x)) y)", JlllException.class);
        // Shadowing
        eval(2, "(let ((x 1)) (let ((x 2)) x))");
        // Multiple body expressions
        eval(3, "(let ((x 1)) (set! x 2) (+ x 1))");
    }

    @Test
    public void testLetStar() throws Exception
    {
        // Sequential binding - each can reference previous
        eval(3, "(let* ((x 1) (y (+ x 1)) (z (+ y 1))) z)");
        // a=1, b=1*2=2, c=2*3=6, sum=1+2+6=9
        eval(9, "(let* ((a 1) (b (* a 2)) (c (* b 3))) (+ a b c))");
        // Single binding
        eval(5, "(let* ((x 5)) x)");
        // Empty bindings
        eval(42, "(let* () 42)");
        // Nested let*
        eval(10, "(let* ((x 1)) (let* ((y (+ x 2)) (z (+ y 3))) (+ x y z)))");
    }

    @Test
    public void testWhenUnless() throws Exception
    {
        // when - evaluates body if true
        eval("yes", "(when true \"yes\")");
        eval(false, "(when false \"yes\")");
        eval(3, "(when (> 5 3) (+ 1 2))");
        // Multiple body expressions
        eval(10, "(let ((x 0)) (when true (set! x 5) (set! x 10)) x)");
        // unless - evaluates body if false
        eval("no", "(unless false \"no\")");
        eval(false, "(unless true \"no\")");
        eval(3, "(unless (< 5 3) (+ 1 2))");
    }

    @Test
    public void testDotimes() throws Exception
    {
        // Sum 0+1+2+3+4 = 10
        eval(10, "(let ((sum 0)) (dotimes (i 5) (set! sum (+ sum i))) sum)");
        // Zero iterations
        eval(0, "(let ((sum 0)) (dotimes (i 0) (set! sum (+ sum 1))) sum)");
        // Count iterations
        eval(5, "(let ((count 0)) (dotimes (i 5) (set! count (+ count 1))) count)");
    }

    @Test
    public void testDolist() throws Exception
    {
        // Sum elements
        eval(6, "(let ((sum 0)) (dolist (x '(1 2 3)) (set! sum (+ sum x))) sum)");
        // Empty list
        eval(0, "(let ((sum 0)) (dolist (x '()) (set! sum (+ sum 1))) sum)");
        // Collect strings
        eval("abc", "(let ((result \"\")) (dolist (s '(\"a\" \"b\" \"c\")) (set! result (concat result s))) result)");
    }

    @Test
    public void testWithExceptionHandler() throws Exception
    {
        // Handler is called, exception still propagates
        eval(1, "(let ((handled 0)) " + "(try " + "  (with-exception-handler " + "    (lambda (e) (set! handled 1)) "
                + "    (lambda () (raise \"error\"))) " + "  (catch e handled)))");
        // Handler runs for logging before propagation
        assertException("(with-exception-handler (lambda (e) false) (lambda () (raise \"test\")))",
                JlllException.class);
        // No exception - handler not called
        eval(42, "(with-exception-handler (lambda (e) (error \"should not run\")) (lambda () 42))");
    }

    @Test
    public void testReadLine() throws Exception
    {
        // Setup: bind console with test input
        bindTestInput("hello\nworld\n");
        // Read first line
        eval("hello", "(read-line)");
        // Read second line
        eval("world", "(read-line)");
        // Read at EOF returns EOF object
        Object result = Jlll.eval("(read-line)", env);
        assertEquals(Eof.EOF, result);
        // eof-object? predicate
        bindTestInput("");
        eval(true, "(eof-object? (read-line))");
        eval(false, "(eof-object? \"hello\")");
        eval(false, "(eof-object? 42)");
    }

    @Test
    public void testRead() throws Exception
    {
        // Setup: bind console with JLLL expressions
        bindTestInput("(+ 1 2) 42 'symbol");
        // Read first expression: (+ 1 2)
        Object expr1 = Jlll.eval("(read)", env);
        assertTrue(expr1 instanceof Cons);
        assertEquals(Jlll.prepare("(+ 1 2)"), expr1);
        // Read second expression: 42
        Object expr2 = Jlll.eval("(read)", env);
        assertEquals(42, expr2);
        // Read third expression: 'symbol
        Object expr3 = Jlll.eval("(read)", env);
        assertEquals(Jlll.prepare("'symbol"), expr3);
        // Read at EOF returns EOF object
        Object eof = Jlll.eval("(read)", env);
        assertEquals(Eof.EOF, eof);
        // Test eval of read expression
        bindTestInput("(+ 1 2)");
        eval(3, "(eval (read))");
    }

    @Test
    public void testReadChar() throws Exception
    {
        // Setup: bind console with test input
        bindTestInput("abc");
        // Read characters one at a time
        eval("a", "(read-char)");
        eval("b", "(read-char)");
        eval("c", "(read-char)");
        // Read at EOF returns EOF object
        Object eof = Jlll.eval("(read-char)", env);
        assertEquals(Eof.EOF, eof);
        // Test with special characters
        bindTestInput("\n\t ");
        eval("\n", "(read-char)");
        eval("\t", "(read-char)");
        eval(" ", "(read-char)");
    }

    @Test
    public void testPeekChar() throws Exception
    {
        // Setup: bind console with test input
        bindTestInput("ab");
        // Peek doesn't consume the character
        eval("a", "(peek-char)");
        eval("a", "(peek-char)");
        eval("a", "(peek-char)");
        // read-char consumes it
        eval("a", "(read-char)");
        // Now peek shows next char
        eval("b", "(peek-char)");
        eval("b", "(read-char)");
        // Peek at EOF returns EOF object
        Object eof = Jlll.eval("(peek-char)", env);
        assertEquals(Eof.EOF, eof);
    }

    @Test
    public void testCharReady() throws Exception
    {
        // StringReader always has characters ready (if not empty)
        bindTestInput("abc");
        eval(true, "(char-ready?)");
        // Read all characters
        Jlll.eval("(read-char)", env);
        Jlll.eval("(read-char)", env);
        // Still one char left
        eval(true, "(char-ready?)");
        Jlll.eval("(read-char)", env);
        // After exhausting input, ready status may vary by implementation
        // BufferedReader.ready() can return true even at EOF (per Java docs: ready if next read won't block)
        // So we just verify char-ready? returns a boolean
        Object result = Jlll.eval("(char-ready?)", env);
        assertTrue(result instanceof Boolean);
    }

    @Test
    public void testNewline() throws Exception
    {
        // newline just prints a newline - verify it doesn't throw
        Jlll.eval("(newline)", env);
    }

    @Test
    public void testFileIO() throws Exception
    {
        // Test file-exists? and directory? with known paths
        eval(true, "(file-exists? \"pom.xml\")");
        eval(false, "(file-exists? \"nonexistent-file-xyz.txt\")");
        eval(true, "(directory? \"src\")");
        eval(false, "(directory? \"pom.xml\")");
        // Test current-directory
        Object cwd = Jlll.eval("(current-directory)", env);
        assertTrue(cwd instanceof String);
        assertTrue(((String) cwd).length() > 0);
        // Test path utilities
        eval("a/b/c.txt", "(path-join \"a\" \"b\" \"c.txt\")");
        eval("c.txt", "(path-filename \"/a/b/c.txt\")");
        eval("txt", "(path-extension \"/a/b/c.txt\")");
        eval("", "(path-extension \"/a/b/noext\")");
        // Test directory-list returns a list
        Object files = Jlll.eval("(directory-list \"src\")", env);
        assertTrue(files instanceof Cons);
        // Test slurp/spit with temp file
        String testFile = "target/jlll-test-" + System.currentTimeMillis() + ".txt";
        Jlll.eval("(spit \"" + testFile + "\" \"hello world\")", env);
        eval(true, "(file-exists? \"" + testFile + "\")");
        eval("hello world", "(slurp \"" + testFile + "\")");
        // Test spit with append
        Jlll.eval("(spit \"" + testFile + "\" \"!\" :append true)", env);
        eval("hello world!", "(slurp \"" + testFile + "\")");
        // Test file-size
        Object size = Jlll.eval("(file-size \"" + testFile + "\")", env);
        assertTrue(size instanceof Long);
        assertTrue(((Long) size) > 0);
        // Cleanup
        Jlll.eval("(delete-file \"" + testFile + "\")", env);
        eval(false, "(file-exists? \"" + testFile + "\")");
    }

    @Test
    public void testFilePortIO() throws Exception
    {
        // Test open-input-file, read-line, close-input-port
        String testFile = "target/jlll-port-test-" + System.currentTimeMillis() + ".txt";
        Jlll.eval("(spit \"" + testFile + "\" \"line1\\nline2\\nline3\")", env);
        Jlll.eval("(define port (open-input-file \"" + testFile + "\"))", env);
        eval("line1", "(read-line port)");
        eval("line2", "(read-line port)");
        Jlll.eval("(close-input-port port)", env);
        // Test call-with-input-file
        eval("line1", "(call-with-input-file \"" + testFile + "\" (lambda (p) (read-line p)))");
        // Cleanup
        Jlll.eval("(delete-file \"" + testFile + "\")", env);
    }

    @Test
    public void testInputWithPort() throws Exception
    {
        // Test read-line with explicit port argument
        BufferedReader port = new BufferedReader(new StringReader("line1\nline2"));
        env.addBinding(Symbol.intern("my-port"), port);
        eval("line1", "(read-line my-port)");
        eval("line2", "(read-line my-port)");
        // Test read-char with explicit port argument
        port = new BufferedReader(new StringReader("xy"));
        env.addBinding(Symbol.intern("my-port"), port);
        eval("x", "(read-char my-port)");
        eval("y", "(read-char my-port)");
        // Test peek-char with explicit port argument
        port = new BufferedReader(new StringReader("z"));
        env.addBinding(Symbol.intern("my-port"), port);
        eval("z", "(peek-char my-port)");
        eval("z", "(peek-char my-port)");
        eval("z", "(read-char my-port)");
        // Test char-ready? with explicit port argument
        port = new BufferedReader(new StringReader("data"));
        env.addBinding(Symbol.intern("my-port"), port);
        eval(true, "(char-ready? my-port)");
    }

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }

    private void assertException(String code, Class<JlllException> clss) throws Exception
    {
        try
        {
            Jlll.eval(code, env);
        }
        catch (Exception e)
        {
            //if(e.getClass().isAssignableFrom(clss)) return;
            if (clss.isAssignableFrom(e.getClass()))
                return;
            else
                throw e;
        }
        throw new AssertionFailedError("Exception was not thrown");
    }
}
