package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.Iterator;
import org.junit.Test;
import junit.framework.AssertionFailedError;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionPrimitive;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.io.Marshaller;
import ru.ydn.jlll.util.ListUtil;

public class JLLLTestCase
{
    private final Enviroment env;

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
        public int testMethod2(Enviroment env)
        {
            return 0;
        }

        @JlllName("testMethod3")
        public String testMethod3(Enviroment env, String toEcho)
        {
            return toEcho;
        }

        @JlllName("testMethod4")
        public int testMethod4(Enviroment env, String... strings)
        {
            return strings.length;
        }
    }

    public JLLLTestCase()
    {
        env = new Enviroment(Enviroment.top);
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
        Enviroment env = Enviroment.top;
        assertEquals(((Procedure) env.lookup("new")).applay(env, Cons.class), new Cons());
        assertEquals(((Primitive) env.lookup("new")).applayEvaluated(env, Cons.class, Symbol.intern("a"),
                Symbol.intern("b")), new Cons(Symbol.intern("a"), Symbol.intern("b")));
    }

    @Test
    public void testMath() throws Exception
    {
        Enviroment env = Enviroment.top;
        assertEquals(((Procedure) env.lookup("+")).applay(env, 2, 2), 4);
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
