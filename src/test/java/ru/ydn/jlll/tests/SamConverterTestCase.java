package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.util.SamConverter;

/**
 * Tests for SAM (Single Abstract Method) / functional interface conversion.
 *
 * <p>
 * Tests that JLLL procedures/lambdas can be transparently converted to Java
 * functional interfaces when passed to Java methods via invoke/invoke-static.
 * </p>
 */
public class SamConverterTestCase
{
    private Environment env;

    @Before
    public void setUp() throws Exception
    {
        env = new Environment(Environment.top);
    }
    // ========== SAM Detection Tests ==========

    @Test
    public void testIsFunctionalInterface()
    {
        // Standard functional interfaces
        assertTrue(SamConverter.isFunctionalInterface(Runnable.class));
        assertTrue(SamConverter.isFunctionalInterface(Callable.class));
        assertTrue(SamConverter.isFunctionalInterface(Comparator.class));
        assertTrue(SamConverter.isFunctionalInterface(ActionListener.class));
        assertTrue(SamConverter.isFunctionalInterface(Consumer.class));
        assertTrue(SamConverter.isFunctionalInterface(Supplier.class));
        assertTrue(SamConverter.isFunctionalInterface(Function.class));
        assertTrue(SamConverter.isFunctionalInterface(Predicate.class));
        // Non-functional interfaces (multiple abstract methods)
        assertFalse(SamConverter.isFunctionalInterface(List.class));
        // Note: Comparable IS a functional interface (compareTo is the only abstract method)
        // equals/hashCode/toString come from Object and are not counted
        assertTrue(SamConverter.isFunctionalInterface(Comparable.class));
        // Not interfaces
        assertFalse(SamConverter.isFunctionalInterface(String.class));
        assertFalse(SamConverter.isFunctionalInterface(ArrayList.class));
    }

    @Test
    public void testGetSamMethod()
    {
        assertEquals("run", SamConverter.getSamMethod(Runnable.class).getName());
        assertEquals("call", SamConverter.getSamMethod(Callable.class).getName());
        assertEquals("compare", SamConverter.getSamMethod(Comparator.class).getName());
        assertEquals("actionPerformed", SamConverter.getSamMethod(ActionListener.class).getName());
        assertEquals("accept", SamConverter.getSamMethod(Consumer.class).getName());
        assertEquals("get", SamConverter.getSamMethod(Supplier.class).getName());
        assertEquals("apply", SamConverter.getSamMethod(Function.class).getName());
        assertEquals("test", SamConverter.getSamMethod(Predicate.class).getName());
    }
    // ========== Runnable Conversion Tests ==========

    @Test
    public void testRunnableConversion() throws Exception
    {
        AtomicBoolean executed = new AtomicBoolean(false);
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("flag"), executed);
        // Define a procedure and convert it to Runnable
        Jlll.eval("(define my-runnable (lambda () (invoke flag \"set\" true)))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("my-runnable"));
        assertNotNull(proc);
        assertTrue(proc instanceof ru.ydn.jlll.common.Procedure);
        Runnable runnable = SamConverter.convert(Runnable.class, (ru.ydn.jlll.common.Procedure) proc, env);
        assertNotNull(runnable);
        assertFalse(executed.get());
        runnable.run();
        assertTrue(executed.get());
    }

    @Test
    public void testRunnableViaInvoke() throws Exception
    {
        AtomicBoolean executed = new AtomicBoolean(false);
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("flag"), executed);
        // Create a Thread with a lambda and start it - SAM conversion in constructor
        Jlll.eval("(define t (new 'java.lang.Thread (lambda () (invoke flag \"set\" true))))", env);
        Jlll.eval("(invoke t \"start\")", env);
        Jlll.eval("(invoke t \"join\")", env);
        assertTrue("Lambda should have been executed as Runnable", executed.get());
    }
    // ========== Comparator Conversion Tests ==========

    @Test
    public void testComparatorConversion() throws Exception
    {
        // Sort a list using a JLLL lambda as Comparator
        List<Integer> list = new ArrayList<>();
        list.add(3);
        list.add(1);
        list.add(4);
        list.add(1);
        list.add(5);
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("my-list"), list);
        // Sort descending using a lambda comparator
        Jlll.eval("(invoke-static 'java.util.Collections \"sort\" my-list (lambda (a b) (- b a)))", env);
        assertEquals(5, (int) list.get(0));
        assertEquals(4, (int) list.get(1));
        assertEquals(3, (int) list.get(2));
        assertEquals(1, (int) list.get(3));
        assertEquals(1, (int) list.get(4));
    }

    @Test
    public void testComparatorAscending() throws Exception
    {
        List<Integer> list = new ArrayList<>();
        list.add(3);
        list.add(1);
        list.add(2);
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("nums"), list);
        // Sort ascending
        Jlll.eval("(invoke-static 'java.util.Collections \"sort\" nums (lambda (a b) (- a b)))", env);
        assertEquals(1, (int) list.get(0));
        assertEquals(2, (int) list.get(1));
        assertEquals(3, (int) list.get(2));
    }
    // ========== Consumer Conversion Tests ==========

    @Test
    public void testConsumerConversion() throws Exception
    {
        List<String> collected = new ArrayList<>();
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("results"), collected);
        List<String> items = new ArrayList<>();
        items.add("a");
        items.add("b");
        items.add("c");
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("items"), items);
        // Use forEach with a lambda Consumer
        Jlll.eval("(invoke items \"forEach\" (lambda (x) (invoke results \"add\" x)))", env);
        assertEquals(3, collected.size());
        assertEquals("a", collected.get(0));
        assertEquals("b", collected.get(1));
        assertEquals("c", collected.get(2));
    }
    // ========== Callable Conversion Tests ==========

    @Test
    public void testCallableConversion() throws Exception
    {
        // Define a lambda that returns a value
        Jlll.eval("(define my-callable (lambda () (* 6 7)))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("my-callable"));
        @SuppressWarnings("unchecked")
        Callable<Object> callable = SamConverter.convert(Callable.class, (ru.ydn.jlll.common.Procedure) proc, env);
        Object result = callable.call();
        // JLLL returns Integer for small values
        assertEquals(42, ((Number) result).intValue());
    }
    // ========== Predicate Conversion Tests ==========

    @Test
    public void testPredicateConversion() throws Exception
    {
        // Define a predicate lambda
        Jlll.eval("(define is-even (lambda (x) (= (modulo x 2) 0)))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("is-even"));
        @SuppressWarnings("unchecked")
        Predicate<Object> predicate = SamConverter.convert(Predicate.class, (ru.ydn.jlll.common.Procedure) proc, env);
        // Use Integer values to match JLLL's internal number handling
        assertTrue(predicate.test(2));
        assertTrue(predicate.test(4));
        assertFalse(predicate.test(3));
        assertFalse(predicate.test(5));
    }
    // ========== Function Conversion Tests ==========

    @Test
    public void testFunctionConversion() throws Exception
    {
        // Define a function lambda
        Jlll.eval("(define square (lambda (x) (* x x)))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("square"));
        @SuppressWarnings("unchecked")
        Function<Object, Object> function = SamConverter.convert(Function.class, (ru.ydn.jlll.common.Procedure) proc,
                env);
        // Use Integer values to match JLLL's internal number handling
        assertEquals(4, ((Number) function.apply(2)).intValue());
        assertEquals(9, ((Number) function.apply(3)).intValue());
        assertEquals(16, ((Number) function.apply(4)).intValue());
    }
    // ========== ActionListener Conversion Tests ==========

    @Test
    public void testActionListenerConversion() throws Exception
    {
        AtomicReference<String> lastCommand = new AtomicReference<>();
        env.addBinding(ru.ydn.jlll.common.Symbol.intern("last-cmd"), lastCommand);
        // Define an action listener lambda
        Jlll.eval("(define my-listener (lambda (event) "
                + "(invoke last-cmd \"set\" (invoke event \"getActionCommand\"))))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("my-listener"));
        ActionListener listener = SamConverter.convert(ActionListener.class, (ru.ydn.jlll.common.Procedure) proc, env);
        // Simulate action event
        ActionEvent event = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "test-command");
        listener.actionPerformed(event);
        assertEquals("test-command", lastCommand.get());
    }
    // ========== Supplier Conversion Tests ==========

    @Test
    public void testSupplierConversion() throws Exception
    {
        // Define a supplier lambda
        Jlll.eval("(define get-value (lambda () \"hello world\"))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("get-value"));
        @SuppressWarnings("unchecked")
        Supplier<Object> supplier = SamConverter.convert(Supplier.class, (ru.ydn.jlll.common.Procedure) proc, env);
        assertEquals("hello world", supplier.get());
    }
    // ========== Cache Tests ==========

    @Test
    public void testWrapperClassCaching() throws Exception
    {
        // Convert two different procedures to Runnable - should use same wrapper class
        Jlll.eval("(define r1 (lambda () null))", env);
        Jlll.eval("(define r2 (lambda () null))", env);
        Object proc1 = env.lookup(ru.ydn.jlll.common.Symbol.intern("r1"));
        Object proc2 = env.lookup(ru.ydn.jlll.common.Symbol.intern("r2"));
        Runnable runnable1 = SamConverter.convert(Runnable.class, (ru.ydn.jlll.common.Procedure) proc1, env);
        Runnable runnable2 = SamConverter.convert(Runnable.class, (ru.ydn.jlll.common.Procedure) proc2, env);
        // Both should be instances of the same generated class
        assertEquals(runnable1.getClass(), runnable2.getClass());
    }
    // ========== Error Cases ==========

    @Test(expected = ru.ydn.jlll.common.JlllException.class)
    public void testNonFunctionalInterfaceThrows() throws Exception
    {
        Jlll.eval("(define proc (lambda () null))", env);
        Object proc = env.lookup(ru.ydn.jlll.common.Symbol.intern("proc"));
        // List is not a functional interface - should throw
        SamConverter.convert(List.class, (ru.ydn.jlll.common.Procedure) proc, env);
    }
}
