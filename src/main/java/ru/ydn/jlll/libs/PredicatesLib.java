package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.common.annotation.JlllName;

/**
 * Type predicates and testing functions.
 *
 * <p>
 * Provides type checking and value testing:
 * </p>
 * <ul>
 * <li><b>null?:</b> tests if value is null/nil</li>
 * <li><b>jlll-bound?:</b> tests if a symbol is bound in the environment</li>
 * </ul>
 *
 * <p>
 * Also loads predicates.jlll which provides additional predicates like
 * list?, number?, string?, symbol?, procedure?, etc.
 * </p>
 */
public class PredicatesLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        Jlll.eval("(load-system-script \"predicates.jlll\")", env);
    }

    /**
     * Tests if a value is null/nil. {@code (null? x)} returns true for null or Null.NULL.
     *
     * @param env
     *            the environment for evaluation
     * @param obj
     *            the unevaluated expression to test
     * @return true if the value is null
     * @throws JlllException
     *             if evaluation fails
     */
    @JlllName(value = "null?", useEvaluated = false)
    public boolean isNull(Environment env, Object obj) throws JlllException
    {
        Object firstArg = obj == null ? null : Evaluator.eval(obj, env);
        return firstArg == null || Null.NULL.equals(firstArg);
    }

    /**
     * Tests if a symbol is bound. {@code (jlll-bound? 'name)} returns true if name has a binding.
     *
     * @param env
     *            the environment to check
     * @param obj
     *            the symbol name to look up
     * @return true if a binding exists
     */
    @JlllName("jlll-bound?")
    public boolean isJlllBound(Environment env, Object obj)
    {
        return env.lookup(obj.toString()) != null;
    }

    /**
     * Tests if a value is a keyword. {@code (keyword? :foo)} returns true.
     *
     * @param obj
     *            the value to test
     * @return true if obj is a Keyword
     */
    @JlllName("keyword?")
    public boolean isKeyword(Object obj)
    {
        return obj instanceof Keyword;
    }

    /**
     * Converts a keyword to a symbol. {@code (keyword->symbol :foo)} returns foo.
     *
     * @param keyword
     *            the keyword to convert
     * @return the corresponding symbol
     */
    @JlllName("keyword->symbol")
    public Symbol keywordToSymbol(Keyword keyword)
    {
        return keyword.toSymbol();
    }

    /**
     * Converts a symbol to a keyword. {@code (symbol->keyword 'foo)} returns :foo.
     *
     * @param symbol
     *            the symbol to convert
     * @return the corresponding keyword
     * @throws JlllException
     *             if conversion fails
     */
    @JlllName("symbol->keyword")
    public Keyword symbolToKeyword(Symbol symbol) throws JlllException
    {
        return Keyword.fromSymbol(symbol);
    }

    /**
     * Returns the name of a keyword. {@code (keyword-name :foo)} returns "foo".
     *
     * @param keyword
     *            the keyword
     * @return the keyword's name as a string
     */
    @JlllName("keyword-name")
    public String keywordName(Keyword keyword)
    {
        return keyword.getName();
    }

    /**
     * Tests if a value is a number. {@code (number? 42)} returns true.
     * Accepts any Java Number type (Integer, Double, Long, etc.).
     *
     * @param obj
     *            the value to test
     * @return true if obj is a Number
     */
    @JlllName("number?")
    public boolean isNumber(Object obj)
    {
        return obj instanceof Number;
    }

    /**
     * Tests if a value is an integer. {@code (integer? 42)} returns true.
     * {@code (integer? 3.14)} returns false.
     *
     * @param obj
     *            the value to test
     * @return true if obj is an Integer or Long
     */
    @JlllName("integer?")
    public boolean isInteger(Object obj)
    {
        return obj instanceof Integer || obj instanceof Long;
    }

    /**
     * Tests if a number is zero. {@code (zero? 0)} returns true.
     *
     * @param n
     *            the number to test
     * @return true if n equals zero
     */
    @JlllName("zero?")
    public boolean isZero(Number n)
    {
        return n.doubleValue() == 0.0;
    }

    /**
     * Tests if a number is positive. {@code (positive? 5)} returns true.
     *
     * @param n
     *            the number to test
     * @return true if n is greater than zero
     */
    @JlllName("positive?")
    public boolean isPositive(Number n)
    {
        return n.doubleValue() > 0.0;
    }

    /**
     * Tests if a number is negative. {@code (negative? -3)} returns true.
     *
     * @param n
     *            the number to test
     * @return true if n is less than zero
     */
    @JlllName("negative?")
    public boolean isNegative(Number n)
    {
        return n.doubleValue() < 0.0;
    }

    /**
     * Tests if an integer is even. {@code (even? 4)} returns true.
     *
     * @param n
     *            the integer to test
     * @return true if n is divisible by 2
     */
    @JlllName("even?")
    public boolean isEven(Integer n)
    {
        return n % 2 == 0;
    }

    /**
     * Tests if an integer is odd. {@code (odd? 3)} returns true.
     *
     * @param n
     *            the integer to test
     * @return true if n is not divisible by 2
     */
    @JlllName("odd?")
    public boolean isOdd(Integer n)
    {
        return n % 2 != 0;
    }
}
