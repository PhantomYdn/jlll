package ru.ydn.jlll.libs;

import java.util.LinkedHashSet;
import java.util.Set;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * Set data structure primitives.
 *
 * <p>
 * Provides mutable hash-based sets with O(1) membership testing, backed by Java's
 * LinkedHashSet to preserve insertion order.
 * </p>
 * <ul>
 * <li><b>Creation:</b> make-set, set, list-&gt;set</li>
 * <li><b>Predicates:</b> set?, set-empty?, set-contains?</li>
 * <li><b>Access:</b> set-count, set-&gt;list</li>
 * <li><b>Mutation:</b> set-add!, set-remove!, set-clear!</li>
 * <li><b>Set Operations:</b> set-union, set-intersection, set-difference, set-symmetric-difference</li>
 * <li><b>Set Predicates:</b> set-subset?, set-superset?, set-disjoint?, set-equal?</li>
 * <li><b>Iteration:</b> set-for-each, set-map, set-filter</li>
 * </ul>
 */
public class SetLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // set - Create set from varargs
        new Primitive("set", env, "Creates a set from elements. (set 1 2 3) returns a set containing 1, 2, 3.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> set = new LinkedHashSet<>();
                if (values != null && !values.isNull())
                {
                    for (Object elem : values)
                    {
                        set.add(elem);
                    }
                }
                return set;
            }
        };
        // list->set - Convert list to set
        new Primitive("list->set", env, "Converts a list to a set. (list->set '(1 2 2 3)) returns a set with 1, 2, 3.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> set = new LinkedHashSet<>();
                Object listArg = values.get(0);
                if (listArg == null || (listArg instanceof Cons && ((Cons) listArg).isNull()))
                {
                    return set;
                }
                Cons list = (Cons) listArg;
                for (Object elem : list)
                {
                    set.add(elem);
                }
                return set;
            }
        };
        // set-union - Elements in either set
        new Primitive("set-union", env,
                "Returns a new set with elements from both sets. (set-union s1 s2) returns their union.")
        {
            private static final long serialVersionUID = 3L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> s1 = (Set<Object>) values.get(0);
                Set<Object> s2 = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>(s1);
                result.addAll(s2);
                return result;
            }
        };
        // set-intersection - Elements in both sets
        new Primitive("set-intersection", env,
                "Returns a new set with elements in both sets. (set-intersection s1 s2) returns their intersection.")
        {
            private static final long serialVersionUID = 4L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> s1 = (Set<Object>) values.get(0);
                Set<Object> s2 = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>(s1);
                result.retainAll(s2);
                return result;
            }
        };
        // set-difference - Elements in s1 but not s2
        new Primitive("set-difference", env,
                "Returns a new set with elements in s1 but not in s2. (set-difference s1 s2) returns s1 - s2.")
        {
            private static final long serialVersionUID = 5L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> s1 = (Set<Object>) values.get(0);
                Set<Object> s2 = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>(s1);
                result.removeAll(s2);
                return result;
            }
        };
        // set-symmetric-difference - Elements in exactly one set
        new Primitive("set-symmetric-difference", env, "Returns a new set with elements in exactly one of the sets. "
                + "(set-symmetric-difference s1 s2) returns (s1 - s2) ∪ (s2 - s1).")
        {
            private static final long serialVersionUID = 6L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Set<Object> s1 = (Set<Object>) values.get(0);
                Set<Object> s2 = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>();
                // Add elements in s1 but not s2
                for (Object elem : s1)
                {
                    if (!s2.contains(elem))
                    {
                        result.add(elem);
                    }
                }
                // Add elements in s2 but not s1
                for (Object elem : s2)
                {
                    if (!s1.contains(elem))
                    {
                        result.add(elem);
                    }
                }
                return result;
            }
        };
        // set-for-each - Iterate over elements
        new Primitive("set-for-each", env,
                "Applies a procedure to each element for side effects. (set-for-each println s) prints each element.")
        {
            private static final long serialVersionUID = 7L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Set<Object> set = (Set<Object>) values.get(1);
                for (Object elem : set)
                {
                    proc.applyEvaluated(env, elem);
                }
                return Null.NULL;
            }
        };
        // set-map - Transform elements
        new Primitive("set-map", env, "Returns a new set with function applied to each element. "
                + "(set-map (lambda (x) (* x 2)) s) doubles each element.")
        {
            private static final long serialVersionUID = 8L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure proc = (Procedure) values.get(0);
                Set<Object> set = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>();
                for (Object elem : set)
                {
                    Object transformed = proc.applyEvaluated(env, elem);
                    result.add(transformed);
                }
                return result;
            }
        };
        // set-filter - Filter elements
        new Primitive("set-filter", env, "Returns a new set with elements matching predicate. "
                + "(set-filter positive? s) keeps only positive elements.")
        {
            private static final long serialVersionUID = 9L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Procedure pred = (Procedure) values.get(0);
                Set<Object> set = (Set<Object>) values.get(1);
                Set<Object> result = new LinkedHashSet<>();
                for (Object elem : set)
                {
                    Object testResult = pred.applyEvaluated(env, elem);
                    if (Boolean.TRUE.equals(testResult) || (testResult != null && !Null.NULL.equals(testResult)
                            && !Boolean.FALSE.equals(testResult)))
                    {
                        result.add(elem);
                    }
                }
                return result;
            }
        };
    }
    // ========== Simple methods using @JlllName ==========

    /**
     * Creates an empty mutable set.
     * ({@code (make-set)}) returns an empty set.
     *
     * @return a new empty LinkedHashSet
     */
    @JlllName("make-set")
    public Set<Object> makeSet()
    {
        return new LinkedHashSet<>();
    }

    /**
     * Tests if a value is a set.
     * ({@code (set? obj)}) returns true if obj is a Set.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a Set
     */
    @JlllName("set?")
    public Boolean isSet(Object obj)
    {
        return obj instanceof Set;
    }

    /**
     * Tests if a set is empty.
     * ({@code (set-empty? s)}) returns true if the set has no elements.
     *
     * @param set
     *            the set to test
     * @return true if the set is empty
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-empty?")
    public Boolean setEmpty(Set<Object> set)
    {
        return set.isEmpty();
    }

    /**
     * Tests if a set contains an element.
     * ({@code (set-contains? s elem)}) returns true if elem is in the set.
     *
     * @param set
     *            the set
     * @param elem
     *            the element to find
     * @return true if the element is in the set
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-contains?")
    public Boolean setContains(Set<Object> set, Object elem)
    {
        return set.contains(elem);
    }

    /**
     * Returns the number of elements in the set.
     * ({@code (set-count s)}) returns the size.
     *
     * @param set
     *            the set
     * @return number of elements
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-count")
    public Integer setCount(Set<Object> set)
    {
        return set.size();
    }

    /**
     * Converts a set to a list.
     * ({@code (set->list s)}) returns a list of all elements.
     *
     * @param set
     *            the set to convert
     * @return list of elements (order preserved from LinkedHashSet)
     */
    @SuppressWarnings("unchecked")
    @JlllName("set->list")
    public Cons setToList(Set<Object> set)
    {
        if (set.isEmpty())
        {
            return new Cons(null, null);
        }
        return ListUtil.arrayToCons(set.toArray());
    }

    /**
     * Adds an element to the set.
     * ({@code (set-add! s elem)}) adds the element and returns the set.
     *
     * @param set
     *            the set
     * @param elem
     *            the element to add
     * @return the set (for chaining)
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-add!")
    public Set<Object> setAdd(Set<Object> set, Object elem)
    {
        set.add(elem);
        return set;
    }

    /**
     * Removes an element from the set.
     * ({@code (set-remove! s elem)}) removes the element and returns the set.
     *
     * @param set
     *            the set
     * @param elem
     *            the element to remove
     * @return the set (for chaining)
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-remove!")
    public Set<Object> setRemove(Set<Object> set, Object elem)
    {
        set.remove(elem);
        return set;
    }

    /**
     * Removes all elements from the set.
     * ({@code (set-clear! s)}) clears the set and returns it.
     *
     * @param set
     *            the set
     * @return the now-empty set
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-clear!")
    public Set<Object> setClear(Set<Object> set)
    {
        set.clear();
        return set;
    }

    /**
     * Tests if s1 is a subset of s2.
     * ({@code (set-subset? s1 s2)}) returns true if all elements of s1 are in s2.
     *
     * @param s1
     *            the potential subset
     * @param s2
     *            the potential superset
     * @return true if s1 is a subset of s2
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-subset?")
    public Boolean setSubset(Set<Object> s1, Set<Object> s2)
    {
        return s2.containsAll(s1);
    }

    /**
     * Tests if s1 is a superset of s2.
     * ({@code (set-superset? s1 s2)}) returns true if all elements of s2 are in s1.
     *
     * @param s1
     *            the potential superset
     * @param s2
     *            the potential subset
     * @return true if s1 is a superset of s2
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-superset?")
    public Boolean setSuperset(Set<Object> s1, Set<Object> s2)
    {
        return s1.containsAll(s2);
    }

    /**
     * Tests if two sets have no common elements.
     * ({@code (set-disjoint? s1 s2)}) returns true if intersection is empty.
     *
     * @param s1
     *            first set
     * @param s2
     *            second set
     * @return true if the sets are disjoint
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-disjoint?")
    public Boolean setDisjoint(Set<Object> s1, Set<Object> s2)
    {
        for (Object elem : s1)
        {
            if (s2.contains(elem))
            {
                return false;
            }
        }
        return true;
    }

    /**
     * Tests if two sets have the same elements.
     * ({@code (set-equal? s1 s2)}) returns true if they contain exactly the same elements.
     *
     * @param s1
     *            first set
     * @param s2
     *            second set
     * @return true if the sets are equal
     */
    @SuppressWarnings("unchecked")
    @JlllName("set-equal?")
    public Boolean setEqual(Set<Object> s1, Set<Object> s2)
    {
        return s1.equals(s2);
    }
}
