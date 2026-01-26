package ru.ydn.jlll.libs;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
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
 * Hash table (associative array) primitives.
 *
 * <p>
 * Provides mutable hash tables with O(1) access, backed by Java's LinkedHashMap
 * to preserve insertion order.
 * </p>
 * <ul>
 * <li><b>Creation:</b> make-hash, hash-map, alist-&gt;hash</li>
 * <li><b>Predicates:</b> hash?, hash-has-key?</li>
 * <li><b>Access:</b> hash-ref, hash-keys, hash-values, hash-count</li>
 * <li><b>Mutation:</b> hash-set!, hash-remove!, hash-update!, hash-clear!</li>
 * <li><b>Conversion:</b> hash-&gt;alist, alist-&gt;hash, hash-merge</li>
 * </ul>
 */
public class HashLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // hash-map: Create hash map from key-value pairs (varargs)
        new Primitive("hash-map", env,
                "Creates a hash map from key-value pairs. (hash-map :a 1 :b 2) returns a hash with keys :a and :b.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<Object, Object> map = new LinkedHashMap<>();
                if (values == null || values.car() == null)
                {
                    return map;
                }
                int len = values.length();
                if (len % 2 != 0)
                {
                    throw new JlllException("hash-map requires an even number of arguments (key-value pairs)");
                }
                Iterator<?> it = values.iterator();
                while (it.hasNext())
                {
                    Object key = it.next();
                    Object value = it.next();
                    map.put(key, value);
                }
                return map;
            }
        };
        // hash-ref: Lookup value with optional default
        new Primitive("hash-ref", env,
                "Gets value for key from hash map. (hash-ref h :a) returns the value or null if missing. "
                        + "(hash-ref h :a 'default) returns 'default if key is missing.")
        {
            private static final long serialVersionUID = 2L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<Object, Object> map = (Map<Object, Object>) values.get(0);
                Object key = values.get(1);
                if (values.length() > 2)
                {
                    Object defaultValue = values.get(2);
                    return map.getOrDefault(key, defaultValue);
                }
                return map.get(key);
            }
        };
        // hash-update!: Update value with function
        new Primitive("hash-update!", env,
                "Updates value at key by applying function. (hash-update! h :count (lambda (x) (+ x 1))) "
                        + "increments the value at :count.")
        {
            private static final long serialVersionUID = 3L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<Object, Object> map = (Map<Object, Object>) values.get(0);
                Object key = values.get(1);
                Procedure fn = (Procedure) values.get(2);
                Object currentValue = map.get(key);
                // Convert null to Null.NULL for JLLL lambda invocation
                if (currentValue == null)
                {
                    currentValue = Null.NULL;
                }
                Object newValue = fn.applyEvaluated(env, currentValue);
                map.put(key, newValue);
                return newValue;
            }
        };
        // hash->alist: Convert hash to association list
        new Primitive("hash->alist", env,
                "Converts hash map to association list. (hash->alist h) returns ((key1 . val1) (key2 . val2) ...).")
        {
            private static final long serialVersionUID = 4L;

            @SuppressWarnings("unchecked")
            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<Object, Object> map = (Map<Object, Object>) values.get(0);
                if (map.isEmpty())
                {
                    return new Cons(null, null);
                }
                Cons result = new Cons(null, null);
                for (Map.Entry<Object, Object> entry : map.entrySet())
                {
                    Cons pair = new Cons(entry.getKey(), entry.getValue());
                    ListUtil.append(result, pair);
                }
                return result;
            }
        };
        // alist->hash: Create hash from association list
        new Primitive("alist->hash", env,
                "Creates hash map from association list. (alist->hash '((:a . 1) (:b . 2))) returns a hash map.")
        {
            private static final long serialVersionUID = 5L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<Object, Object> map = new LinkedHashMap<>();
                Object listArg = values.get(0);
                if (listArg == null || (listArg instanceof Cons && ((Cons) listArg).car() == null))
                {
                    return map;
                }
                Cons alist = (Cons) listArg;
                Iterator<?> it = alist.iterator();
                while (it.hasNext())
                {
                    Object item = it.next();
                    if (!(item instanceof Cons))
                    {
                        throw new JlllException("alist->hash: expected cons pair, got " + item);
                    }
                    Cons pair = (Cons) item;
                    map.put(pair.car(), pair.cdr());
                }
                return map;
            }
        };
    }
    // ========== Simple methods using @JlllName ==========

    /**
     * Creates an empty mutable hash map.
     * ({@code (make-hash)}) returns an empty hash map.
     *
     * @return a new empty LinkedHashMap
     */
    @JlllName("make-hash")
    public Map<Object, Object> makeHash()
    {
        return new LinkedHashMap<>();
    }

    /**
     * Tests if a value is a hash map.
     * ({@code (hash? obj)}) returns true if obj is a Map.
     *
     * @param obj
     *            the object to test
     * @return true if obj is a Map
     */
    @JlllName("hash?")
    public Boolean isHash(Object obj)
    {
        return obj instanceof Map;
    }

    /**
     * Adds or updates an entry in the hash map.
     * ({@code (hash-set! h :key value)}) sets the value and returns it.
     *
     * @param map
     *            the hash map
     * @param key
     *            the key
     * @param value
     *            the value
     * @return the value that was set
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-set!")
    public Object hashSet(Map<Object, Object> map, Object key, Object value)
    {
        map.put(key, value);
        return value;
    }

    /**
     * Removes an entry from the hash map.
     * ({@code (hash-remove! h :key)}) removes the entry and returns the removed value.
     *
     * @param map
     *            the hash map
     * @param key
     *            the key to remove
     * @return the removed value, or null if key was not present
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-remove!")
    public Object hashRemove(Map<Object, Object> map, Object key)
    {
        return map.remove(key);
    }

    /**
     * Tests if a key exists in the hash map.
     * ({@code (hash-has-key? h :key)}) returns true if key exists.
     *
     * @param map
     *            the hash map
     * @param key
     *            the key to check
     * @return true if the key exists
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-has-key?")
    public Boolean hashHasKey(Map<Object, Object> map, Object key)
    {
        return map.containsKey(key);
    }

    /**
     * Returns the keys of the hash map as a list.
     * ({@code (hash-keys h)}) returns a list of all keys.
     *
     * @param map
     *            the hash map
     * @return list of keys
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-keys")
    public Cons hashKeys(Map<Object, Object> map)
    {
        if (map.isEmpty())
        {
            return new Cons(null, null);
        }
        return ListUtil.arrayToCons(map.keySet().toArray());
    }

    /**
     * Returns the values of the hash map as a list.
     * ({@code (hash-values h)}) returns a list of all values.
     *
     * @param map
     *            the hash map
     * @return list of values
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-values")
    public Cons hashValues(Map<Object, Object> map)
    {
        if (map.isEmpty())
        {
            return new Cons(null, null);
        }
        return ListUtil.arrayToCons(map.values().toArray());
    }

    /**
     * Returns the number of entries in the hash map.
     * ({@code (hash-count h)}) returns the size.
     *
     * @param map
     *            the hash map
     * @return number of key-value pairs
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-count")
    public Integer hashCount(Map<Object, Object> map)
    {
        return map.size();
    }

    /**
     * Removes all entries from the hash map.
     * ({@code (hash-clear! h)}) clears the map and returns it.
     *
     * @param map
     *            the hash map
     * @return the now-empty map
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-clear!")
    public Map<Object, Object> hashClear(Map<Object, Object> map)
    {
        map.clear();
        return map;
    }

    /**
     * Merges two hash maps into a new one. Values from the second map override the first.
     * ({@code (hash-merge h1 h2)}) returns a new map with all entries.
     *
     * @param map1
     *            the first hash map
     * @param map2
     *            the second hash map (overrides first)
     * @return a new merged hash map
     */
    @SuppressWarnings("unchecked")
    @JlllName("hash-merge")
    public Map<Object, Object> hashMerge(Map<Object, Object> map1, Map<Object, Object> map2)
    {
        Map<Object, Object> result = new LinkedHashMap<>(map1);
        result.putAll(map2);
        return result;
    }
}
