package ru.ydn.jlll.libs;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSyntaxException;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.util.ListUtil;

/**
 * JSON parsing and generation primitives.
 *
 * <p>
 * Provides functions for converting between JSON strings and JLLL data structures,
 * backed by Google's Gson library.
 * </p>
 *
 * <p>
 * Type mappings:
 * </p>
 * <table>
 * <tr>
 * <th>JSON</th>
 * <th>JLLL</th>
 * </tr>
 * <tr>
 * <td>object</td>
 * <td>hash-map (LinkedHashMap)</td>
 * </tr>
 * <tr>
 * <td>array</td>
 * <td>list (Cons)</td>
 * </tr>
 * <tr>
 * <td>string</td>
 * <td>string</td>
 * </tr>
 * <tr>
 * <td>number (int)</td>
 * <td>Long</td>
 * </tr>
 * <tr>
 * <td>number (float)</td>
 * <td>Double</td>
 * </tr>
 * <tr>
 * <td>boolean</td>
 * <td>Boolean</td>
 * </tr>
 * <tr>
 * <td>null</td>
 * <td>null / Null.NULL</td>
 * </tr>
 * </table>
 *
 * <ul>
 * <li><b>Parsing:</b> json-parse</li>
 * <li><b>Generating:</b> json-stringify</li>
 * <li><b>File operations:</b> json-read-file, json-write-file (defined in json.jlll)</li>
 * </ul>
 */
public class JsonLib extends ReflectionLibrary
{
    private static final Gson gson = new Gson();
    private static final Gson gsonPretty = new GsonBuilder().setPrettyPrinting().create();

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // json-parse: Parse JSON string to JLLL data (with optional :keywords flag)
        new Primitive("json-parse", env,
                "Parses a JSON string to JLLL data. "
                        + "(json-parse \"{\\\"name\\\": \\\"Alice\\\"}\") returns a hash-map. "
                        + "Options: :keywords true converts object keys to keywords.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.car() == null)
                {
                    throw new JlllException("json-parse requires a JSON string argument");
                }
                String jsonString = (String) values.get(0);
                boolean useKeywords = false;
                // Parse optional keyword arguments
                int len = values.length();
                for (int i = 1; i < len; i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= len)
                    {
                        throw new JlllException("json-parse: missing value for option " + key);
                    }
                    Object val = values.get(i + 1);
                    if (key instanceof Keyword && "keywords".equals(((Keyword) key).getName()))
                    {
                        useKeywords = Boolean.TRUE.equals(val);
                    }
                }
                try
                {
                    JsonElement element = JsonParser.parseString(jsonString);
                    return jsonToJlll(element, useKeywords);
                }
                catch (JsonSyntaxException e)
                {
                    throw new JlllException("json-parse: invalid JSON - " + e.getMessage());
                }
            }
        };
        // json-stringify: Convert JLLL data to JSON string (with optional :pretty flag)
        new Primitive("json-stringify", env,
                "Converts JLLL data to a JSON string. "
                        + "(json-stringify (hash-map :name \"Alice\")) returns \"{\\\"name\\\":\\\"Alice\\\"}\". "
                        + "Options: :pretty true for formatted output with indentation.")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.car() == null)
                {
                    throw new JlllException("json-stringify requires a data argument");
                }
                Object data = values.get(0);
                boolean pretty = false;
                // Parse optional keyword arguments
                int len = values.length();
                for (int i = 1; i < len; i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= len)
                    {
                        throw new JlllException("json-stringify: missing value for option " + key);
                    }
                    Object val = values.get(i + 1);
                    if (key instanceof Keyword && "pretty".equals(((Keyword) key).getName()))
                    {
                        pretty = Boolean.TRUE.equals(val);
                    }
                }
                JsonElement element = jlllToJson(data);
                return pretty ? gsonPretty.toJson(element) : gson.toJson(element);
            }
        };
        // json-read-file: Read and parse JSON from file (with optional :keywords flag)
        new Primitive("json-read-file", env,
                "Reads and parses JSON from a file path. " + "(json-read-file \"config.json\") returns JLLL data. "
                        + "Options: :keywords true converts object keys to keywords.")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.car() == null)
                {
                    throw new JlllException("json-read-file requires a file path argument");
                }
                String path = (String) values.get(0);
                boolean useKeywords = false;
                // Parse optional keyword arguments
                int len = values.length();
                for (int i = 1; i < len; i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= len)
                    {
                        throw new JlllException("json-read-file: missing value for option " + key);
                    }
                    Object val = values.get(i + 1);
                    if (key instanceof Keyword && "keywords".equals(((Keyword) key).getName()))
                    {
                        useKeywords = Boolean.TRUE.equals(val);
                    }
                }
                try
                {
                    // Use slurp to read the file content
                    Object content = Jlll.invokeProcedure("slurp", env, path);
                    if (content == null || content instanceof Null)
                    {
                        throw new JlllException("json-read-file: could not read file: " + path);
                    }
                    String jsonString = content.toString();
                    JsonElement element = JsonParser.parseString(jsonString);
                    return jsonToJlll(element, useKeywords);
                }
                catch (JsonSyntaxException e)
                {
                    throw new JlllException("json-read-file: invalid JSON in " + path + " - " + e.getMessage());
                }
            }
        };
        // json-write-file: Write JLLL data as JSON to file (with optional :pretty flag)
        new Primitive("json-write-file", env,
                "Writes JLLL data as JSON to a file path. " + "(json-write-file \"output.json\" data) writes JSON. "
                        + "Options: :pretty true for formatted output with indentation.")
        {
            private static final long serialVersionUID = 4L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.length() < 2)
                {
                    throw new JlllException("json-write-file requires path and data arguments");
                }
                String path = (String) values.get(0);
                Object data = values.get(1);
                boolean pretty = false;
                // Parse optional keyword arguments
                int len = values.length();
                for (int i = 2; i < len; i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= len)
                    {
                        throw new JlllException("json-write-file: missing value for option " + key);
                    }
                    Object val = values.get(i + 1);
                    if (key instanceof Keyword && "pretty".equals(((Keyword) key).getName()))
                    {
                        pretty = Boolean.TRUE.equals(val);
                    }
                }
                JsonElement element = jlllToJson(data);
                String json = pretty ? gsonPretty.toJson(element) : gson.toJson(element);
                // Use spit to write the file content
                Jlll.invokeProcedure("spit", env, path, json);
                return null;
            }
        };
    }

    /**
     * Converts a Gson JsonElement to JLLL data structure.
     *
     * @param element
     *            the JSON element to convert
     * @param useKeywords
     *            if true, object keys become Keywords; if false, they remain Strings
     * @return the corresponding JLLL data structure
     * @throws JlllException
     *             if conversion fails
     */
    private Object jsonToJlll(JsonElement element, boolean useKeywords) throws JlllException
    {
        if (element == null || element.isJsonNull())
        {
            return Null.NULL;
        }
        else if (element.isJsonPrimitive())
        {
            JsonPrimitive prim = element.getAsJsonPrimitive();
            if (prim.isBoolean())
            {
                return prim.getAsBoolean();
            }
            else if (prim.isString())
            {
                return prim.getAsString();
            }
            else if (prim.isNumber())
            {
                // Try to preserve integer vs floating point
                Number num = prim.getAsNumber();
                double d = num.doubleValue();
                long l = num.longValue();
                if (d == l)
                {
                    return l;
                }
                else
                {
                    return d;
                }
            }
        }
        else if (element.isJsonArray())
        {
            JsonArray arr = element.getAsJsonArray();
            if (arr.isEmpty())
            {
                // Empty JSON array becomes JLLL null/()
                // Note: JLLL doesn't distinguish null from empty list
                return Null.NULL;
            }
            Object[] items = new Object[arr.size()];
            for (int i = 0; i < arr.size(); i++)
            {
                items[i] = jsonToJlll(arr.get(i), useKeywords);
            }
            return ListUtil.arrayToCons(items);
        }
        else if (element.isJsonObject())
        {
            JsonObject obj = element.getAsJsonObject();
            Map<Object, Object> map = new LinkedHashMap<>();
            for (Map.Entry<String, JsonElement> entry : obj.entrySet())
            {
                Object key = useKeywords ? Keyword.intern(entry.getKey()) : entry.getKey();
                Object value = jsonToJlll(entry.getValue(), useKeywords);
                map.put(key, value);
            }
            return map;
        }
        throw new JlllException("json-parse: unsupported JSON element type: " + element.getClass());
    }

    /**
     * Converts a JLLL data structure to a Gson JsonElement.
     *
     * @param obj
     *            the JLLL object to convert
     * @return the corresponding JSON element
     * @throws JlllException
     *             if conversion fails
     */
    private JsonElement jlllToJson(Object obj) throws JlllException
    {
        // Check for Null singleton first (JSON null)
        // Note: Null extends Cons, so must check before Cons
        if (obj == null || obj instanceof Null)
        {
            return JsonNull.INSTANCE;
        }
        // Check for empty list (JSON empty array)
        else if (obj instanceof Cons && ((Cons) obj).isNull())
        {
            return new JsonArray();
        }
        else if (obj instanceof Boolean)
        {
            return new JsonPrimitive((Boolean) obj);
        }
        else if (obj instanceof Number)
        {
            return new JsonPrimitive((Number) obj);
        }
        else if (obj instanceof String)
        {
            return new JsonPrimitive((String) obj);
        }
        else if (obj instanceof Keyword)
        {
            // Keywords become strings in JSON
            return new JsonPrimitive(((Keyword) obj).getName());
        }
        else if (obj instanceof Cons)
        {
            Cons list = (Cons) obj;
            JsonArray arr = new JsonArray();
            Iterator<?> it = list.iterator();
            while (it.hasNext())
            {
                arr.add(jlllToJson(it.next()));
            }
            return arr;
        }
        else if (obj instanceof Map)
        {
            @SuppressWarnings("unchecked")
            Map<Object, Object> map = (Map<Object, Object>) obj;
            JsonObject jsonObj = new JsonObject();
            for (Map.Entry<Object, Object> entry : map.entrySet())
            {
                String key;
                if (entry.getKey() instanceof Keyword)
                {
                    key = ((Keyword) entry.getKey()).getName();
                }
                else if (entry.getKey() instanceof String)
                {
                    key = (String) entry.getKey();
                }
                else
                {
                    key = String.valueOf(entry.getKey());
                }
                jsonObj.add(key, jlllToJson(entry.getValue()));
            }
            return jsonObj;
        }
        else
        {
            // Fallback: convert to string
            return new JsonPrimitive(String.valueOf(obj));
        }
    }
}
