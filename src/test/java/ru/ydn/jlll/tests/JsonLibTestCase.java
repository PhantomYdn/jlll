package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.File;
import java.nio.file.Files;
import java.util.Map;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Null;

/**
 * Tests for the JsonLib JSON parsing and generation functions.
 */
public class JsonLibTestCase
{
    private Environment env;
    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== json-parse: Primitives ==========

    @Test
    public void testParseString() throws Exception
    {
        eval("hello", "(json-parse \"\\\"hello\\\"\")");
    }

    @Test
    public void testParseInteger() throws Exception
    {
        eval(42L, "(json-parse \"42\")");
    }

    @Test
    public void testParseNegativeInteger() throws Exception
    {
        eval(-123L, "(json-parse \"-123\")");
    }

    @Test
    public void testParseFloat() throws Exception
    {
        eval(3.14, "(json-parse \"3.14\")");
    }

    @Test
    public void testParseBooleanTrue() throws Exception
    {
        eval(true, "(json-parse \"true\")");
    }

    @Test
    public void testParseBooleanFalse() throws Exception
    {
        eval(false, "(json-parse \"false\")");
    }

    @Test
    public void testParseNull() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"null\")", env);
        assertTrue("null should parse to Null.NULL", result instanceof Null);
    }
    // ========== json-parse: Arrays ==========

    @Test
    public void testParseEmptyArray() throws Exception
    {
        // Empty JSON array becomes JLLL null (they're equivalent in JLLL)
        Object result = Jlll.eval("(json-parse \"[]\")", env);
        assertTrue("Empty array should be Null", result instanceof Null);
    }

    @Test
    public void testParseSimpleArray() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"[1, 2, 3]\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(3, list.length());
        assertEquals(1L, list.get(0));
        assertEquals(2L, list.get(1));
        assertEquals(3L, list.get(2));
    }

    @Test
    public void testParseMixedArray() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"[1, \\\"hello\\\", true, null]\")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(4, list.length());
        assertEquals(1L, list.get(0));
        assertEquals("hello", list.get(1));
        assertEquals(true, list.get(2));
        assertTrue(list.get(3) instanceof Null);
    }
    // ========== json-parse: Objects ==========

    @Test
    public void testParseEmptyObject() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"{}\")", env);
        assertTrue(result instanceof Map);
        assertEquals(0, ((Map<?, ?>) result).size());
    }

    @Test
    public void testParseSimpleObject() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"{\\\"name\\\": \\\"Alice\\\", \\\"age\\\": 30}\")", env);
        assertTrue(result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals(2, map.size());
        assertEquals("Alice", map.get("name"));
        assertEquals(30L, map.get("age"));
    }

    @Test
    public void testParseObjectWithKeywords() throws Exception
    {
        Object result = Jlll.eval("(json-parse \"{\\\"name\\\": \\\"Alice\\\"}\" :keywords true)", env);
        assertTrue(result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals(1, map.size());
        // Key should be a Keyword, not a String
        assertTrue("Key should be Keyword with :keywords option", map.containsKey(Keyword.intern("name")));
        assertEquals("Alice", map.get(Keyword.intern("name")));
    }
    // ========== json-parse: Nested Structures ==========

    @Test
    public void testParseNestedObject() throws Exception
    {
        String json = "\"{\\\"person\\\": {\\\"name\\\": \\\"Bob\\\", \\\"age\\\": 25}}\"";
        Object result = Jlll.eval("(json-parse " + json + ")", env);
        assertTrue(result instanceof Map);
        Map<?, ?> outer = (Map<?, ?>) result;
        assertTrue(outer.get("person") instanceof Map);
        Map<?, ?> person = (Map<?, ?>) outer.get("person");
        assertEquals("Bob", person.get("name"));
        assertEquals(25L, person.get("age"));
    }

    @Test
    public void testParseArrayOfObjects() throws Exception
    {
        String json = "\"[{\\\"id\\\": 1}, {\\\"id\\\": 2}]\"";
        Object result = Jlll.eval("(json-parse " + json + ")", env);
        assertTrue(result instanceof Cons);
        Cons list = (Cons) result;
        assertEquals(2, list.length());
        assertTrue(list.get(0) instanceof Map);
        assertEquals(1L, ((Map<?, ?>) list.get(0)).get("id"));
        assertEquals(2L, ((Map<?, ?>) list.get(1)).get("id"));
    }
    // ========== json-parse: Error Handling ==========

    @Test(expected = JlllException.class)
    public void testParseInvalidJson() throws Exception
    {
        Jlll.eval("(json-parse \"not valid json\")", env);
    }

    @Test(expected = JlllException.class)
    public void testParseMissingArgument() throws Exception
    {
        Jlll.eval("(json-parse)", env);
    }
    // ========== json-stringify: Primitives ==========

    @Test
    public void testStringifyString() throws Exception
    {
        eval("\"hello\"", "(json-stringify \"hello\")");
    }

    @Test
    public void testStringifyInteger() throws Exception
    {
        eval("42", "(json-stringify 42)");
    }

    @Test
    public void testStringifyFloat() throws Exception
    {
        eval("3.14", "(json-stringify 3.14)");
    }

    @Test
    public void testStringifyBoolean() throws Exception
    {
        eval("true", "(json-stringify true)");
        eval("false", "(json-stringify false)");
    }

    @Test
    public void testStringifyNull() throws Exception
    {
        // In JLLL, null and '() are equivalent (both are Null.NULL)
        // They both become JSON null
        eval("null", "(json-stringify null)");
    }

    @Test
    public void testStringifyNullValue() throws Exception
    {
        // Roundtrip: JSON null -> JLLL null -> JSON null
        eval("null", "(json-stringify (json-parse \"null\"))");
    }
    // ========== json-stringify: Lists ==========

    @Test
    public void testStringifyEmptyList() throws Exception
    {
        // In JLLL, '() is equivalent to null (both are Null.NULL)
        // They both become JSON null (not an empty array)
        // To get an empty JSON array, use (list) which creates a proper Cons
        eval("null", "(json-stringify '())");
    }

    @Test
    public void testStringifyEmptyListFromList() throws Exception
    {
        // (list) with no args creates a list containing nothing
        // But in JLLL this still evaluates to '() / null
        // So there's no way to create an "empty array" distinct from null
        eval("null", "(json-stringify (list))");
    }

    @Test
    public void testStringifySimpleList() throws Exception
    {
        eval("[1,2,3]", "(json-stringify '(1 2 3))");
    }

    @Test
    public void testStringifyMixedList() throws Exception
    {
        eval("[1,\"hello\",true]", "(json-stringify (list 1 \"hello\" true))");
    }
    // ========== json-stringify: Hash Maps ==========

    @Test
    public void testStringifyEmptyHash() throws Exception
    {
        eval("{}", "(json-stringify (make-hash))");
    }

    @Test
    public void testStringifyHashWithStringKeys() throws Exception
    {
        Object result = Jlll.eval("(json-stringify (hash-map \"name\" \"Alice\" \"age\" 30))", env);
        String json = (String) result;
        // Order may vary, just check it contains the right parts
        assertTrue(json.contains("\"name\":\"Alice\"") || json.contains("\"name\": \"Alice\""));
        assertTrue(json.contains("\"age\":30") || json.contains("\"age\": 30"));
    }

    @Test
    public void testStringifyHashWithKeywordKeys() throws Exception
    {
        Object result = Jlll.eval("(json-stringify (hash-map :name \"Alice\"))", env);
        String json = (String) result;
        // Keyword :name should become "name" in JSON
        assertTrue(json.contains("\"name\":\"Alice\"") || json.contains("\"name\": \"Alice\""));
    }
    // ========== json-stringify: Pretty Printing ==========

    @Test
    public void testStringifyPretty() throws Exception
    {
        Object result = Jlll.eval("(json-stringify (hash-map :a 1) :pretty true)", env);
        String json = (String) result;
        // Pretty output should have newlines
        assertTrue("Pretty output should have newlines", json.contains("\n"));
    }
    // ========== json-stringify: Nested Structures ==========

    @Test
    public void testStringifyNestedStructure() throws Exception
    {
        Jlll.eval("(define data (hash-map :items '(1 2 3) :meta (hash-map :count 3)))", env);
        Object result = Jlll.eval("(json-stringify data)", env);
        String json = (String) result;
        assertTrue(json.contains("\"items\":[1,2,3]") || json.contains("\"items\": [1, 2, 3]"));
    }
    // ========== Roundtrip Tests ==========

    @Test
    public void testRoundtripSimpleObject() throws Exception
    {
        String original = "\"{\\\"name\\\": \\\"Alice\\\", \\\"age\\\": 30}\"";
        Jlll.eval("(define parsed (json-parse " + original + "))", env);
        Jlll.eval("(define stringified (json-stringify parsed))", env);
        Jlll.eval("(define reparsed (json-parse stringified))", env);
        // Compare values
        eval("Alice", "(hash-ref reparsed \"name\")");
        eval(30L, "(hash-ref reparsed \"age\")");
    }

    @Test
    public void testRoundtripArray() throws Exception
    {
        Jlll.eval("(define original '(1 2 3))", env);
        Jlll.eval("(define json (json-stringify original))", env);
        Jlll.eval("(define back (json-parse json))", env);
        eval(3, "(length back)");
        eval(1L, "(car back)");
    }
    // ========== File Operations ==========

    @Test
    public void testJsonReadFile() throws Exception
    {
        File jsonFile = tempFolder.newFile("test.json");
        Files.writeString(jsonFile.toPath(), "{\"message\": \"hello\"}");
        Jlll.eval("(define data (json-read-file \"" + jsonFile.getAbsolutePath().replace("\\", "\\\\") + "\"))", env);
        eval("hello", "(hash-ref data \"message\")");
    }

    @Test
    public void testJsonReadFileWithKeywords() throws Exception
    {
        File jsonFile = tempFolder.newFile("test2.json");
        Files.writeString(jsonFile.toPath(), "{\"name\": \"Bob\"}");
        Jlll.eval("(define data (json-read-file \"" + jsonFile.getAbsolutePath().replace("\\", "\\\\")
                + "\" :keywords true))", env);
        eval("Bob", "(hash-ref data :name)");
    }

    @Test
    public void testJsonWriteFile() throws Exception
    {
        File outFile = tempFolder.newFile("output.json");
        String path = outFile.getAbsolutePath().replace("\\", "\\\\");
        Jlll.eval("(json-write-file \"" + path + "\" (hash-map :x 1 :y 2))", env);
        String content = Files.readString(outFile.toPath());
        assertTrue(content.contains("\"x\""));
        assertTrue(content.contains("\"y\""));
    }

    @Test
    public void testJsonWriteFilePretty() throws Exception
    {
        File outFile = tempFolder.newFile("pretty.json");
        String path = outFile.getAbsolutePath().replace("\\", "\\\\");
        Jlll.eval("(json-write-file \"" + path + "\" (hash-map :a 1) :pretty true)", env);
        String content = Files.readString(outFile.toPath());
        assertTrue("Pretty output should have newlines", content.contains("\n"));
    }
    // ========== Edge Cases ==========

    @Test
    public void testStringifyKeyword() throws Exception
    {
        // Keyword as a value should become a string
        eval("\"foo\"", "(json-stringify :foo)");
    }

    @Test
    public void testParseUnicodeString() throws Exception
    {
        // JSON unicode escapes are handled by Gson, so we pass a JSON string
        // containing unicode escapes and verify they're decoded correctly
        Object result = Jlll.eval("(json-parse \"\\\"\\\\u4e2d\\\\u6587\\\"\")", env);
        assertEquals("\u4e2d\u6587", result);
    }

    @Test
    public void testStringifySpecialCharacters() throws Exception
    {
        Object result = Jlll.eval("(json-stringify \"hello\\nworld\")", env);
        String json = (String) result;
        assertTrue("Should escape newline", json.contains("\\n"));
    }
    // ========== Helper Methods ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
