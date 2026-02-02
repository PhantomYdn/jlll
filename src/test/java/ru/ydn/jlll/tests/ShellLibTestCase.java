package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;

/**
 * Tests for the ShellLib shell execution functions.
 */
public class ShellLibTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }
    // ========== Basic Execution ==========

    @Test
    public void testBasicEcho() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo hello\")", env);
        assertTrue("bash should return a Map", result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain 'hello'", stdout.trim().equals("hello"));
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testMultilineOutput() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo 'line1' && echo 'line2' && echo 'line3'\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain line1", stdout.contains("line1"));
        assertTrue("stdout should contain line2", stdout.contains("line2"));
        assertTrue("stdout should contain line3", stdout.contains("line3"));
    }
    // ========== Exit Codes ==========

    @Test
    public void testExitCodeSuccess() throws Exception
    {
        Object result = Jlll.eval("(bash \"exit 0\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testExitCodeFailure() throws Exception
    {
        Object result = Jlll.eval("(bash \"exit 1\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals("exit-code should be 1", 1, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testExitCodeArbitrary() throws Exception
    {
        Object result = Jlll.eval("(bash \"exit 42\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals("exit-code should be 42", 42, map.get(Keyword.intern("exit-code")));
    }
    // ========== Stdout/Stderr Separation ==========

    @Test
    public void testStdoutOnly() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo stdout_msg\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        String stderr = (String) map.get(Keyword.intern("stderr"));
        assertTrue("stdout should contain message", stdout.contains("stdout_msg"));
        assertTrue("stderr should be empty", stderr.isEmpty());
    }

    @Test
    public void testStderrOnly() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo stderr_msg >&2\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        String stderr = (String) map.get(Keyword.intern("stderr"));
        assertTrue("stdout should be empty", stdout.isEmpty());
        assertTrue("stderr should contain message", stderr.contains("stderr_msg"));
    }

    @Test
    public void testBothStreams() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo out_msg && echo err_msg >&2\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        String stderr = (String) map.get(Keyword.intern("stderr"));
        assertTrue("stdout should contain out_msg", stdout.contains("out_msg"));
        assertTrue("stderr should contain err_msg", stderr.contains("err_msg"));
    }
    // ========== Working Directory ==========

    @Test
    public void testWorkingDirectory() throws Exception
    {
        // Create temp directory
        File tempDir = Files.createTempDirectory("jlll-shell-test").toFile();
        tempDir.deleteOnExit();
        String path = tempDir.getAbsolutePath().replace("\\", "\\\\");
        Object result = Jlll.eval("(bash \"pwd\" :cwd \"" + path + "\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain temp dir path", stdout.trim().endsWith(tempDir.getName()));
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testWorkingDirectoryTilde() throws Exception
    {
        Object result = Jlll.eval("(bash \"pwd\" :cwd \"~\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        String home = System.getProperty("user.home");
        assertTrue("stdout should be home directory", stdout.trim().equals(home));
    }

    @Test(expected = JlllException.class)
    public void testInvalidWorkingDirectory() throws Exception
    {
        Jlll.eval("(bash \"pwd\" :cwd \"/nonexistent/path/12345\")", env);
    }
    // ========== Timeout ==========

    @Test
    public void testTimeoutSuccess() throws Exception
    {
        // Command that completes quickly with generous timeout
        Object result = Jlll.eval("(bash \"echo fast\" :timeout 5000)", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testTimeoutExpired() throws Exception
    {
        // Command that takes longer than timeout
        Object result = Jlll.eval("(bash \"sleep 10\" :timeout 100)", env);
        Map<?, ?> map = (Map<?, ?>) result;
        assertEquals("exit-code should be -1 on timeout", -1, map.get(Keyword.intern("exit-code")));
        String stderr = (String) map.get(Keyword.intern("stderr"));
        assertTrue("stderr should contain timeout message", stderr.contains("timed out"));
    }
    // ========== Input Handling ==========

    @Test
    public void testStringInput() throws Exception
    {
        Object result = Jlll.eval("(bash \"cat\" :input \"hello from stdin\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertEquals("stdout should match input", "hello from stdin", stdout);
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }

    @Test
    public void testMultilineInput() throws Exception
    {
        Object result = Jlll.eval("(bash \"wc -l\" :input \"line1\\nline2\\nline3\")", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        // wc -l output varies by platform, but should contain "3" or "2" (depends on trailing newline)
        assertTrue("stdout should contain line count", stdout.trim().matches("\\d+"));
    }

    @Test
    public void testPortInput() throws Exception
    {
        // Create a temp file with content
        File tempFile = File.createTempFile("jlll-shell-input", ".txt");
        tempFile.deleteOnExit();
        try (FileWriter fw = new FileWriter(tempFile))
        {
            fw.write("port content");
        }
        String path = tempFile.getAbsolutePath().replace("\\", "\\\\");
        // Open input file as port and pass to bash
        Object result = Jlll.eval("(let ((port (open-input-file \"" + path + "\"))) "
                + "(let ((result (bash \"cat\" :input port))) " + "(close-input-port port) " + "result))", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertEquals("stdout should match file content", "port content", stdout);
    }
    // ========== Environment Variables ==========

    @Test
    public void testEnvironmentVariables() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo $MY_TEST_VAR\" :env (hash-map \"MY_TEST_VAR\" \"test_value\"))", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain env var value", stdout.trim().equals("test_value"));
    }

    @Test
    public void testEnvironmentVariablesWithKeywords() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo $FOO\" :env (hash-map :FOO \"bar\"))", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain env var value", stdout.trim().equals("bar"));
    }

    @Test
    public void testMultipleEnvVars() throws Exception
    {
        Object result = Jlll.eval("(bash \"echo $VAR1-$VAR2\" :env (hash-map \"VAR1\" \"hello\" \"VAR2\" \"world\"))",
                env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain both vars", stdout.trim().equals("hello-world"));
    }
    // ========== Aliases and Helpers ==========

    @Test
    public void testShellAlias() throws Exception
    {
        Object result = Jlll.eval("(shell \"echo test\")", env);
        assertTrue("shell should return a Map", result instanceof Map);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain 'test'", stdout.trim().equals("test"));
    }

    @Test
    public void testShellStdout() throws Exception
    {
        Object result = Jlll.eval("(shell-stdout \"echo hello\")", env);
        assertTrue("shell-stdout should return a String", result instanceof String);
        assertEquals("result should be stdout content", "hello\n", result);
    }

    @Test
    public void testShellOkSuccess() throws Exception
    {
        Object result = Jlll.eval("(shell-ok? \"exit 0\")", env);
        assertEquals("shell-ok? should return true for exit 0", Boolean.TRUE, result);
    }

    @Test
    public void testShellOkFailure() throws Exception
    {
        Object result = Jlll.eval("(shell-ok? \"exit 1\")", env);
        assertEquals("shell-ok? should return false for exit 1", Boolean.FALSE, result);
    }
    // ========== Combined Options ==========

    @Test
    public void testCombinedOptions() throws Exception
    {
        File tempDir = Files.createTempDirectory("jlll-shell-combined").toFile();
        tempDir.deleteOnExit();
        String path = tempDir.getAbsolutePath().replace("\\", "\\\\");
        Object result = Jlll.eval("(bash \"echo $MY_VAR\" :cwd \"" + path
                + "\" :env (hash-map \"MY_VAR\" \"combined\") " + ":timeout 5000)", env);
        Map<?, ?> map = (Map<?, ?>) result;
        String stdout = (String) map.get(Keyword.intern("stdout"));
        assertTrue("stdout should contain env var", stdout.trim().equals("combined"));
        assertEquals("exit-code should be 0", 0, map.get(Keyword.intern("exit-code")));
    }
    // ========== Error Handling ==========

    @Test(expected = JlllException.class)
    public void testMissingCommand() throws Exception
    {
        Jlll.eval("(bash)", env);
    }

    @Test
    public void testInvalidEnvType() throws Exception
    {
        try
        {
            Jlll.eval("(bash \"echo test\" :env \"not-a-map\")", env);
            fail("Should throw exception for invalid env type");
        }
        catch (JlllException e)
        {
            assertTrue("Error should mention hash-map", e.getMessage().contains("hash-map"));
        }
    }
    // ========== Helper ==========

    private void eval(Object expected, String code) throws Exception
    {
        Object ret = Jlll.eval(code, env);
        assertEquals(expected, ret);
    }
}
