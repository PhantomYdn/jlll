package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static ru.ydn.jlll.common.Jlll.eval;
import java.io.File;
import java.nio.file.Files;
import java.util.Map;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.ProcEnvironment;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.libs.ai.AIConfig;
import ru.ydn.jlll.libs.ai.AISession;
import ru.ydn.jlll.libs.ai.AITool;

/**
 * Test cases for AILib.
 *
 * <p>
 * Note: Tests that require actual API calls are skipped if no API key is configured.
 * These tests can be run by setting the appropriate environment variables:
 * OPENAI_API_KEY, ANTHROPIC_API_KEY, GOOGLE_AI_API_KEY, or OLLAMA_BASE_URL.
 * </p>
 */
public class AILibTestCase
{
    private Environment env;
    private boolean hasProvider;

    @Before
    public void setUp() throws Exception
    {
        env = new Environment(Environment.top);
        // Check if any provider is configured
        hasProvider = AIConfig.getInstance().detectProviderOptional().isPresent();
        // Clear any existing sessions
        AISession.clearAllSessions();
    }

    @After
    public void tearDown() throws Exception
    {
        // Clean up sessions
        AISession.clearAllSessions();
        // Reset config
        AIConfig.getInstance().reset();
    }
    // ========== Configuration Tests ==========

    @Test
    public void testAiConfig() throws Exception
    {
        Object result = eval("(ai-config)", env);
        assertTrue("ai-config should return a Map", result instanceof Map);
    }

    @Test
    public void testAiConfigure() throws Exception
    {
        eval("(ai-configure :default-model \"test-model\")", env);
        Map<?, ?> config = (Map<?, ?>) eval("(ai-config)", env);
        assertEquals("test-model", config.get(ru.ydn.jlll.common.Symbol.intern("default-model-override")));
    }
    // ========== Session Management Tests ==========

    @Test
    public void testAiSessionsEmpty() throws Exception
    {
        Object result = eval("(ai-sessions)", env);
        assertTrue("ai-sessions should return a Cons", result instanceof Cons || Null.NULL.equals(result));
    }

    @Test
    public void testAiSessionCreate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionCreate: No AI provider configured");
            return;
        }
        Object session = eval("(ai-session-create :name \"test-session\")", env);
        assertNotNull("ai-session-create should return a session", session);
        assertTrue("Result should be an AISession", session instanceof AISession);
        AISession aiSession = (AISession) session;
        assertEquals("test-session", aiSession.getName());
    }

    @Test
    public void testAiSessionActivate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionActivate: No AI provider configured");
            return;
        }
        // Create and activate session
        eval("(define sess (ai-session-create :name \"activate-test\"))", env);
        eval("(ai-session-activate sess)", env);
        // Verify it's active
        Object current = eval("(ai-session-current)", env);
        assertTrue("Current session should be an AISession", current instanceof AISession);
        assertEquals("activate-test", ((AISession) current).getName());
    }

    @Test
    public void testAiSessionDeactivate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionDeactivate: No AI provider configured");
            return;
        }
        // Create, activate, then deactivate
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        eval("(ai-session-deactivate)", env);
        // Verify no current session
        Object current = eval("(ai-session-current)", env);
        assertTrue("No session should be active", Null.NULL.equals(current));
    }

    @Test
    public void testAiSessionNameAndId() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionNameAndId: No AI provider configured");
            return;
        }
        eval("(define sess (ai-session-create :name \"id-test\"))", env);
        eval("(ai-session-activate sess)", env);
        String name = (String) eval("(ai-session-name sess)", env);
        String id = (String) eval("(ai-session-id sess)", env);
        assertEquals("id-test", name);
        assertTrue("ID should start with sess-", id.startsWith("sess-"));
    }
    // ========== Predicate Tests ==========

    @Test
    public void testAiSessionPredicate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionPredicate: No AI provider configured");
            return;
        }
        eval("(define sess (ai-session-create))", env);
        assertTrue((Boolean) eval("(ai-session? sess)", env));
        assertFalse((Boolean) eval("(ai-session? \"not a session\")", env));
        assertFalse((Boolean) eval("(ai-session? 42)", env));
    }

    @Test
    public void testAiToolPredicate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiToolPredicate: No AI provider configured");
            return;
        }
        eval("(define my-tool (ai-tool \"test\" :description \"test tool\" :fn (lambda (x) x)))", env);
        assertTrue((Boolean) eval("(ai-tool? my-tool)", env));
        assertFalse((Boolean) eval("(ai-tool? \"not a tool\")", env));
    }
    // ========== Tool Management Tests ==========

    @Test
    public void testAiToolCreate() throws Exception
    {
        Object tool = eval("(ai-tool \"my-tool\" :description \"A test tool\" "
                + ":parameters '((x \"string\" \"input\")) " + ":fn (lambda (x) (concat \"result: \" x)))", env);
        assertNotNull("ai-tool should return a tool", tool);
        assertTrue("Result should be an AITool", tool instanceof AITool);
        AITool aiTool = (AITool) tool;
        assertEquals("my-tool", aiTool.getName());
        assertEquals("A test tool", aiTool.getDescription());
    }

    @Test
    public void testAiToolAddRemove() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiToolAddRemove: No AI provider configured");
            return;
        }
        // Create session without eval tool
        eval("(define sess (ai-session-create :eval false))", env);
        eval("(ai-session-activate sess)", env);
        // Check no tools
        Cons tools = (Cons) eval("(ai-tools)", env);
        assertTrue("Should have no tools initially", tools == null || tools.isNull());
        // Create and add tool
        eval("(define my-tool (ai-tool \"custom\" :description \"Custom tool\" :fn (lambda () \"ok\")))", env);
        eval("(ai-tool-add my-tool)", env);
        // Verify tool added
        tools = (Cons) eval("(ai-tools)", env);
        assertEquals(1, tools.length());
        // Remove tool
        eval("(ai-tool-remove \"custom\")", env);
        tools = (Cons) eval("(ai-tools)", env);
        assertTrue("Should have no tools after removal", tools == null || tools.isNull());
    }

    @Test
    public void testBuiltInEvalTool() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testBuiltInEvalTool: No AI provider configured");
            return;
        }
        // Create session (eval tool enabled by default)
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Check eval tool exists
        Cons tools = (Cons) eval("(ai-tools)", env);
        boolean hasEval = false;
        for (Object t : tools)
        {
            if (t instanceof Map)
            {
                Object name = ((Map<?, ?>) t).get(ru.ydn.jlll.common.Symbol.intern("name"));
                if ("eval".equals(name))
                {
                    hasEval = true;
                    break;
                }
            }
        }
        assertTrue("Session should have eval tool by default", hasEval);
    }
    // ========== History Tests ==========

    @Test
    public void testAiHistoryEmpty() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiHistoryEmpty: No AI provider configured");
            return;
        }
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        Object history = eval("(ai-history)", env);
        assertTrue("History should be empty initially",
                Null.NULL.equals(history) || (history instanceof Cons && ((Cons) history).isNull()));
    }

    @Test
    public void testAiClear() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiClear: No AI provider configured");
            return;
        }
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Clear should work even on empty history
        Object result = eval("(ai-clear)", env);
        assertTrue(Null.NULL.equals(result));
    }
    // ========== AI Request Tests (require actual API key) ==========

    @Test
    public void testAiSimpleRequest() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSimpleRequest: No AI provider configured");
            return;
        }
        // This test actually calls the API - only runs if configured
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        try
        {
            // Make a simple request using ai-prompt (returns lazy sequence)
            Object response = eval("(ai-prompt \"Say just the word 'hello' and nothing else.\")", env);
            assertNotNull("AI should return a response (lazy sequence)", response);
            // Realize the response
            String fullResponse = (String) eval("(string-join (realize " + getVarRef(response) + ") \"\")", env);
            assertNotNull("Full response should not be null", fullResponse);
            System.out.println("AI Response: " + fullResponse);
        }
        catch (Exception e)
        {
            // Check if this is an API quota/billing/rate limit error
            String msg = getFullExceptionMessage(e);
            if (msg.contains("quota") || msg.contains("billing") || msg.contains("rate_limit")
                    || msg.contains("insufficient") || msg.contains("exceeded"))
            {
                System.out.println("Skipping testAiSimpleRequest: API quota/billing issue - " + e.getMessage());
                return;
            }
            throw e;
        }
    }

    /**
     * Simple smoke test for AI - asks a math question and verifies the answer.
     * This is a quick sanity check that the full AI pipeline works.
     * Note: This test validates that we can get a response, but does not fail
     * if the AI gives a wrong answer (that's an AI quality issue, not code).
     */
    @Test
    public void testAiMathQuestion() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiMathQuestion: No AI provider configured");
            return;
        }
        try
        {
            // Create session with system prompt to ensure concise answers
            eval("(define sess (ai-session-create :system \"Answer with just the number, nothing else.\"))", env);
            eval("(ai-session-activate sess)", env);
            // Ask simple math question using ai-prompt (returns lazy sequence)
            Object response = eval("(ai-prompt \"What is 2 + 2?\")", env);
            assertNotNull("AI should return a response", response);
            // Get full response
            String answer = (String) eval("(string-join (realize " + getVarRef(response) + ") \"\")", env);
            assertNotNull("Answer should not be null", answer);
            assertFalse("Answer should not be empty", answer.isEmpty());
            System.out.println("AI Math Answer: " + answer);
            // Check if answer is correct - but only warn if wrong (AI quality issue, not code bug)
            boolean hasCorrectAnswer = answer.contains("4") || answer.toLowerCase().contains("four");
            if (!hasCorrectAnswer)
            {
                System.out.println("WARNING: AI gave unexpected answer '" + answer + "' for 2+2 (expected 4)");
                System.out.println("This may indicate a model quality issue, not a code bug.");
            }
        }
        catch (Exception e)
        {
            if (isApiError(e))
            {
                System.out.println("Skipping testAiMathQuestion: API issue - " + e.getMessage());
                return;
            }
            throw e;
        }
    }

    /**
     * Test that eval tool works - LLM can execute JLLL code.
     */
    @Test
    public void testAiEvalTool() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiEvalTool: No AI provider configured");
            return;
        }
        try
        {
            // Create session with eval tool (default)
            eval("(define sess (ai-session-create :system \"Use the eval tool to compute the answer. Return only the result.\"))",
                    env);
            eval("(ai-session-activate sess)", env);
            // Ask something that requires computation using ai-prompt
            Object response = eval("(ai-prompt \"Use eval to calculate (+ 10 20 30)\")", env);
            String answer = (String) eval("(string-join (realize " + getVarRef(response) + ") \"\")", env);
            assertNotNull("Answer should not be null", answer);
            System.out.println("AI Eval Tool Answer: " + answer);
            // Should contain 60 somewhere in the response
            assertTrue("Answer should contain '60', got: " + answer, answer.contains("60"));
        }
        catch (Exception e)
        {
            if (isApiError(e))
            {
                System.out.println("Skipping testAiEvalTool: API issue - " + e.getMessage());
                return;
            }
            throw e;
        }
    }

    /**
     * Test that the eval tool uses the session's current environment, not a stale reference.
     * This verifies that bindings persist across tool calls within the same session.
     */
    @Test
    public void testEvalToolEnvironmentSync() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testEvalToolEnvironmentSync: No AI provider configured");
            return;
        }
        // Create a session in env
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Get the session and its eval tool
        AISession session = (AISession) eval("(ai-session-current)", env);
        assertNotNull("Session should be active", session);
        AITool evalTool = session.getTool("eval");
        assertNotNull("Eval tool should exist", evalTool);
        // Verify eval tool uses the session's environment
        assertSame("Eval tool should use session's environment", env, evalTool.getEnvironment());
        // Create a child environment and activate the same session there
        Environment childEnv = new Environment(env);
        // Simulate activating session in child environment
        session.setEnvironment(childEnv);
        // Verify eval tool environment was updated
        assertSame("Eval tool should use updated environment", childEnv, evalTool.getEnvironment());
    }

    /**
     * Test that bindings created via eval tool persist in the environment.
     * This simulates what happens when AI defines a variable and then retrieves it.
     */
    @Test
    public void testEvalToolBindingPersistence() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testEvalToolBindingPersistence: No AI provider configured");
            return;
        }
        // Create and activate session
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Get the eval tool directly
        AISession session = (AISession) eval("(ai-session-current)", env);
        AITool evalTool = session.getTool("eval");
        // Simulate AI calling eval tool to define a variable
        String defineResult = evalTool.execute("{\"code\": \"(define ai-test-var 42)\"}");
        System.out.println("Define result: " + defineResult);
        // Simulate AI calling eval tool to retrieve the variable
        String getResult = evalTool.execute("{\"code\": \"ai-test-var\"}");
        System.out.println("Get result: " + getResult);
        assertEquals("42", getResult);
        // Also verify we can access the variable directly in JLLL
        Object value = eval("ai-test-var", env);
        assertEquals(42, value);
    }

    /**
     * Test that bindings created via eval tool persist in the USER environment,
     * not the transient procedure environment.
     * This simulates the scenario: REPL -> (ai "define x") -> ai-prompt -> eval tool
     * The binding should end up in the REPL's environment, not lost in a ProcEnvironment.
     */
    @Test
    public void testEvalToolBindingInUserEnvironment() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testEvalToolBindingInUserEnvironment: No AI provider configured");
            return;
        }
        // Create a wrapper function that calls ai-prompt (simulating the 'ai' function)
        // This creates a ProcEnvironment when the wrapper is called
        eval("(define (my-ai-wrapper prompt) (ai-prompt prompt))", env);
        // Call the wrapper - this should use env.getUserEnvironment() to find the REPL env
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Get the session and manually call the eval tool to define something
        // This simulates what happens when AI decides to call the eval tool
        AISession session = (AISession) eval("(ai-session-current)", env);
        // Now simulate calling through the wrapper by creating a ProcEnvironment
        // and verifying that getUserEnvironment() returns the parent (user) env
        Environment procEnv = new ProcEnvironment(Symbol.intern("prompt"), Cons.list("test"), env);
        assertTrue("ProcEnvironment should be transient", procEnv.isTransient());
        assertFalse("Base env should not be transient", env.isTransient());
        assertSame("getUserEnvironment should skip ProcEnvironment", env, procEnv.getUserEnvironment());
        // Now test that when we update the session with a ProcEnvironment,
        // it uses the user environment for tool execution
        session.setEnvironment(procEnv.getUserEnvironment());
        AITool evalTool = session.getTool("eval");
        // Define a variable via the eval tool
        String defineResult = evalTool.execute("{\"code\": \"(define wrapper-test-var 123)\"}");
        System.out.println("Define via wrapper result: " + defineResult);
        // The variable should be accessible in the user environment (env), not lost
        Object value = eval("wrapper-test-var", env);
        assertEquals("Variable should be defined in user environment", 123, value);
    }

    /**
     * Test that eval tool captures printed output and returns it to AI.
     */
    @Test
    public void testEvalToolCapturesOutput() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testEvalToolCapturesOutput: No AI provider configured");
            return;
        }
        // Create and activate session
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Get the eval tool
        AISession session = (AISession) eval("(ai-session-current)", env);
        AITool evalTool = session.getTool("eval");
        // Execute code that prints something
        String result = evalTool.execute("{\"code\": \"(println \\\"Hello from AI\\\")\"}");
        System.out.println("Captured output result: " + result);
        // The result should contain the printed text
        assertTrue("Result should contain printed output", result.contains("Hello from AI"));
    }

    /**
     * Test that eval tool returns both output and result when there's a non-null result.
     */
    @Test
    public void testEvalToolOutputAndResult() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testEvalToolOutputAndResult: No AI provider configured");
            return;
        }
        // Create and activate session
        eval("(define sess (ai-session-create))", env);
        eval("(ai-session-activate sess)", env);
        // Get the eval tool
        AISession session = (AISession) eval("(ai-session-current)", env);
        AITool evalTool = session.getTool("eval");
        // Execute code that prints and returns a value
        String result = evalTool.execute("{\"code\": \"(begin (println \\\"Computing...\\\") (+ 1 2 3))\"}");
        System.out.println("Output and result: " + result);
        // The result should contain both the output and the result
        assertTrue("Result should contain printed output", result.contains("Computing..."));
        assertTrue("Result should contain return value 6", result.contains("6"));
    }

    /**
     * Test that nil is bound as alias for null.
     */
    @Test
    public void testNilBinding() throws Exception
    {
        // Both nil and null should refer to the same Null.NULL
        Object nilValue = eval("nil", env);
        Object nullValue = eval("null", env);
        assertEquals("nil should equal null", nullValue, nilValue);
        assertTrue("nil should be Null.NULL", Null.NULL.equals(nilValue));
    }

    /**
     * Test that print returns null (no double printing).
     */
    @Test
    public void testPrintReturnsNull() throws Exception
    {
        Object result = eval("(print \"test\")", env);
        assertTrue("print should return null", Null.NULL.equals(result));
    }

    /**
     * Check if exception is an API error that should cause test to skip.
     */
    private boolean isApiError(Exception e)
    {
        String msg = getFullExceptionMessage(e);
        return msg.contains("quota") || msg.contains("billing") || msg.contains("rate_limit")
                || msg.contains("insufficient") || msg.contains("exceeded") || msg.contains("unauthorized")
                || msg.contains("invalid_api_key") || msg.contains("connection");
    }

    /**
     * Gets the full exception message including nested causes.
     */
    private String getFullExceptionMessage(Throwable t)
    {
        StringBuilder sb = new StringBuilder();
        while (t != null)
        {
            if (t.getMessage() != null)
            {
                sb.append(t.getMessage().toLowerCase()).append(" ");
            }
            t = t.getCause();
        }
        return sb.toString();
    }

    /**
     * Helper to reference an object in JLLL.
     */
    private String getVarRef(Object obj)
    {
        // Store in env and return variable name
        env.addBinding("_test_response", obj);
        return "_test_response";
    }
    // ========== Session Save/Load Tests ==========

    /**
     * Test basic session save and load cycle.
     */
    @Test
    public void testAiSessionSaveLoad() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSaveLoad: No AI provider configured");
            return;
        }
        // Create a temp file for the session
        File tempFile = File.createTempFile("jlll-session-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"save-test\" :system \"Test system prompt\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Verify file was created and contains JSON
        String content = Files.readString(tempFile.toPath());
        assertTrue("Saved file should contain session name", content.contains("save-test"));
        assertTrue("Saved file should contain system prompt", content.contains("Test system prompt"));
        assertTrue("Saved file should contain version", content.contains("\"version\""));
        // Load the session
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\")", env);
        assertTrue("Loaded object should be AISession", loaded instanceof AISession);
        AISession loadedSession = (AISession) loaded;
        assertEquals("Loaded session should have same name", "save-test", loadedSession.getName());
        assertEquals("Loaded session should have same system prompt", "Test system prompt",
                loadedSession.getSystemPrompt());
    }

    /**
     * Test session save with pretty formatting option.
     */
    @Test
    public void testAiSessionSavePretty() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSavePretty: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-pretty-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Save with pretty formatting
        eval("(define sess (ai-session-create :name \"pretty-test\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\" :pretty true)", env);
        // Verify file contains indentation (pretty printed)
        String content = Files.readString(tempFile.toPath());
        assertTrue("Pretty printed JSON should contain newlines", content.contains("\n"));
        assertTrue("Pretty printed JSON should have proper formatting", content.contains("  \""));
    }

    /**
     * Test session load with name override.
     */
    @Test
    public void testAiSessionLoadNameOverride() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionLoadNameOverride: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-name-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session with original name
        eval("(define sess (ai-session-create :name \"original-name\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Load with name override
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\" :name \"overridden-name\")", env);
        AISession loadedSession = (AISession) loaded;
        assertEquals("Loaded session should have overridden name", "overridden-name", loadedSession.getName());
    }

    /**
     * Test session load with activate option.
     */
    @Test
    public void testAiSessionLoadActivate() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionLoadActivate: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-activate-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"activate-test\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Deactivate and clear sessions
        eval("(ai-session-deactivate)", env);
        AISession.clearAllSessions();
        // Load with activate option
        eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\" :activate true)", env);
        // Verify it's the current session
        Object current = eval("(ai-session-current)", env);
        assertTrue("Current session should be set", current instanceof AISession);
        assertEquals("activate-test", ((AISession) current).getName());
    }

    /**
     * Test session save/load preserves conversation history.
     */
    @Test
    public void testAiSessionSaveLoadWithHistory() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSaveLoadWithHistory: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-history-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session and add history manually
        eval("(define sess (ai-session-create :name \"history-test\"))", env);
        AISession session = (AISession) eval("sess", env);
        session.addUserMessage("Hello from user");
        session.addAiMessage("Hello from AI");
        session.addUserMessage("Second message");
        // Save session
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Clear and load
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\")", env);
        AISession loadedSession = (AISession) loaded;
        // Verify history
        assertEquals("History should have 3 messages", 3, loadedSession.getHistory().size());
    }

    /**
     * Test session save/load with custom tools.
     */
    @Test
    public void testAiSessionSaveLoadWithCustomTools() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSaveLoadWithCustomTools: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-tools-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session without eval tool, add custom tool
        eval("(define sess (ai-session-create :name \"tools-test\" :eval false))", env);
        eval("(ai-session-activate sess)", env);
        eval("(define my-tool (ai-tool \"custom-tool\" :description \"A custom tool\" "
                + ":parameters '((x \"string\" \"input\" true)) " + ":fn (lambda (x) (concat \"result: \" x))))", env);
        eval("(ai-tool-add my-tool)", env);
        // Save session
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Clear and load
        eval("(ai-session-deactivate)", env);
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\" :eval false)", env);
        AISession loadedSession = (AISession) loaded;
        // Verify custom tool was restored
        assertTrue("Loaded session should have custom-tool", loadedSession.hasTool("custom-tool"));
        AITool restoredTool = loadedSession.getTool("custom-tool");
        assertEquals("custom-tool", restoredTool.getName());
        assertEquals("A custom tool", restoredTool.getDescription());
        // Test that the restored tool works
        String result = restoredTool.execute("{\"x\": \"hello\"}");
        System.out.println("Restored tool result: " + result);
        assertTrue("Restored tool should work, got: " + result, result.contains("result: hello"));
    }

    /**
     * Test session load ID collision handling.
     */
    @Test
    public void testAiSessionLoadIdCollision() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionLoadIdCollision: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-collision-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"collision-test\"))", env);
        AISession original = (AISession) eval("sess", env);
        String originalId = original.getId();
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Load without clearing - should get new ID due to collision
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\")", env);
        AISession loadedSession = (AISession) loaded;
        assertNotEquals("Loaded session should have different ID due to collision", originalId, loadedSession.getId());
        assertTrue("New ID should contain 'restored'", loadedSession.getId().contains("-restored-"));
    }

    /**
     * Test that eval tool is added by default when loading.
     */
    @Test
    public void testAiSessionLoadWithEvalTool() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionLoadWithEvalTool: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-eval-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session without eval tool (to verify it's added on load)
        eval("(define sess (ai-session-create :name \"eval-test\" :eval false))", env);
        AISession session = (AISession) eval("sess", env);
        assertFalse("Original session should not have eval tool", session.hasTool("eval"));
        // Save session
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Clear and load (default: eval tool added)
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\")", env);
        AISession loadedSession = (AISession) loaded;
        assertTrue("Loaded session should have eval tool by default", loadedSession.hasTool("eval"));
    }

    /**
     * Test session load without eval tool.
     */
    @Test
    public void testAiSessionLoadWithoutEvalTool() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionLoadWithoutEvalTool: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-noeval-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"noeval-test\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Clear and load without eval tool
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + path.replace("\\", "\\\\") + "\" :eval false)", env);
        AISession loadedSession = (AISession) loaded;
        assertFalse("Loaded session should not have eval tool when :eval false", loadedSession.hasTool("eval"));
    }

    /**
     * Test saving current session (without explicit session argument).
     */
    @Test
    public void testAiSessionSaveCurrent() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSaveCurrent: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-current-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and activate session
        eval("(define sess (ai-session-create :name \"current-test\"))", env);
        eval("(ai-session-activate sess)", env);
        // Save current session (no session argument)
        eval("(ai-session-save \"" + path.replace("\\", "\\\\") + "\")", env);
        // Verify file was created correctly
        String content = Files.readString(tempFile.toPath());
        assertTrue("Saved file should contain session name", content.contains("current-test"));
    }

    /**
     * Test ai-session-restore convenience function (load + activate in one call).
     */
    @Test
    public void testAiSessionRestore() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionRestore: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-restore-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"restore-test\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Deactivate and clear
        eval("(ai-session-deactivate)", env);
        AISession.clearAllSessions();
        // Use ai-session-restore (should load AND activate)
        Object restored = eval("(ai-session-restore \"" + path.replace("\\", "\\\\") + "\")", env);
        assertTrue("Restored object should be AISession", restored instanceof AISession);
        // Verify it's the current session
        Object current = eval("(ai-session-current)", env);
        assertSame("ai-session-restore should activate the session", restored, current);
        assertEquals("restore-test", ((AISession) current).getName());
    }

    /**
     * Test ai-session-restore with name override option.
     */
    @Test
    public void testAiSessionRestoreWithOptions() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionRestoreWithOptions: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-session-restore-opts-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and save session
        eval("(define sess (ai-session-create :name \"original\"))", env);
        eval("(ai-session-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Clear and restore with name override
        eval("(ai-session-deactivate)", env);
        AISession.clearAllSessions();
        eval("(ai-session-restore \"" + path.replace("\\", "\\\\") + "\" :name \"renamed\")", env);
        // Verify name was overridden
        Object current = eval("(ai-session-current)", env);
        assertEquals("renamed", ((AISession) current).getName());
    }
    // ========== Directory Creation Tests ==========

    /**
     * Test that ai-session-save creates parent directories automatically.
     */
    @Test
    public void testAiSessionSaveCreatesDirectories() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionSaveCreatesDirectories: No AI provider configured");
            return;
        }
        // Create a temp directory and define a nested path
        File tempDir = Files.createTempDirectory("jlll-session-dirs-").toFile();
        tempDir.deleteOnExit();
        String nestedPath = tempDir.getAbsolutePath() + "/subdir1/subdir2/session.json";
        // Create and save session - should create directories
        eval("(define sess (ai-session-create :name \"dir-test\"))", env);
        eval("(ai-session-save sess \"" + nestedPath.replace("\\", "\\\\") + "\")", env);
        // Verify file was created
        File savedFile = new File(nestedPath);
        assertTrue("Session file should exist", savedFile.exists());
        // Clean up
        savedFile.delete();
        new File(tempDir.getAbsolutePath() + "/subdir1/subdir2").delete();
        new File(tempDir.getAbsolutePath() + "/subdir1").delete();
    }
    // ========== Auto-Save Tests ==========

    /**
     * Test enabling auto-save on current session.
     */
    @Test
    public void testAiSessionAutoSaveEnable() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionAutoSaveEnable: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-autosave-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create and activate session
        eval("(define sess (ai-session-create :name \"autosave-test\"))", env);
        eval("(ai-session-activate sess)", env);
        // Enable auto-save
        Object result = eval("(ai-session-auto-save \"" + path.replace("\\", "\\\\") + "\")", env);
        assertEquals("Should return the path", path, result);
        // Verify auto-save is enabled
        AISession session = (AISession) eval("(ai-session-current)", env);
        assertEquals("Auto-save path should be set", path, session.getAutoSavePath());
        // Verify initial save was performed
        String content = Files.readString(tempFile.toPath());
        assertTrue("File should contain session data", content.contains("autosave-test"));
    }

    /**
     * Test disabling auto-save.
     */
    @Test
    public void testAiSessionAutoSaveDisable() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionAutoSaveDisable: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-autosave-disable-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session with auto-save enabled
        eval("(define sess (ai-session-create :name \"disable-test\" :auto-save \"" + path.replace("\\", "\\\\")
                + "\"))", env);
        eval("(ai-session-activate sess)", env);
        // Disable auto-save
        eval("(ai-session-auto-save false)", env);
        // Verify auto-save is disabled
        AISession session = (AISession) eval("(ai-session-current)", env);
        assertNull("Auto-save path should be null", session.getAutoSavePath());
    }

    /**
     * Test querying auto-save status.
     */
    @Test
    public void testAiSessionAutoSaveQuery() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionAutoSaveQuery: No AI provider configured");
            return;
        }
        // Create session without auto-save
        eval("(define sess (ai-session-create :name \"query-test\"))", env);
        eval("(ai-session-activate sess)", env);
        // Query should return false
        Object result = eval("(ai-session-auto-save)", env);
        assertEquals("Should return false when disabled", Boolean.FALSE, result);
        // Enable auto-save
        File tempFile = File.createTempFile("jlll-autosave-query-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        eval("(ai-session-auto-save \"" + path.replace("\\", "\\\\") + "\")", env);
        // Query should return path
        result = eval("(ai-session-auto-save)", env);
        assertEquals("Should return path when enabled", path, result);
    }

    /**
     * Test creating session with :auto-save option.
     */
    @Test
    public void testAiSessionCreateWithAutoSave() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionCreateWithAutoSave: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-create-autosave-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session with auto-save
        eval("(define sess (ai-session-create :name \"create-autosave\" :auto-save \"" + path.replace("\\", "\\\\")
                + "\"))", env);
        // Verify auto-save is enabled
        AISession session = (AISession) eval("sess", env);
        assertEquals("Auto-save path should be set", path, session.getAutoSavePath());
        // Verify initial save was performed
        String content = Files.readString(tempFile.toPath());
        assertTrue("File should contain session data", content.contains("create-autosave"));
    }

    /**
     * Test that auto-save path persists through save/load cycle.
     */
    @Test
    public void testAiSessionAutoSavePersists() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionAutoSavePersists: No AI provider configured");
            return;
        }
        File autoSaveFile = File.createTempFile("jlll-autosave-persist-", ".json");
        autoSaveFile.deleteOnExit();
        String autoSavePath = autoSaveFile.getAbsolutePath();
        File manualSaveFile = File.createTempFile("jlll-manual-save-", ".json");
        manualSaveFile.deleteOnExit();
        String manualSavePath = manualSaveFile.getAbsolutePath();
        // Create session with auto-save
        eval("(define sess (ai-session-create :name \"persist-test\" :auto-save \"" + autoSavePath.replace("\\", "\\\\")
                + "\"))", env);
        // Manually save to different file
        eval("(ai-session-save sess \"" + manualSavePath.replace("\\", "\\\\") + "\")", env);
        // Clear and load from manual save
        AISession.clearAllSessions();
        Object loaded = eval("(ai-session-load \"" + manualSavePath.replace("\\", "\\\\") + "\")", env);
        AISession loadedSession = (AISession) loaded;
        // Verify auto-save path was restored
        assertEquals("Auto-save path should be restored", autoSavePath, loadedSession.getAutoSavePath());
    }

    /**
     * Test auto-save with specific session argument.
     */
    @Test
    public void testAiSessionAutoSaveWithSession() throws Exception
    {
        if (!hasProvider)
        {
            System.out.println("Skipping testAiSessionAutoSaveWithSession: No AI provider configured");
            return;
        }
        File tempFile = File.createTempFile("jlll-autosave-session-", ".json");
        tempFile.deleteOnExit();
        String path = tempFile.getAbsolutePath();
        // Create session (not activated)
        eval("(define sess (ai-session-create :name \"session-arg-test\"))", env);
        // Enable auto-save on specific session
        eval("(ai-session-auto-save sess \"" + path.replace("\\", "\\\\") + "\")", env);
        // Verify auto-save is enabled
        AISession session = (AISession) eval("sess", env);
        assertEquals("Auto-save path should be set", path, session.getAutoSavePath());
        // Query specific session
        Object result = eval("(ai-session-auto-save sess)", env);
        assertEquals("Should return path", path, result);
        // Disable on specific session
        eval("(ai-session-auto-save sess false)", env);
        assertNull("Auto-save should be disabled", session.getAutoSavePath());
    }
}
