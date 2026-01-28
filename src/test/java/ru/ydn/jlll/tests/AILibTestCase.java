package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static ru.ydn.jlll.common.Jlll.eval;
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
}
