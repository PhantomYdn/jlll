package ru.ydn.jlll.libs;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;
import dev.langchain4j.agent.tool.ToolExecutionRequest;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.ToolExecutionResultMessage;
import dev.langchain4j.model.chat.StreamingChatModel;
import dev.langchain4j.model.chat.request.ChatRequest;
import dev.langchain4j.model.chat.response.ChatResponse;
import dev.langchain4j.model.chat.response.StreamingChatResponseHandler;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.LazyThunk;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;
import ru.ydn.jlll.libs.ai.AIConfig;
import ru.ydn.jlll.libs.ai.AISession;
import ru.ydn.jlll.libs.ai.AITool;

/**
 * AI/LLM integration library using LangChain4j.
 *
 * <p>
 * Provides session-based AI interactions with streaming responses returned as lazy sequences.
 * Supports multiple providers (OpenAI, Anthropic, Google AI, Ollama) with automatic detection.
 * </p>
 *
 * <h3>Session Management</h3>
 * <ul>
 * <li><b>ai-session-create:</b> Create a new AI session</li>
 * <li><b>ai-session-activate:</b> Make a session active for the current environment</li>
 * <li><b>ai-session-deactivate:</b> Deactivate the current session</li>
 * <li><b>ai-session-current:</b> Get the currently active session</li>
 * <li><b>ai-sessions:</b> List all sessions</li>
 * </ul>
 *
 * <h3>Core Operations</h3>
 * <ul>
 * <li><b>ai:</b> Chat with the LLM, returns lazy sequence of response chunks</li>
 * <li><b>ai-history:</b> Get conversation history</li>
 * <li><b>ai-clear:</b> Clear conversation history</li>
 * </ul>
 *
 * <h3>Tool Management</h3>
 * <ul>
 * <li><b>ai-tool:</b> Define a custom tool from a JLLL procedure</li>
 * <li><b>ai-tool-add:</b> Add a tool to a session</li>
 * <li><b>ai-tool-remove:</b> Remove a tool from a session</li>
 * <li><b>ai-tools:</b> List tools in a session</li>
 * </ul>
 *
 * <h3>Configuration</h3>
 * <ul>
 * <li><b>ai-configure:</b> Set API keys and defaults</li>
 * <li><b>ai-config:</b> Get current configuration</li>
 * </ul>
 */
public class AILib implements Library
{
    /** Symbol for active session binding in environment */
    private static final Symbol AI_SESSION_SYMBOL = Symbol.intern("*ai-session*");
    /** Sentinel object to signal end of stream */
    private static final Object END_OF_STREAM = new Object();
    /** Maximum depth for recursive tool calls to prevent infinite loops */
    private static final int MAX_TOOL_CALL_DEPTH = 10;

    /** Sentinel object to signal error */
    private static final class StreamError
    {
        final Throwable error;

        StreamError(Throwable error)
        {
            this.error = error;
        }
    }

    /**
     * Outputs debug message to stderr when tracing is enabled.
     */
    private static void debugLog(String message)
    {
        System.err.println(message);
    }

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        // ========== ai-session-create ==========
        new Primitive("ai-session-create", env,
                "Creates a new AI session. Options: :name (string), :system (system prompt), "
                        + ":model (model name), :tier (\"best\", \"balanced\", \"fast\"), "
                        + ":tools (list of tools), :auto-save (file path for auto-saving), "
                        + ":trace (boolean, enable tool call tracing). " + "Returns the session object.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String name = null;
                String systemPrompt = null;
                String modelName = null;
                AIConfig.ModelTier tier = null;
                List<AITool> tools = new ArrayList<>();
                boolean addEvalTool = true;
                String autoSavePath = null;
                boolean traceToolCalls = false;
                // Parse keyword arguments
                for (int i = 0; i < values.length(); i += 2)
                {
                    Object key = values.get(i);
                    if (i + 1 >= values.length())
                        break;
                    Object value = values.get(i + 1);
                    String keyStr = key.toString();
                    switch (keyStr)
                    {
                        case ":name" :
                            name = value.toString();
                            break;
                        case ":system" :
                            systemPrompt = value.toString();
                            break;
                        case ":model" :
                            modelName = value.toString();
                            break;
                        case ":tier" :
                            try
                            {
                                tier = AIConfig.ModelTier.fromString(value.toString());
                            }
                            catch (IllegalArgumentException e)
                            {
                                throw new JlllException("ai-session-create: " + e.getMessage());
                            }
                            break;
                        case ":tools" :
                            if (value instanceof Cons toolList)
                            {
                                for (Object t : toolList)
                                {
                                    if (t instanceof AITool tool)
                                    {
                                        tools.add(tool);
                                    }
                                }
                            }
                            break;
                        case ":eval" :
                            addEvalTool = Boolean.TRUE.equals(value);
                            break;
                        case ":auto-save" :
                            autoSavePath = value.toString();
                            break;
                        case ":trace" :
                            traceToolCalls = Boolean.TRUE.equals(value);
                            break;
                    }
                }
                // Detect provider
                AIConfig config = AIConfig.getInstance();
                AIConfig.Provider provider = config.detectProvider();
                // Resolve model from tier if not explicitly specified
                if (modelName == null && tier != null)
                {
                    modelName = config.getModel(provider, tier);
                }
                // Create session
                AISession session = new AISession(name, provider, modelName, systemPrompt, env);
                // Add built-in tools by default
                if (addEvalTool)
                {
                    // Add eval tool (Java-based, needs direct env access)
                    session.addTool(AITool.createEvalTool(env));
                    // Add JLLL-defined discovery tools (apropos, describe, env, jlll-docs)
                    Symbol addToolsFn = Symbol.intern("ai-add-builtin-tools");
                    Object fn = env.lookup(addToolsFn);
                    if (fn instanceof Procedure proc)
                    {
                        try
                        {
                            proc.applyEvaluated(Cons.list(session), env);
                        }
                        catch (JlllException e)
                        {
                            System.err.println("[WARN] Failed to add built-in tools: " + e.getMessage());
                        }
                    }
                }
                // Add custom tools
                for (AITool tool : tools)
                {
                    session.addTool(tool);
                }
                // Enable tool tracing if requested (must be set before auto-save!)
                if (traceToolCalls)
                {
                    session.setTraceToolCalls(true);
                }
                // Set auto-save path if provided
                if (autoSavePath != null)
                {
                    session.setAutoSavePath(autoSavePath);
                    // Perform initial save
                    session.performAutoSave(env);
                }
                return session;
            }
        };
        // ========== ai-session-activate ==========
        new Primitive("ai-session-activate", env,
                "Makes the given session active for the current environment. " + "(ai-session-activate session)")
        {
            private static final long serialVersionUID = 2L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("ai-session-activate requires a session argument");
                }
                Object sessionObj = values.get(0);
                if (!(sessionObj instanceof AISession session))
                {
                    throw new JlllException("ai-session-activate: argument must be an AISession");
                }
                session.setEnvironment(env);
                // Use setBinding to update the *ai-session* variable defined in init.jlll
                env.setBinding(AI_SESSION_SYMBOL, session);
                return session;
            }
        };
        // ========== ai-session-deactivate ==========
        new Primitive("ai-session-deactivate", env,
                "Deactivates the current session by setting *ai-session* to null. (ai-session-deactivate)")
        {
            private static final long serialVersionUID = 3L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                // Set *ai-session* to null (don't remove, just clear)
                env.setBinding(AI_SESSION_SYMBOL, Null.NULL);
                return Null.NULL;
            }
        };
        // ========== ai-session-current ==========
        new Primitive("ai-session-current", env,
                "Returns the currently active session, or null if none. (ai-session-current)")
        {
            private static final long serialVersionUID = 4L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Object session = env.lookup(AI_SESSION_SYMBOL);
                return session != null ? session : Null.NULL;
            }
        };
        // ========== ai-sessions ==========
        new Primitive("ai-sessions", env, "Returns a list of all registered AI sessions. (ai-sessions)")
        {
            private static final long serialVersionUID = 5L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                List<AISession> sessions = AISession.getAllSessions();
                return Cons.list(sessions.toArray());
            }
        };
        // ========== ai-session-name ==========
        new Primitive("ai-session-name", env, "Returns the name of a session. (ai-session-name session)")
        {
            private static final long serialVersionUID = 6L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session = getSessionArg(values, env, 0);
                return session.getName();
            }
        };
        // ========== ai-session-id ==========
        new Primitive("ai-session-id", env, "Returns the unique ID of a session. (ai-session-id session)")
        {
            private static final long serialVersionUID = 7L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session = getSessionArg(values, env, 0);
                return session.getId();
            }
        };
        // ========== ai-prompt ==========
        new Primitive("ai-prompt", env,
                "Chat with the LLM using the active session. Returns a lazy sequence of response chunks. "
                        + "(ai-prompt \"prompt\") or (ai-prompt \"prompt\" :temperature 0.7 :model \"gpt-4\")")
        {
            private static final long serialVersionUID = 8L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("ai-prompt requires at least a prompt argument");
                }
                String prompt = values.get(0).toString();
                // Get or create session
                AISession session = getCurrentOrCreateSession(env);
                // Update session environment to user level (skip transient procedure scopes)
                // This ensures AI tool definitions persist in the user's environment
                session.setEnvironment(env.getUserEnvironment());
                // Parse optional parameters
                Double temperature = null;
                String modelOverride = null;
                for (int i = 1; i < values.length(); i += 2)
                {
                    if (i + 1 >= values.length())
                        break;
                    String key = values.get(i).toString();
                    Object value = values.get(i + 1);
                    switch (key)
                    {
                        case ":temperature" :
                            temperature = ((Number) value).doubleValue();
                            break;
                        case ":model" :
                            modelOverride = value.toString();
                            break;
                        case ":session" :
                            if (value instanceof AISession s)
                            {
                                session = s;
                            }
                            break;
                    }
                }
                // Execute AI request and return lazy sequence
                return executeAiRequest(session, prompt, temperature, modelOverride);
            }
        };
        // ========== ai-history ==========
        new Primitive("ai-history", env, "Returns the conversation history of a session as a list. "
                + "(ai-history) for current session or (ai-history session)")
        {
            private static final long serialVersionUID = 9L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session = values.length() > 0 ? getSessionArg(values, env, 0) : getCurrentSession(env);
                List<ChatMessage> history = session.getHistory();
                // Convert to JLLL list of hash-maps
                List<Object> result = new ArrayList<>();
                for (ChatMessage msg : history)
                {
                    Map<Object, Object> map = new LinkedHashMap<>();
                    map.put(Symbol.intern("type"), msg.type().name().toLowerCase());
                    if (msg instanceof dev.langchain4j.data.message.UserMessage um)
                    {
                        map.put(Symbol.intern("content"), um.singleText());
                    }
                    else if (msg instanceof AiMessage am)
                    {
                        map.put(Symbol.intern("content"), am.text());
                    }
                    result.add(map);
                }
                return Cons.list(result.toArray());
            }
        };
        // ========== ai-clear ==========
        new Primitive("ai-clear", env,
                "Clears the conversation history. (ai-clear) for current session or (ai-clear session)")
        {
            private static final long serialVersionUID = 10L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session = values.length() > 0 ? getSessionArg(values, env, 0) : getCurrentSession(env);
                session.clearHistory();
                return Null.NULL;
            }
        };
        // ========== ai-tool ==========
        new Primitive("ai-tool", env, "Creates a tool from a JLLL procedure. "
                + "(ai-tool \"name\" :description \"desc\" :parameters '((param1 \"string\" \"desc\")) :fn procedure)")
        {
            private static final long serialVersionUID = 11L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("ai-tool requires a name");
                }
                String name = values.get(0).toString();
                String description = "";
                Cons parameters = null;
                Procedure fn = null;
                // Parse keyword arguments
                for (int i = 1; i < values.length(); i += 2)
                {
                    if (i + 1 >= values.length())
                        break;
                    String key = values.get(i).toString();
                    Object value = values.get(i + 1);
                    switch (key)
                    {
                        case ":description" :
                            description = value.toString();
                            break;
                        case ":parameters" :
                            if (value instanceof Cons c)
                            {
                                parameters = c;
                            }
                            break;
                        case ":fn" :
                            if (value instanceof Procedure p)
                            {
                                fn = p;
                            }
                            break;
                    }
                }
                if (fn == null)
                {
                    throw new JlllException("ai-tool requires :fn procedure");
                }
                return AITool.fromSpec(name, description, parameters, fn, env);
            }
        };
        // ========== ai-tool-add ==========
        new Primitive("ai-tool-add", env,
                "Adds a tool to a session. (ai-tool-add session tool) or (ai-tool-add tool) for current session")
        {
            private static final long serialVersionUID = 12L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session;
                AITool tool;
                if (values.length() >= 2 && values.get(0) instanceof AISession)
                {
                    session = (AISession) values.get(0);
                    tool = (AITool) values.get(1);
                }
                else if (values.length() >= 1 && values.get(0) instanceof AITool)
                {
                    session = getCurrentSession(env);
                    tool = (AITool) values.get(0);
                }
                else
                {
                    throw new JlllException("ai-tool-add requires a tool argument");
                }
                session.addTool(tool);
                return tool;
            }
        };
        // ========== ai-tool-remove ==========
        new Primitive("ai-tool-remove", env,
                "Removes a tool from a session by name. (ai-tool-remove session \"name\") or (ai-tool-remove \"name\")")
        {
            private static final long serialVersionUID = 13L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session;
                String toolName;
                if (values.length() >= 2 && values.get(0) instanceof AISession)
                {
                    session = (AISession) values.get(0);
                    toolName = values.get(1).toString();
                }
                else if (values.length() >= 1)
                {
                    session = getCurrentSession(env);
                    toolName = values.get(0).toString();
                }
                else
                {
                    throw new JlllException("ai-tool-remove requires a tool name");
                }
                AITool removed = session.removeTool(toolName);
                return removed != null ? removed : Null.NULL;
            }
        };
        // ========== ai-tools ==========
        new Primitive("ai-tools", env, "Lists tools in a session. (ai-tools) for current session or (ai-tools session)")
        {
            private static final long serialVersionUID = 14L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AISession session = values.length() > 0 ? getSessionArg(values, env, 0) : getCurrentSession(env);
                Map<String, AITool> tools = session.getTools();
                List<Object> result = new ArrayList<>();
                for (AITool tool : tools.values())
                {
                    Map<Object, Object> map = new LinkedHashMap<>();
                    map.put(Symbol.intern("name"), tool.getName());
                    map.put(Symbol.intern("description"), tool.getDescription());
                    result.add(map);
                }
                return Cons.list(result.toArray());
            }
        };
        // ========== ai-configure ==========
        new Primitive("ai-configure", env, "Configures AI settings. (ai-configure :openai-api-key \"sk-...\") or "
                + "(ai-configure :default-model \"gpt-4\") or (ai-configure :default-tier \"fast\")")
        {
            private static final long serialVersionUID = 15L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                AIConfig config = AIConfig.getInstance();
                for (int i = 0; i < values.length(); i += 2)
                {
                    if (i + 1 >= values.length())
                        break;
                    String key = values.get(i).toString();
                    Object value = values.get(i + 1);
                    switch (key)
                    {
                        case ":openai-api-key" :
                            config.setApiKey(AIConfig.Provider.OPENAI, value.toString());
                            break;
                        case ":anthropic-api-key" :
                            config.setApiKey(AIConfig.Provider.ANTHROPIC, value.toString());
                            break;
                        case ":google-ai-api-key" :
                            config.setApiKey(AIConfig.Provider.GOOGLE_AI, value.toString());
                            break;
                        case ":ollama-base-url" :
                            config.setApiKey(AIConfig.Provider.OLLAMA, value.toString());
                            break;
                        case ":default-model" :
                            config.setDefaultModel(value.toString());
                            break;
                        case ":default-tier" :
                            try
                            {
                                config.setDefaultTier(AIConfig.ModelTier.fromString(value.toString()));
                            }
                            catch (IllegalArgumentException e)
                            {
                                throw new JlllException("ai-configure: " + e.getMessage());
                            }
                            break;
                        case ":default-temperature" :
                            config.setDefaultTemperature(((Number) value).doubleValue());
                            break;
                    }
                }
                return Null.NULL;
            }
        };
        // ========== ai-config ==========
        new Primitive("ai-config", env, "Returns current AI configuration as a hash-map. (ai-config)")
        {
            private static final long serialVersionUID = 16L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<String, Object> config = AIConfig.getInstance().toMap();
                // Convert to JLLL hash-map with keyword keys
                Map<Object, Object> result = new LinkedHashMap<>();
                for (Map.Entry<String, Object> entry : config.entrySet())
                {
                    result.put(Symbol.intern(entry.getKey()), entry.getValue());
                }
                return result;
            }
        };
        // ========== ai-models ==========
        new Primitive("ai-models", env,
                "Returns all model configurations as a nested hash-map of provider -> tier -> model. (ai-models)")
        {
            private static final long serialVersionUID = 22L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Map<String, Map<String, String>> modelsMap = AIConfig.getInstance().getModelsMap();
                // Convert to JLLL hash-map with symbol keys
                Map<Object, Object> result = new LinkedHashMap<>();
                for (Map.Entry<String, Map<String, String>> providerEntry : modelsMap.entrySet())
                {
                    Map<Object, Object> tierMap = new LinkedHashMap<>();
                    for (Map.Entry<String, String> tierEntry : providerEntry.getValue().entrySet())
                    {
                        tierMap.put(Symbol.intern(tierEntry.getKey()), tierEntry.getValue());
                    }
                    result.put(Symbol.intern(providerEntry.getKey()), tierMap);
                }
                return result;
            }
        };
        // ========== ai-session? ==========
        new Primitive("ai-session?", env, "Returns true if the argument is an AI session. (ai-session? obj)")
        {
            private static final long serialVersionUID = 17L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                return values.length() > 0 && values.get(0) instanceof AISession;
            }
        };
        // ========== ai-tool? ==========
        new Primitive("ai-tool?", env, "Returns true if the argument is an AI tool. (ai-tool? obj)")
        {
            private static final long serialVersionUID = 18L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                return values.length() > 0 && values.get(0) instanceof AITool;
            }
        };
        // ========== ai-session-save ==========
        new Primitive("ai-session-save", env, "Saves an AI session to a file. "
                + "(ai-session-save session \"path.json\") or (ai-session-save \"path.json\") for current session. "
                + "Options: :pretty true for formatted output.")
        {
            private static final long serialVersionUID = 19L;
            private static final Gson GSON = new Gson();
            private static final Gson GSON_PRETTY = new GsonBuilder().setPrettyPrinting().create();

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("ai-session-save requires at least a file path argument");
                }
                AISession session;
                String path;
                int optionStart;
                // Determine if first arg is session or path
                Object firstArg = values.get(0);
                if (firstArg instanceof AISession)
                {
                    session = (AISession) firstArg;
                    if (values.length() < 2)
                    {
                        throw new JlllException("ai-session-save: missing file path");
                    }
                    path = values.get(1).toString();
                    optionStart = 2;
                }
                else
                {
                    session = getCurrentSession(env);
                    path = firstArg.toString();
                    optionStart = 1;
                }
                // Parse options
                boolean pretty = false;
                for (int i = optionStart; i < values.length(); i += 2)
                {
                    if (i + 1 >= values.length())
                        break;
                    Object key = values.get(i);
                    Object value = values.get(i + 1);
                    String keyStr = key instanceof Keyword ? ((Keyword) key).getName() : key.toString();
                    if ("pretty".equals(keyStr) || ":pretty".equals(keyStr))
                    {
                        pretty = Boolean.TRUE.equals(value);
                    }
                }
                // Create parent directories if needed
                Path filePath = Paths.get(path);
                Path parent = filePath.getParent();
                if (parent != null && !Files.exists(parent))
                {
                    try
                    {
                        Files.createDirectories(parent);
                    }
                    catch (IOException e)
                    {
                        throw new JlllException(
                                "ai-session-save: failed to create directory " + parent + ": " + e.getMessage());
                    }
                }
                // Serialize session to JSON
                Map<String, Object> sessionMap = session.toSerializableMap();
                String json = pretty ? GSON_PRETTY.toJson(sessionMap) : GSON.toJson(sessionMap);
                // Write to file using spit
                Jlll.invokeProcedure("spit", env, path, json);
                return session;
            }
        };
        // ========== ai-session-load ==========
        new Primitive("ai-session-load", env,
                "Loads an AI session from a file. " + "(ai-session-load \"path.json\") returns the restored session. "
                        + "Options: :name \"override\" to use different name, :activate true to make it current, "
                        + ":eval true/false to include eval tool (default true).")
        {
            private static final long serialVersionUID = 20L;
            private static final Gson GSON = new Gson();

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values.length() < 1)
                {
                    throw new JlllException("ai-session-load requires a file path argument");
                }
                String path = values.get(0).toString();
                // Parse options
                String nameOverride = null;
                boolean activate = false;
                boolean addEvalTool = true;
                for (int i = 1; i < values.length(); i += 2)
                {
                    if (i + 1 >= values.length())
                        break;
                    Object key = values.get(i);
                    Object value = values.get(i + 1);
                    String keyStr = key instanceof Keyword ? ((Keyword) key).getName() : key.toString();
                    switch (keyStr)
                    {
                        case "name", ":name" -> nameOverride = value.toString();
                        case "activate", ":activate" -> activate = Boolean.TRUE.equals(value);
                        case "eval", ":eval" -> addEvalTool = Boolean.TRUE.equals(value);
                    }
                }
                // Read file using slurp
                Object content = Jlll.invokeProcedure("slurp", env, path);
                if (content == null || content instanceof Null)
                {
                    throw new JlllException("ai-session-load: could not read file: " + path);
                }
                String json = content.toString();
                // Parse JSON to map
                Map<String, Object> sessionMap;
                try
                {
                    sessionMap = GSON.fromJson(json, new TypeToken<Map<String, Object>>()
                    {
                    }.getType());
                }
                catch (JsonSyntaxException e)
                {
                    throw new JlllException("ai-session-load: invalid JSON in " + path + " - " + e.getMessage());
                }
                if (sessionMap == null)
                {
                    throw new JlllException("ai-session-load: empty or invalid session file: " + path);
                }
                // Restore session
                AISession session = AISession.fromSerializableMap(sessionMap, env, nameOverride, addEvalTool);
                // Optionally activate
                if (activate)
                {
                    session.setEnvironment(env);
                    env.setBinding(AI_SESSION_SYMBOL, session);
                }
                return session;
            }
        };
        // ========== ai-session-auto-save ==========
        new Primitive("ai-session-auto-save", env,
                "Enables, disables, or queries auto-save for an AI session. "
                        + "(ai-session-auto-save \"path.json\") enables auto-save on current session. "
                        + "(ai-session-auto-save session \"path.json\") enables on specific session. "
                        + "(ai-session-auto-save false) disables auto-save. "
                        + "(ai-session-auto-save) returns current auto-save path or false.")
        {
            private static final long serialVersionUID = 21L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                int len = values.length();
                // No args: query current session
                if (len == 0)
                {
                    AISession session = getCurrentSession(env);
                    String path = session.getAutoSavePath();
                    return path != null ? path : Boolean.FALSE;
                }
                Object first = values.get(0);
                // One arg: session (query), path (enable on current), or false (disable on current)
                if (len == 1)
                {
                    if (first instanceof AISession session)
                    {
                        // Query specific session
                        String path = session.getAutoSavePath();
                        return path != null ? path : Boolean.FALSE;
                    }
                    else if (Boolean.FALSE.equals(first))
                    {
                        // Disable on current session
                        AISession session = getCurrentSession(env);
                        session.setAutoSavePath(null);
                        return Null.NULL;
                    }
                    else if (Boolean.TRUE.equals(first))
                    {
                        // Invalid: true is not a valid path
                        throw new JlllException("ai-session-auto-save: must provide a file path string, not true. "
                                + "Use (ai-session-auto-save \"path.json\") to enable auto-save.");
                    }
                    else
                    {
                        // Enable on current session with path
                        AISession session = getCurrentSession(env);
                        String path = first.toString();
                        session.setAutoSavePath(path);
                        // Perform initial save
                        session.performAutoSave(env);
                        return path;
                    }
                }
                // Two args: (session, path) or (session, false)
                if (len >= 2)
                {
                    Object second = values.get(1);
                    if (first instanceof AISession session)
                    {
                        if (Boolean.FALSE.equals(second))
                        {
                            // Disable on specific session
                            session.setAutoSavePath(null);
                            return Null.NULL;
                        }
                        else
                        {
                            // Enable on specific session
                            String path = second.toString();
                            session.setAutoSavePath(path);
                            // Perform initial save
                            session.performAutoSave(env);
                            return path;
                        }
                    }
                    else
                    {
                        throw new JlllException(
                                "ai-session-auto-save: first argument must be a session when two arguments are provided");
                    }
                }
                throw new JlllException("ai-session-auto-save: invalid arguments");
            }
        };
        // ========== ai-session-trace ==========
        new Primitive("ai-session-trace", env,
                "Enables tool call tracing for a session. When enabled, tool calls and results "
                        + "are printed to the console and stored in session history for persistence. "
                        + "(ai-session-trace) enables for current session. "
                        + "(ai-session-trace session) enables for specific session.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                AISession session;
                if (values.length() == 0)
                {
                    session = getCurrentSession(env);
                }
                else if (values.get(0) instanceof AISession s)
                {
                    session = s;
                }
                else
                {
                    throw new JlllException("ai-session-trace: argument must be a session");
                }
                session.setTraceToolCalls(true);
                return true;
            }
        };
        // ========== ai-session-untrace ==========
        new Primitive("ai-session-untrace", env,
                "Disables tool call tracing for a session. " + "(ai-session-untrace) disables for current session. "
                        + "(ai-session-untrace session) disables for specific session.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                AISession session;
                if (values.length() == 0)
                {
                    session = getCurrentSession(env);
                }
                else if (values.get(0) instanceof AISession s)
                {
                    session = s;
                }
                else
                {
                    throw new JlllException("ai-session-untrace: argument must be a session");
                }
                session.setTraceToolCalls(false);
                return false;
            }
        };
        // ========== ai-session-trace? ==========
        new Primitive("ai-session-trace?", env,
                "Returns true if tool call tracing is enabled for a session. "
                        + "(ai-session-trace?) checks current session. "
                        + "(ai-session-trace? session) checks specific session.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object apply(Cons values, Environment env) throws JlllException
            {
                AISession session;
                if (values.length() == 0)
                {
                    session = getCurrentSession(env);
                }
                else if (values.get(0) instanceof AISession s)
                {
                    session = s;
                }
                else
                {
                    throw new JlllException("ai-session-trace?: argument must be a session");
                }
                return session.isTraceToolCalls();
            }
        };
        // Load additional JLLL definitions (ai-session-restore, etc.)
        Jlll.eval("(load-system-script \"ai.jlll\")", env);
    }
    // ========== Helper Methods ==========

    /**
     * Gets a session argument, or current session if not specified.
     */
    private AISession getSessionArg(Cons values, Environment env, int index) throws JlllException
    {
        if (values.length() > index)
        {
            Object obj = values.get(index);
            if (obj instanceof AISession session)
            {
                return session;
            }
        }
        return getCurrentSession(env);
    }

    /**
     * Gets the current session from the environment.
     * Returns null if *ai-session* is null or not set.
     */
    private AISession getCurrentSessionOrNull(Environment env)
    {
        Object session = env.lookup(AI_SESSION_SYMBOL);
        if (session instanceof AISession s)
        {
            return s;
        }
        return null;
    }

    /**
     * Gets the current session from the environment.
     * Throws exception if no session is active.
     */
    private AISession getCurrentSession(Environment env) throws JlllException
    {
        AISession session = getCurrentSessionOrNull(env);
        if (session != null)
        {
            return session;
        }
        throw new JlllException("No active AI session. Use (ai-session-create) and (ai-session-activate) first.");
    }

    /**
     * Gets the current session or creates a default one.
     * Auto-created session is stored in *ai-session* using setBinding (set! semantics).
     */
    private AISession getCurrentOrCreateSession(Environment env) throws JlllException
    {
        AISession session = getCurrentSessionOrNull(env);
        if (session != null)
        {
            return session;
        }
        // Auto-create a default session and store in *ai-session*
        AIConfig config = AIConfig.getInstance();
        AIConfig.Provider provider = config.detectProvider();
        AISession newSession = new AISession(null, provider, null, null, env);
        newSession.addTool(AITool.createEvalTool(env));
        // Use setBinding to update the *ai-session* variable defined in init.jlll
        env.setBinding(AI_SESSION_SYMBOL, newSession);
        return newSession;
    }

    /**
     * Executes an AI request and returns a lazy sequence of response chunks.
     */
    private Object executeAiRequest(AISession session, String prompt, Double temperature, String modelOverride)
            throws JlllException
    {
        // Add user message to history
        session.addUserMessage(prompt);
        // Build messages list
        List<ChatMessage> messages = session.getMessagesForRequest();
        // Get tool specifications
        List<ToolSpecification> toolSpecs = session.getToolSpecifications();
        // Get the model
        StreamingChatModel model = session.getModel();
        // Create a blocking queue for streaming tokens
        BlockingQueue<Object> tokenQueue = new LinkedBlockingQueue<>();
        AtomicBoolean completed = new AtomicBoolean(false);
        AtomicReference<StringBuilder> fullResponse = new AtomicReference<>(new StringBuilder());
        AtomicReference<AiMessage> aiMessageRef = new AtomicReference<>();
        // Build chat request
        ChatRequest.Builder requestBuilder = ChatRequest.builder().messages(messages);
        if (!toolSpecs.isEmpty())
        {
            requestBuilder.toolSpecifications(toolSpecs);
        }
        // Note: temperature is set at model level, not request level in langchain4j
        // Debug logging before request
        boolean tracing = session.isTraceToolCalls();
        if (tracing)
        {
            debugLog("[DEBUG] Sending initial AI request:");
            debugLog("  messageCount: " + messages.size());
            debugLog("  toolSpecCount: " + toolSpecs.size());
            for (ToolSpecification spec : toolSpecs)
            {
                debugLog("  - tool: " + spec.name());
            }
        }
        // Start streaming request
        model.chat(requestBuilder.build(), new StreamingChatResponseHandler()
        {
            @Override
            public void onPartialResponse(String partialResponse)
            {
                if (partialResponse != null && !partialResponse.isEmpty())
                {
                    fullResponse.get().append(partialResponse);
                    tokenQueue.offer(partialResponse);
                }
            }

            @Override
            public void onCompleteResponse(ChatResponse completeResponse)
            {
                try
                {
                    AiMessage aiMessage = completeResponse.aiMessage();
                    aiMessageRef.set(aiMessage);
                    // Debug logging
                    if (tracing)
                    {
                        debugLog("[DEBUG] Initial response received:");
                        debugLog("  finishReason: " + completeResponse.finishReason());
                        debugLog("  metadata: " + completeResponse.metadata());
                        debugLog("  hasToolCalls: " + aiMessage.hasToolExecutionRequests());
                        debugLog("  textLength: " + (aiMessage.text() != null ? aiMessage.text().length() : 0));
                        if (aiMessage.hasToolExecutionRequests())
                        {
                            debugLog("  toolCallCount: " + aiMessage.toolExecutionRequests().size());
                            for (var req : aiMessage.toolExecutionRequests())
                            {
                                debugLog("  - toolCall: " + req.name() + " id=" + req.id());
                            }
                        }
                        else
                        {
                            // Log the actual text when there are no tool calls (might reveal why)
                            String text = aiMessage.text();
                            if (text != null && text.length() > 200)
                            {
                                debugLog("  text (truncated): " + text.substring(0, 200) + "...");
                            }
                            else
                            {
                                debugLog("  text: " + text);
                            }
                        }
                    }
                    // Check for tool calls
                    if (aiMessage.hasToolExecutionRequests())
                    {
                        try
                        {
                            handleToolCalls(session, aiMessage, tokenQueue, fullResponse, messages, toolSpecs, model,
                                    temperature, 0, tracing);
                        }
                        catch (Exception e)
                        {
                            if (tracing)
                            {
                                debugLog("[DEBUG] Exception in handleToolCalls: " + e.getMessage());
                                e.printStackTrace(System.err);
                            }
                            tokenQueue.offer(new StreamError(e));
                        }
                    }
                    else
                    {
                        // Add AI response to history
                        session.addAiMessage(aiMessage);
                        // Trigger auto-save if enabled
                        session.performAutoSave(session.getEnvironment());
                        completed.set(true);
                        tokenQueue.offer(END_OF_STREAM);
                    }
                }
                catch (Exception e)
                {
                    if (tracing)
                    {
                        debugLog("[DEBUG] Exception in initial onCompleteResponse: " + e.getMessage());
                        e.printStackTrace(System.err);
                    }
                    tokenQueue.offer(new StreamError(e));
                }
            }

            @Override
            public void onError(Throwable error)
            {
                if (tracing)
                {
                    debugLog("[DEBUG] Streaming error in initial request: " + error.getMessage());
                    error.printStackTrace(System.err);
                }
                tokenQueue.offer(new StreamError(error));
            }
        });
        // Return a lazy sequence that pulls from the queue
        return createLazyStream(tokenQueue);
    }

    /**
     * Handles tool execution requests from the AI.
     *
     * @param depth
     *            current recursion depth for tool calls (starts at 0)
     * @param tracing
     *            whether debug tracing is enabled
     */
    private void handleToolCalls(AISession session, AiMessage aiMessage, BlockingQueue<Object> tokenQueue,
            AtomicReference<StringBuilder> fullResponse, List<ChatMessage> messages, List<ToolSpecification> toolSpecs,
            StreamingChatModel model, Double temperature, int depth, boolean tracing)
    {
        // Check for maximum tool call depth to prevent infinite loops
        if (depth >= MAX_TOOL_CALL_DEPTH)
        {
            Console errorConsole = KernelLib.getConsole(session.getEnvironment());
            String errorMsg = "Error: Maximum tool call depth (" + MAX_TOOL_CALL_DEPTH + ") exceeded. "
                    + "This usually indicates repeated failures or an infinite loop. "
                    + "Please try a simpler request or break it into smaller steps.";
            errorConsole.println("[ERROR] " + errorMsg);
            if (tracing)
            {
                debugLog("[DEBUG] Max tool call depth exceeded at depth " + depth);
            }
            tokenQueue.offer(errorMsg);
            tokenQueue.offer(END_OF_STREAM);
            return;
        }
        Console console = tracing ? KernelLib.getConsole(session.getEnvironment()) : null;
        // If tracing, store the AI message with tool calls in history
        if (tracing)
        {
            session.addMessage(aiMessage);
        }
        // Execute each tool call
        List<ToolExecutionResultMessage> toolResults = new ArrayList<>();
        for (ToolExecutionRequest request : aiMessage.toolExecutionRequests())
        {
            String toolName = request.name();
            AITool tool = session.getTool(toolName);
            // Trace tool call if enabled
            if (tracing)
            {
                console.println("[TOOL CALL] " + toolName);
                console.println("  Arguments: " + request.arguments());
            }
            String result;
            if (tool != null)
            {
                result = tool.execute(request);
            }
            else
            {
                result = "Error: Unknown tool '" + toolName + "'";
            }
            // Trace tool result if enabled
            if (tracing)
            {
                console.println("[TOOL RESULT] " + toolName);
                // Truncate very long results for console output
                String displayResult = result.length() > 500 ? result.substring(0, 500) + "..." : result;
                console.println("  Result: " + displayResult);
            }
            ToolExecutionResultMessage resultMsg = ToolExecutionResultMessage.from(request, result);
            toolResults.add(resultMsg);
            // If tracing, store tool result in history
            if (tracing)
            {
                session.addMessage(resultMsg);
            }
        }
        // Add AI message and tool results to messages for next request
        List<ChatMessage> newMessages = new ArrayList<>(messages);
        newMessages.add(aiMessage);
        newMessages.addAll(toolResults);
        // Continue conversation with tool results
        ChatRequest.Builder requestBuilder = ChatRequest.builder().messages(newMessages);
        if (!toolSpecs.isEmpty())
        {
            requestBuilder.toolSpecifications(toolSpecs);
        }
        // Debug logging before continuation request
        if (tracing)
        {
            debugLog("[DEBUG] Sending continuation request (after tool results):");
            debugLog("  depth: " + depth);
            debugLog("  messageCount: " + newMessages.size());
            debugLog("  toolResultCount: " + toolResults.size());
            for (ToolExecutionResultMessage resultMsg : toolResults)
            {
                String text = resultMsg.text();
                String displayText = (text != null && text.length() > 100) ? text.substring(0, 100) + "..." : text;
                debugLog("  - toolResult[" + resultMsg.toolName() + "]: " + displayText);
            }
            debugLog("  toolSpecCount: " + toolSpecs.size());
            for (ToolSpecification spec : toolSpecs)
            {
                debugLog("  - tool: " + spec.name());
            }
        }
        // Note: temperature is set at model level, not request level
        model.chat(requestBuilder.build(), new StreamingChatResponseHandler()
        {
            @Override
            public void onPartialResponse(String partialResponse)
            {
                if (partialResponse != null && !partialResponse.isEmpty())
                {
                    fullResponse.get().append(partialResponse);
                    tokenQueue.offer(partialResponse);
                }
            }

            @Override
            public void onCompleteResponse(ChatResponse completeResponse)
            {
                try
                {
                    AiMessage finalMessage = completeResponse.aiMessage();
                    // Debug logging for continuation response
                    if (tracing)
                    {
                        debugLog("[DEBUG] Continuation response received (depth=" + depth + "):");
                        debugLog("  finishReason: " + completeResponse.finishReason());
                        debugLog("  metadata: " + completeResponse.metadata());
                        debugLog("  hasToolCalls: " + finalMessage.hasToolExecutionRequests());
                        debugLog("  textLength: " + (finalMessage.text() != null ? finalMessage.text().length() : 0));
                        if (finalMessage.hasToolExecutionRequests())
                        {
                            debugLog("  toolCallCount: " + finalMessage.toolExecutionRequests().size());
                            for (var req : finalMessage.toolExecutionRequests())
                            {
                                debugLog("  - toolCall: " + req.name() + " id=" + req.id());
                            }
                        }
                        else
                        {
                            // Log actual text when no tool calls - this is key for debugging!
                            String text = finalMessage.text();
                            if (text != null && text.length() > 200)
                            {
                                debugLog("  text (truncated): " + text.substring(0, 200) + "...");
                            }
                            else
                            {
                                debugLog("  text: " + text);
                            }
                        }
                    }
                    // Check for more tool calls (recursive)
                    if (finalMessage.hasToolExecutionRequests())
                    {
                        handleToolCalls(session, finalMessage, tokenQueue, fullResponse, newMessages, toolSpecs, model,
                                temperature, depth + 1, tracing);
                    }
                    else
                    {
                        // Add final AI response to history (skip intermediate tool messages)
                        session.addAiMessage(finalMessage);
                        // Trigger auto-save if enabled
                        session.performAutoSave(session.getEnvironment());
                        tokenQueue.offer(END_OF_STREAM);
                    }
                }
                catch (Exception e)
                {
                    if (tracing)
                    {
                        debugLog("[DEBUG] Exception in continuation onCompleteResponse (depth=" + depth + "): "
                                + e.getMessage());
                        e.printStackTrace(System.err);
                    }
                    tokenQueue.offer(new StreamError(e));
                }
            }

            @Override
            public void onError(Throwable error)
            {
                if (tracing)
                {
                    debugLog("[DEBUG] Streaming error in continuation (depth=" + depth + "): " + error.getMessage());
                    error.printStackTrace(System.err);
                }
                tokenQueue.offer(new StreamError(error));
            }
        });
    }

    /**
     * Extracts a user-friendly error message from an AI streaming error.
     */
    private String extractErrorMessage(Throwable error)
    {
        // Walk the cause chain to find useful information
        Throwable current = error;
        while (current != null)
        {
            String msg = current.getMessage();
            if (msg != null)
            {
                String lowerMsg = msg.toLowerCase();
                // Check for common API errors
                if (lowerMsg.contains("quota") || lowerMsg.contains("insufficient_quota"))
                {
                    return "AI API quota exceeded. Check your API plan and billing.";
                }
                if (lowerMsg.contains("rate_limit") || lowerMsg.contains("rate limit"))
                {
                    return "AI API rate limit reached. Please wait and try again.";
                }
                if (lowerMsg.contains("invalid_api_key") || lowerMsg.contains("invalid api key")
                        || lowerMsg.contains("unauthorized"))
                {
                    return "Invalid AI API key. Check your API key configuration.";
                }
                if (lowerMsg.contains("connection") || lowerMsg.contains("timeout"))
                {
                    return "AI API connection error. Check your network connection.";
                }
            }
            current = current.getCause();
        }
        // Fall back to generic message
        return "AI streaming error: " + (error.getMessage() != null ? error.getMessage() : error.getClass().getName());
    }

    /**
     * Creates a lazy sequence that pulls from a blocking queue.
     */
    private Object createLazyStream(BlockingQueue<Object> queue)
    {
        return createLazyStreamNode(queue);
    }

    /**
     * Creates a single node in the lazy stream.
     */
    private Object createLazyStreamNode(BlockingQueue<Object> queue)
    {
        try
        {
            // Wait for next token (with timeout to avoid hanging forever)
            Object token = queue.poll(60, TimeUnit.SECONDS);
            if (token == null)
            {
                // Timeout - end stream
                return Null.NULL;
            }
            if (token == END_OF_STREAM)
            {
                return Null.NULL;
            }
            if (token instanceof StreamError error)
            {
                // Try to extract a more helpful error message
                String errorMessage = extractErrorMessage(error.error);
                throw new RuntimeException(errorMessage, error.error);
            }
            // Return cons with lazy tail
            LazyThunk lazyTail = new LazyThunk(() -> createLazyStreamNode(queue));
            return new Cons(token.toString(), lazyTail);
        }
        catch (InterruptedException e)
        {
            Thread.currentThread().interrupt();
            return Null.NULL;
        }
    }
}
