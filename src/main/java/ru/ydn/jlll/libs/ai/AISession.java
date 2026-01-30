package ru.ydn.jlll.libs.ai;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.data.message.AiMessage;
import dev.langchain4j.data.message.ChatMessage;
import dev.langchain4j.data.message.SystemMessage;
import dev.langchain4j.data.message.UserMessage;
import dev.langchain4j.model.anthropic.AnthropicStreamingChatModel;
import dev.langchain4j.model.chat.StreamingChatLanguageModel;
import dev.langchain4j.model.googleai.GoogleAiGeminiStreamingChatModel;
import dev.langchain4j.model.ollama.OllamaStreamingChatModel;
import dev.langchain4j.model.openai.OpenAiStreamingChatModel;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;

/**
 * Represents an AI chat session with conversation memory and tools.
 *
 * <p>
 * A session encapsulates:
 * </p>
 * <ul>
 * <li>A streaming chat model (provider-agnostic)</li>
 * <li>Conversation history (chat memory)</li>
 * <li>Registered tools for function calling</li>
 * <li>System prompt and configuration</li>
 * </ul>
 *
 * <p>
 * Sessions are identified by a unique ID and optionally a user-friendly name.
 * </p>
 */
public class AISession implements Serializable
{
    private static final long serialVersionUID = 1L;
    /** Counter for generating unique session IDs */
    private static final AtomicLong SESSION_COUNTER = new AtomicLong(0);
    /** Global session registry */
    private static final Map<String, AISession> SESSIONS = new ConcurrentHashMap<>();
    /** Unique session identifier */
    private final String id;
    /** Optional user-friendly name */
    private final String name;
    /** The provider used by this session */
    private final AIConfig.Provider provider;
    /** The model name */
    private final String modelName;
    /** System message (prompt) */
    private String systemPrompt;
    /** Conversation history */
    private final List<ChatMessage> history = Collections.synchronizedList(new ArrayList<>());
    /** Registered tools (name -> AITool) */
    private final Map<String, AITool> tools = new ConcurrentHashMap<>();
    /** Tool specifications for LangChain4j */
    private final Map<String, ToolSpecification> toolSpecs = new ConcurrentHashMap<>();
    /** The streaming chat model (transient - rebuilt on demand) */
    private transient volatile StreamingChatLanguageModel model;
    /** Temperature setting (null = provider default) */
    private Double temperature;
    /** Max tokens setting (null = provider default) */
    private Integer maxTokens;
    /** Reference to the environment where this session was created */
    private transient Environment environment;
    /** Path for auto-saving session after each interaction (null = disabled) */
    private String autoSavePath;

    /**
     * Creates a new AI session.
     *
     * @param name
     *            optional user-friendly name (null for auto-generated)
     * @param provider
     *            the AI provider to use
     * @param modelName
     *            the model name (null for provider default)
     * @param systemPrompt
     *            the system prompt (null for default JLLL prompt)
     * @param environment
     *            the JLLL environment for tool execution
     */
    public AISession(String name, AIConfig.Provider provider, String modelName, String systemPrompt,
            Environment environment)
    {
        this.id = "sess-" + SESSION_COUNTER.incrementAndGet();
        this.name = name != null ? name : this.id;
        this.provider = provider;
        this.modelName = modelName != null ? modelName : AIConfig.getInstance().getDefaultModel(provider);
        this.systemPrompt = systemPrompt != null ? systemPrompt : getDefaultSystemPrompt();
        this.environment = environment;
        // Register in global registry
        SESSIONS.put(this.id, this);
    }

    /**
     * Creates a new AI session with a specific ID (for restoring saved sessions).
     * If the ID collides with an existing session, a new unique ID is generated.
     *
     * @param requestedId
     *            the requested session ID
     * @param name
     *            optional user-friendly name (null for auto-generated)
     * @param provider
     *            the AI provider to use
     * @param modelName
     *            the model name (null for provider default)
     * @param systemPrompt
     *            the system prompt (null for default JLLL prompt)
     * @param environment
     *            the JLLL environment for tool execution
     */
    public AISession(String requestedId, String name, AIConfig.Provider provider, String modelName, String systemPrompt,
            Environment environment)
    {
        // Handle ID collision by generating a unique suffix
        String actualId = requestedId;
        if (SESSIONS.containsKey(requestedId))
        {
            int suffix = 1;
            while (SESSIONS.containsKey(requestedId + "-restored-" + suffix))
            {
                suffix++;
            }
            actualId = requestedId + "-restored-" + suffix;
        }
        this.id = actualId;
        this.name = name != null ? name : this.id;
        this.provider = provider;
        this.modelName = modelName != null ? modelName : AIConfig.getInstance().getDefaultModel(provider);
        this.systemPrompt = systemPrompt != null ? systemPrompt : getDefaultSystemPrompt();
        this.environment = environment;
        // Register in global registry
        SESSIONS.put(this.id, this);
    }

    /**
     * Gets the default system prompt for JLLL AI sessions.
     * Loads from docs/system-prompt.md resource if available, otherwise falls back to minimal prompt.
     *
     * @return the default system prompt
     */
    public static String getDefaultSystemPrompt()
    {
        // Try to load from packaged documentation
        try (InputStream is = AISession.class.getClassLoader().getResourceAsStream("docs/system-prompt.md"))
        {
            if (is != null)
            {
                return new String(is.readAllBytes(), StandardCharsets.UTF_8);
            }
        }
        catch (IOException e)
        {
            // Fall through to default
        }
        // Fallback minimal prompt
        return """
                You are an AI assistant with access to JLLL (Java Lisp-Like Language) environment.

                CRITICAL: Quote class names in Java interop: (new 'java.util.Date) NOT (new java.util.Date)

                Discovery functions:
                - (apropos "keyword") - search for functions
                - (doc 'function) - get documentation
                - (jlll-docs) - list documentation topics
                - (jlll-docs "topic") - read full documentation

                If you get "unbound symbol" error, you likely forgot to quote a class name.
                """;
    }

    /**
     * Gets or creates the streaming chat model for this session.
     *
     * @return the streaming chat model
     */
    public synchronized StreamingChatLanguageModel getModel()
    {
        if (model == null)
        {
            model = buildModel();
        }
        return model;
    }

    /**
     * Builds the appropriate streaming chat model based on provider.
     *
     * @return the streaming chat model
     */
    private StreamingChatLanguageModel buildModel()
    {
        AIConfig config = AIConfig.getInstance();
        String apiKey = config.getApiKey(provider);
        switch (provider)
        {
            case OPENAI :
                var openaiBuilder = OpenAiStreamingChatModel.builder().apiKey(apiKey).modelName(modelName);
                if (temperature != null)
                    openaiBuilder.temperature(temperature);
                if (maxTokens != null)
                    openaiBuilder.maxTokens(maxTokens);
                return openaiBuilder.build();
            case ANTHROPIC :
                var anthropicBuilder = AnthropicStreamingChatModel.builder().apiKey(apiKey).modelName(modelName);
                if (temperature != null)
                    anthropicBuilder.temperature(temperature);
                if (maxTokens != null)
                    anthropicBuilder.maxTokens(maxTokens);
                return anthropicBuilder.build();
            case GOOGLE_AI :
                var geminiBuilder = GoogleAiGeminiStreamingChatModel.builder().apiKey(apiKey).modelName(modelName);
                if (temperature != null)
                    geminiBuilder.temperature(temperature);
                if (maxTokens != null)
                    geminiBuilder.maxOutputTokens(maxTokens);
                return geminiBuilder.build();
            case OLLAMA :
                // For Ollama, the "API key" is actually the base URL
                String baseUrl = apiKey;
                if (baseUrl == null || baseUrl.isBlank())
                {
                    baseUrl = "http://localhost:11434";
                }
                var ollamaBuilder = OllamaStreamingChatModel.builder().baseUrl(baseUrl).modelName(modelName);
                if (temperature != null)
                    ollamaBuilder.temperature(temperature);
                return ollamaBuilder.build();
            default :
                throw new IllegalStateException("Unknown provider: " + provider);
        }
    }

    /**
     * Rebuilds the model (call after changing settings).
     */
    public synchronized void rebuildModel()
    {
        model = null;
    }
    // ========== Session Identity ==========

    /**
     * Returns the unique session ID.
     *
     * @return the session ID
     */
    public String getId()
    {
        return id;
    }

    /**
     * Returns the session name.
     *
     * @return the session name
     */
    public String getName()
    {
        return name;
    }

    /**
     * Returns the provider used by this session.
     *
     * @return the provider
     */
    public AIConfig.Provider getProvider()
    {
        return provider;
    }

    /**
     * Returns the model name.
     *
     * @return the model name
     */
    public String getModelName()
    {
        return modelName;
    }
    // ========== System Prompt ==========

    /**
     * Returns the system prompt.
     *
     * @return the system prompt
     */
    public String getSystemPrompt()
    {
        return systemPrompt;
    }

    /**
     * Sets the system prompt.
     *
     * @param systemPrompt
     *            the new system prompt
     */
    public void setSystemPrompt(String systemPrompt)
    {
        this.systemPrompt = systemPrompt;
    }
    // ========== Conversation History ==========

    /**
     * Adds a user message to the history.
     *
     * @param content
     *            the message content
     */
    public void addUserMessage(String content)
    {
        history.add(UserMessage.from(content));
    }

    /**
     * Adds an AI message to the history.
     *
     * @param content
     *            the message content
     */
    public void addAiMessage(String content)
    {
        history.add(AiMessage.from(content));
    }

    /**
     * Adds an AI message to the history.
     *
     * @param message
     *            the AI message
     */
    public void addAiMessage(AiMessage message)
    {
        history.add(message);
    }

    /**
     * Returns all messages for a chat request (system + history).
     *
     * @return list of chat messages
     */
    public List<ChatMessage> getMessagesForRequest()
    {
        List<ChatMessage> messages = new ArrayList<>();
        if (systemPrompt != null && !systemPrompt.isBlank())
        {
            messages.add(SystemMessage.from(systemPrompt));
        }
        messages.addAll(history);
        return messages;
    }

    /**
     * Returns the conversation history (without system message).
     *
     * @return list of chat messages
     */
    public List<ChatMessage> getHistory()
    {
        return new ArrayList<>(history);
    }

    /**
     * Clears the conversation history.
     */
    public void clearHistory()
    {
        history.clear();
    }
    // ========== Tool Management ==========

    /**
     * Registers a tool with this session.
     * The tool's environment is updated to use this session's environment.
     *
     * @param tool
     *            the tool to register
     */
    public void addTool(AITool tool)
    {
        // Update tool's environment to use session's environment
        if (environment != null)
        {
            tool.setEnvironment(environment);
        }
        tools.put(tool.getName(), tool);
        toolSpecs.put(tool.getName(), tool.getSpecification());
    }

    /**
     * Removes a tool from this session.
     *
     * @param name
     *            the tool name
     * @return the removed tool, or null if not found
     */
    public AITool removeTool(String name)
    {
        toolSpecs.remove(name);
        return tools.remove(name);
    }

    /**
     * Gets a tool by name.
     *
     * @param name
     *            the tool name
     * @return the tool, or null if not found
     */
    public AITool getTool(String name)
    {
        return tools.get(name);
    }

    /**
     * Returns all registered tools.
     *
     * @return map of tool names to tools
     */
    public Map<String, AITool> getTools()
    {
        return new LinkedHashMap<>(tools);
    }

    /**
     * Returns tool specifications for LangChain4j.
     *
     * @return list of tool specifications
     */
    public List<ToolSpecification> getToolSpecifications()
    {
        return new ArrayList<>(toolSpecs.values());
    }

    /**
     * Checks if a tool is registered.
     *
     * @param name
     *            the tool name
     * @return true if the tool is registered
     */
    public boolean hasTool(String name)
    {
        return tools.containsKey(name);
    }
    // ========== Settings ==========

    /**
     * Gets the temperature setting.
     *
     * @return the temperature, or null if using provider default
     */
    public Double getTemperature()
    {
        return temperature;
    }

    /**
     * Sets the temperature setting.
     *
     * @param temperature
     *            the temperature (0.0-2.0), or null for provider default
     */
    public void setTemperature(Double temperature)
    {
        this.temperature = temperature;
        rebuildModel();
    }

    /**
     * Gets the max tokens setting.
     *
     * @return the max tokens, or null if using provider default
     */
    public Integer getMaxTokens()
    {
        return maxTokens;
    }

    /**
     * Sets the max tokens setting.
     *
     * @param maxTokens
     *            the max tokens, or null for provider default
     */
    public void setMaxTokens(Integer maxTokens)
    {
        this.maxTokens = maxTokens;
        rebuildModel();
    }
    // ========== Environment ==========

    /**
     * Gets the JLLL environment for this session.
     *
     * @return the environment
     */
    public Environment getEnvironment()
    {
        return environment;
    }

    /**
     * Sets the JLLL environment for this session.
     * Also updates the environment for all registered tools so they execute in the correct context.
     *
     * @param environment
     *            the environment
     */
    public void setEnvironment(Environment environment)
    {
        this.environment = environment;
        // Update environment for all tools so they execute in the correct context
        for (AITool tool : tools.values())
        {
            tool.setEnvironment(environment);
        }
    }
    // ========== Auto-Save Methods ==========

    /**
     * Gets the auto-save path for this session.
     *
     * @return the auto-save path, or null if auto-save is disabled
     */
    public String getAutoSavePath()
    {
        return autoSavePath;
    }

    /**
     * Sets the auto-save path for this session.
     * When set, the session will be automatically saved to this path after each AI interaction.
     *
     * @param path
     *            the file path for auto-save, or null to disable
     */
    public void setAutoSavePath(String path)
    {
        this.autoSavePath = path;
    }

    /**
     * Performs an auto-save if auto-save is enabled.
     * This should be called after each AI interaction completes.
     * Errors are printed as warnings but do not throw exceptions.
     *
     * @param env
     *            the environment for file operations
     */
    public void performAutoSave(Environment env)
    {
        if (autoSavePath == null || autoSavePath.isEmpty())
        {
            return;
        }
        try
        {
            // Create parent directories if needed
            Path filePath = Paths.get(autoSavePath);
            Path parent = filePath.getParent();
            if (parent != null && !Files.exists(parent))
            {
                Files.createDirectories(parent);
            }
            // Serialize and save
            Map<String, Object> sessionMap = toSerializableMap();
            String json = new com.google.gson.Gson().toJson(sessionMap);
            // Use Jlll.invokeProcedure to call spit
            ru.ydn.jlll.common.Jlll.invokeProcedure("spit", env, autoSavePath, json);
        }
        catch (Exception e)
        {
            System.err.println("Warning: Auto-save failed for session " + name + ": " + e.getMessage());
        }
    }
    // ========== Static Registry Methods ==========

    /**
     * Gets a session by ID.
     *
     * @param id
     *            the session ID
     * @return the session, or null if not found
     */
    public static AISession getById(String id)
    {
        return SESSIONS.get(id);
    }

    /**
     * Gets all registered sessions.
     *
     * @return list of all sessions
     */
    public static List<AISession> getAllSessions()
    {
        return new ArrayList<>(SESSIONS.values());
    }

    /**
     * Removes a session from the registry.
     *
     * @param id
     *            the session ID
     * @return the removed session, or null if not found
     */
    public static AISession removeSession(String id)
    {
        return SESSIONS.remove(id);
    }

    /**
     * Clears all sessions from the registry.
     */
    public static void clearAllSessions()
    {
        SESSIONS.clear();
    }

    @Override
    public String toString()
    {
        return "AISession[id=" + id + ", name=" + name + ", provider=" + provider + ", model=" + modelName + ", tools="
                + tools.size() + "]";
    }

    // ========== Serialization Methods ==========
    /** Current serialization format version */
    public static final int SERIALIZATION_VERSION = 1;

    /**
     * Converts this session to a serializable map for JSON persistence.
     * The map can be written to JSON and later restored via {@link #fromSerializableMap}.
     *
     * <p>
     * The built-in eval tool is excluded from serialization (it's always recreated on load).
     * </p>
     *
     * @return a map containing session state
     */
    public Map<String, Object> toSerializableMap()
    {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("version", SERIALIZATION_VERSION);
        map.put("id", id);
        map.put("name", name);
        map.put("provider", provider.name());
        map.put("modelName", modelName);
        map.put("systemPrompt", systemPrompt);
        map.put("temperature", temperature);
        map.put("maxTokens", maxTokens);
        // Serialize history
        List<Map<String, Object>> historyList = new ArrayList<>();
        for (ChatMessage msg : history)
        {
            Map<String, Object> msgMap = new LinkedHashMap<>();
            msgMap.put("type", msg.type().name().toLowerCase());
            if (msg instanceof UserMessage um)
            {
                msgMap.put("content", um.singleText());
            }
            else if (msg instanceof AiMessage am)
            {
                msgMap.put("content", am.text());
            }
            historyList.add(msgMap);
        }
        map.put("history", historyList);
        // Serialize custom tools (exclude built-in eval tool)
        List<Map<String, Object>> toolsList = new ArrayList<>();
        for (AITool tool : tools.values())
        {
            if (!tool.isBuiltIn())
            {
                Map<String, Object> toolMap = tool.toSerializableMap();
                if (toolMap != null)
                {
                    toolsList.add(toolMap);
                }
            }
        }
        map.put("tools", toolsList);
        // Save auto-save path if set
        if (autoSavePath != null)
        {
            map.put("autoSavePath", autoSavePath);
        }
        return map;
    }

    /**
     * Creates an AISession from a serialized map (typically loaded from JSON).
     *
     * @param map
     *            the serialized session data
     * @param env
     *            the environment for tool execution and procedure evaluation
     * @param nameOverride
     *            optional name to use instead of saved name (null to use saved name)
     * @param addEvalTool
     *            whether to add the built-in eval tool
     * @return the restored session
     * @throws JlllException
     *             if the map is invalid or tools cannot be restored
     */
    @SuppressWarnings("unchecked")
    public static AISession fromSerializableMap(Map<String, Object> map, Environment env, String nameOverride,
            boolean addEvalTool) throws JlllException
    {
        // Validate version
        Object versionObj = map.get("version");
        int version = versionObj instanceof Number ? ((Number) versionObj).intValue() : 0;
        if (version > SERIALIZATION_VERSION)
        {
            throw new JlllException("ai-session-load: unsupported session file version " + version + " (max supported: "
                    + SERIALIZATION_VERSION + "). Please update JLLL.");
        }
        if (version < 1)
        {
            throw new JlllException("ai-session-load: invalid or missing version in session file");
        }
        // Extract basic fields
        String savedId = (String) map.get("id");
        String savedName = (String) map.get("name");
        String providerStr = (String) map.get("provider");
        String modelName = (String) map.get("modelName");
        String systemPrompt = (String) map.get("systemPrompt");
        // Parse provider
        AIConfig.Provider provider;
        try
        {
            provider = AIConfig.Provider.valueOf(providerStr);
        }
        catch (IllegalArgumentException e)
        {
            throw new JlllException("ai-session-load: unknown provider '" + providerStr + "'. Valid providers: "
                    + java.util.Arrays.toString(AIConfig.Provider.values()));
        }
        // Use name override if provided
        String name = nameOverride != null ? nameOverride : savedName;
        // Create session with requested ID (may be modified if collision)
        AISession session = new AISession(savedId, name, provider, modelName, systemPrompt, env);
        // Restore optional settings
        Object tempObj = map.get("temperature");
        if (tempObj instanceof Number)
        {
            session.setTemperature(((Number) tempObj).doubleValue());
        }
        Object maxTokensObj = map.get("maxTokens");
        if (maxTokensObj instanceof Number)
        {
            session.setMaxTokens(((Number) maxTokensObj).intValue());
        }
        // Restore history
        Object historyObj = map.get("history");
        if (historyObj instanceof List)
        {
            for (Object item : (List<?>) historyObj)
            {
                if (item instanceof Map)
                {
                    Map<String, Object> msgMap = (Map<String, Object>) item;
                    String type = (String) msgMap.get("type");
                    String content = (String) msgMap.get("content");
                    if ("user".equals(type) && content != null)
                    {
                        session.addUserMessage(content);
                    }
                    else if ("ai".equals(type) && content != null)
                    {
                        session.addAiMessage(content);
                    }
                }
            }
        }
        // Add eval tool if requested (before custom tools so custom can override if needed)
        if (addEvalTool)
        {
            session.addTool(AITool.createEvalTool(env));
        }
        // Restore custom tools
        Object toolsObj = map.get("tools");
        if (toolsObj instanceof List)
        {
            for (Object item : (List<?>) toolsObj)
            {
                if (item instanceof Map)
                {
                    try
                    {
                        AITool tool = AITool.fromSerializableMap((Map<String, Object>) item, env);
                        if (tool != null)
                        {
                            session.addTool(tool);
                        }
                    }
                    catch (Exception e)
                    {
                        // Log warning but continue - don't fail entire load for one bad tool
                        System.err.println("Warning: Could not restore tool: " + e.getMessage());
                    }
                }
            }
        }
        // Restore auto-save path if present
        Object autoSaveObj = map.get("autoSavePath");
        if (autoSaveObj instanceof String)
        {
            session.setAutoSavePath((String) autoSaveObj);
        }
        return session;
    }
}
