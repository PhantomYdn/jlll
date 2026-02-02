package ru.ydn.jlll.libs.ai;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Global configuration for AI services with tier-based model selection.
 *
 * <p>
 * Manages API keys, model tiers, and provider detection. Configuration is loaded from
 * a JSON resource file and can be overridden by user configuration files.
 * </p>
 *
 * <h3>Model Tiers</h3>
 * <p>
 * Models are organized into three tiers:
 * </p>
 * <ul>
 * <li><b>best</b> - Most capable models (claude-opus-4, o1)</li>
 * <li><b>balanced</b> - Good balance of capability and cost (claude-sonnet-4, gpt-4o) [DEFAULT]</li>
 * <li><b>fast</b> - Fastest/cheapest models (claude-3-5-haiku, gpt-4o-mini)</li>
 * </ul>
 *
 * <h3>Configuration Override Priority</h3>
 * <ol>
 * <li>JLLL_AI_MODELS_PATH environment variable (explicit path)</li>
 * <li>./models.json (local project override)</li>
 * <li>~/.jlll/models.json (user global override)</li>
 * <li>Bundled resource file (default)</li>
 * </ol>
 *
 * <p>
 * Configurations are merged, so users can override just specific providers or tiers.
 * </p>
 */
public class AIConfig
{
    /** Singleton instance */
    private static final AIConfig INSTANCE = new AIConfig();
    /** Resource path for bundled models.json */
    private static final String RESOURCE_PATH = "ru/ydn/jlll/libs/ai/models.json";
    /** Environment variable for custom config path */
    private static final String ENV_CONFIG_PATH = "JLLL_AI_MODELS_PATH";
    /** User config directory name */
    private static final String USER_CONFIG_DIR = ".jlll";
    /** Config file name */
    private static final String CONFIG_FILE_NAME = "models.json";

    /**
     * Model tier enumeration representing capability levels.
     */
    public enum ModelTier
    {
        /** Most capable models - best reasoning, highest cost */
        BEST("best"),
        /** Balanced capability and cost - good default choice */
        BALANCED("balanced"),
        /** Fastest and cheapest models */
        FAST("fast");

        private final String jsonName;

        ModelTier(String jsonName)
        {
            this.jsonName = jsonName;
        }

        /**
         * Gets the JSON key name for this tier.
         *
         * @return the JSON key name
         */
        public String getJsonName()
        {
            return jsonName;
        }

        /**
         * Parses a tier from a string value.
         *
         * @param value
         *            the string value (case-insensitive)
         * @return the corresponding ModelTier
         * @throws IllegalArgumentException
         *             if the value doesn't match any tier
         */
        public static ModelTier fromString(String value)
        {
            if (value == null)
            {
                throw new IllegalArgumentException("Tier value cannot be null");
            }
            String lower = value.toLowerCase().trim();
            for (ModelTier tier : values())
            {
                if (tier.jsonName.equals(lower) || tier.name().equalsIgnoreCase(lower))
                {
                    return tier;
                }
            }
            throw new IllegalArgumentException("Unknown tier: " + value + ". Valid values: best, balanced, fast");
        }
    }

    /**
     * Provider enumeration for AI services.
     * Kept for backwards compatibility - provider names now come from config.
     */
    public enum Provider
    {
        /** Anthropic Claude models */
        ANTHROPIC,
        /** OpenAI GPT models */
        OPENAI,
        /** Google Gemini models */
        GOOGLE_AI,
        /** Local Ollama models */
        OLLAMA;

        /**
         * Gets the provider from a string name.
         *
         * @param name
         *            the provider name (case-insensitive)
         * @return the Provider enum value
         * @throws IllegalArgumentException
         *             if name doesn't match any provider
         */
        public static Provider fromString(String name)
        {
            if (name == null)
            {
                throw new IllegalArgumentException("Provider name cannot be null");
            }
            String normalized = name.toLowerCase().trim().replace("-", "_").replace(" ", "_");
            // Handle common aliases
            return switch (normalized)
            {
                case "anthropic" -> ANTHROPIC;
                case "openai" -> OPENAI;
                case "google", "google_ai", "googleai", "gemini" -> GOOGLE_AI;
                case "ollama" -> OLLAMA;
                default -> throw new IllegalArgumentException(
                        "Unknown provider: " + name + ". Valid values: anthropic, openai, google, ollama");
            };
        }
    }

    /**
     * Configuration for a single provider.
     */
    public static class ProviderConfig
    {
        private final String envVar;
        private final Map<ModelTier, String> tiers;
        private final Integer maxOutputTokens;

        /**
         * Creates a new provider configuration.
         *
         * @param envVar
         *            the environment variable for the API key
         * @param tiers
         *            the tier-to-model mappings
         * @param maxOutputTokens
         *            the max output tokens override for this provider, or null to use global default
         */
        public ProviderConfig(String envVar, Map<ModelTier, String> tiers, Integer maxOutputTokens)
        {
            this.envVar = envVar;
            this.tiers = Collections.unmodifiableMap(new EnumMap<>(tiers));
            this.maxOutputTokens = maxOutputTokens;
        }

        /**
         * Creates a new provider configuration without max output tokens override.
         *
         * @param envVar
         *            the environment variable for the API key
         * @param tiers
         *            the tier-to-model mappings
         */
        public ProviderConfig(String envVar, Map<ModelTier, String> tiers)
        {
            this(envVar, tiers, null);
        }

        /**
         * Gets the environment variable name for this provider's API key.
         *
         * @return the environment variable name
         */
        public String getEnvVar()
        {
            return envVar;
        }

        /**
         * Gets the model name for a specific tier.
         *
         * @param tier
         *            the model tier
         * @return the model name, or null if not defined
         */
        public String getModel(ModelTier tier)
        {
            return tiers.get(tier);
        }

        /**
         * Gets all tier mappings for this provider.
         *
         * @return unmodifiable map of tier to model name
         */
        public Map<ModelTier, String> getTiers()
        {
            return tiers;
        }

        /**
         * Gets the max output tokens override for this provider.
         *
         * @return the max output tokens, or null to use global default
         */
        public Integer getMaxOutputTokens()
        {
            return maxOutputTokens;
        }
    }
    // Instance fields

    /** Provider configurations loaded from JSON */
    private volatile Map<Provider, ProviderConfig> providerConfigs = new ConcurrentHashMap<>();
    /** Provider priority order for auto-detection */
    private volatile List<Provider> providerPriority = new ArrayList<>();
    /** Default model tier */
    private volatile ModelTier defaultTier = ModelTier.BALANCED;
    /** Programmatic overrides for API keys (takes precedence over env vars) */
    private final Map<Provider, String> keyOverrides = new ConcurrentHashMap<>();
    /** Default model override (if set, used instead of tier-based selection) */
    private volatile String defaultModelOverride = null;
    /** Default temperature for AI requests */
    private volatile Double defaultTemperature = null;
    /** Default max output tokens for AI responses */
    private volatile Integer defaultMaxOutputTokens = 16384;
    /** Sources that were loaded (for debugging) */
    private volatile List<String> loadedSources = new ArrayList<>();

    private AIConfig()
    {
        loadConfiguration();
    }

    /**
     * Returns the singleton configuration instance.
     *
     * @return the global AIConfig instance
     */
    public static AIConfig getInstance()
    {
        return INSTANCE;
    }
    // ========== Configuration Loading ==========

    /**
     * Loads configuration from all sources (resource, user, local, env var).
     * Can be called to reload configuration at runtime.
     */
    public synchronized void loadConfiguration()
    {
        Map<Provider, ProviderConfig> configs = new ConcurrentHashMap<>();
        List<Provider> priority = new ArrayList<>();
        ModelTier tier = ModelTier.BALANCED;
        List<String> sources = new ArrayList<>();
        // 1. Load from bundled resource (base configuration)
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(RESOURCE_PATH))
        {
            if (is != null)
            {
                JsonObject json = parseJson(is);
                tier = loadFromJson(json, configs, priority, tier);
                sources.add("resource:" + RESOURCE_PATH);
            }
        }
        catch (IOException e)
        {
            // Log but continue - bundled resource should always exist
            System.err.println("Warning: Failed to load bundled AI config: " + e.getMessage());
        }
        // 2. Load from user home (~/.jlll/models.json)
        Path userConfig = Paths.get(System.getProperty("user.home"), USER_CONFIG_DIR, CONFIG_FILE_NAME);
        if (Files.exists(userConfig))
        {
            try
            {
                JsonObject json = parseJson(userConfig);
                tier = loadFromJson(json, configs, priority, tier);
                sources.add("user:" + userConfig);
            }
            catch (IOException e)
            {
                System.err.println("Warning: Failed to load user AI config: " + e.getMessage());
            }
        }
        // 3. Load from local directory (./models.json)
        Path localConfig = Paths.get(CONFIG_FILE_NAME);
        if (Files.exists(localConfig))
        {
            try
            {
                JsonObject json = parseJson(localConfig);
                tier = loadFromJson(json, configs, priority, tier);
                sources.add("local:" + localConfig.toAbsolutePath());
            }
            catch (IOException e)
            {
                System.err.println("Warning: Failed to load local AI config: " + e.getMessage());
            }
        }
        // 4. Load from env var path (highest priority)
        String envPath = System.getenv(ENV_CONFIG_PATH);
        if (envPath != null && !envPath.isBlank())
        {
            Path envConfig = Paths.get(envPath);
            if (Files.exists(envConfig))
            {
                try
                {
                    JsonObject json = parseJson(envConfig);
                    tier = loadFromJson(json, configs, priority, tier);
                    sources.add("env:" + envConfig);
                }
                catch (IOException e)
                {
                    System.err.println(
                            "Warning: Failed to load AI config from " + ENV_CONFIG_PATH + ": " + e.getMessage());
                }
            }
            else
            {
                System.err.println("Warning: " + ENV_CONFIG_PATH + " points to non-existent file: " + envPath);
            }
        }
        // Apply loaded configuration
        this.providerConfigs = configs;
        this.providerPriority = priority;
        this.defaultTier = tier;
        this.loadedSources = sources;
    }

    /**
     * Parses JSON from an input stream.
     */
    private JsonObject parseJson(InputStream is) throws IOException
    {
        try (Reader reader = new InputStreamReader(is, StandardCharsets.UTF_8))
        {
            return JsonParser.parseReader(reader).getAsJsonObject();
        }
    }

    /**
     * Parses JSON from a file path.
     */
    private JsonObject parseJson(Path path) throws IOException
    {
        try (Reader reader = Files.newBufferedReader(path, StandardCharsets.UTF_8))
        {
            return JsonParser.parseReader(reader).getAsJsonObject();
        }
    }

    /**
     * Loads configuration from a JSON object, merging into existing config.
     *
     * @return the default tier (may be updated from JSON)
     */
    private ModelTier loadFromJson(JsonObject json, Map<Provider, ProviderConfig> configs, List<Provider> priority,
            ModelTier currentTier)
    {
        ModelTier tier = currentTier;
        // Load default tier
        if (json.has("defaultTier"))
        {
            try
            {
                tier = ModelTier.fromString(json.get("defaultTier").getAsString());
            }
            catch (IllegalArgumentException e)
            {
                System.err.println("Warning: Invalid defaultTier in config: " + e.getMessage());
            }
        }
        // Load default max output tokens
        if (json.has("defaultMaxOutputTokens"))
        {
            this.defaultMaxOutputTokens = json.get("defaultMaxOutputTokens").getAsInt();
        }
        // Load provider priority (replaces if specified)
        if (json.has("providerPriority"))
        {
            priority.clear();
            for (JsonElement elem : json.getAsJsonArray("providerPriority"))
            {
                try
                {
                    priority.add(Provider.fromString(elem.getAsString()));
                }
                catch (IllegalArgumentException e)
                {
                    System.err.println("Warning: Unknown provider in priority list: " + elem);
                }
            }
        }
        // Load provider configs (merges)
        if (json.has("providers"))
        {
            JsonObject providers = json.getAsJsonObject("providers");
            for (String providerName : providers.keySet())
            {
                try
                {
                    Provider provider = Provider.fromString(providerName);
                    JsonObject providerJson = providers.getAsJsonObject(providerName);
                    String envVar = providerJson.has("envVar") ? providerJson.get("envVar").getAsString() : null;
                    // If we already have this provider, merge the env var
                    ProviderConfig existing = configs.get(provider);
                    if (existing != null && envVar == null)
                    {
                        envVar = existing.getEnvVar();
                    }
                    // Load tiers
                    Map<ModelTier, String> tiers = new EnumMap<>(ModelTier.class);
                    // Keep existing tiers if any
                    if (existing != null)
                    {
                        tiers.putAll(existing.getTiers());
                    }
                    // Merge new tiers
                    if (providerJson.has("tiers"))
                    {
                        JsonObject tiersJson = providerJson.getAsJsonObject("tiers");
                        for (String tierName : tiersJson.keySet())
                        {
                            try
                            {
                                ModelTier modelTier = ModelTier.fromString(tierName);
                                tiers.put(modelTier, tiersJson.get(tierName).getAsString());
                            }
                            catch (IllegalArgumentException e)
                            {
                                System.err.println(
                                        "Warning: Unknown tier '" + tierName + "' for provider " + providerName);
                            }
                        }
                    }
                    // Load max output tokens (optional per-provider override)
                    Integer maxOutputTokens = null;
                    if (providerJson.has("maxOutputTokens"))
                    {
                        maxOutputTokens = providerJson.get("maxOutputTokens").getAsInt();
                    }
                    // Preserve existing maxOutputTokens if merging and not overridden
                    if (existing != null && maxOutputTokens == null)
                    {
                        maxOutputTokens = existing.getMaxOutputTokens();
                    }
                    if (envVar != null && !tiers.isEmpty())
                    {
                        configs.put(provider, new ProviderConfig(envVar, tiers, maxOutputTokens));
                    }
                }
                catch (IllegalArgumentException e)
                {
                    System.err.println("Warning: Unknown provider in config: " + providerName);
                }
            }
        }
        return tier;
    }
    // ========== API Key Management ==========

    /**
     * Gets the API key for a provider. Checks programmatic overrides first, then environment.
     *
     * @param provider
     *            the provider to get the key for
     * @return the API key, or null if not configured
     */
    public String getApiKey(Provider provider)
    {
        // Check programmatic override first
        String override = keyOverrides.get(provider);
        if (override != null && !override.isBlank())
        {
            return override;
        }
        // Get env var name from config
        ProviderConfig config = providerConfigs.get(provider);
        if (config == null)
        {
            return null;
        }
        // Fall back to environment variable
        String envValue = System.getenv(config.getEnvVar());
        return (envValue != null && !envValue.isBlank()) ? envValue : null;
    }

    /**
     * Sets a programmatic override for an API key.
     *
     * @param provider
     *            the provider
     * @param apiKey
     *            the API key to use
     */
    public void setApiKey(Provider provider, String apiKey)
    {
        if (apiKey == null || apiKey.isBlank())
        {
            keyOverrides.remove(provider);
        }
        else
        {
            keyOverrides.put(provider, apiKey);
        }
    }

    /**
     * Checks if a provider is configured (has an API key or base URL set).
     *
     * @param provider
     *            the provider to check
     * @return true if the provider is configured
     */
    public boolean isProviderConfigured(Provider provider)
    {
        return getApiKey(provider) != null;
    }
    // ========== Provider Detection ==========

    /**
     * Detects the first available provider based on priority order.
     *
     * @return the first configured provider
     * @throws IllegalStateException
     *             if no provider is configured
     */
    public Provider detectProvider()
    {
        for (Provider provider : providerPriority)
        {
            if (isProviderConfigured(provider))
            {
                return provider;
            }
        }
        throw new IllegalStateException("No AI provider configured. Set one of: ANTHROPIC_API_KEY, OPENAI_API_KEY, "
                + "GOOGLE_AI_API_KEY, or OLLAMA_BASE_URL environment variable.");
    }

    /**
     * Detects the first available provider, returning empty if none configured.
     *
     * @return the first configured provider, or empty if none
     */
    public Optional<Provider> detectProviderOptional()
    {
        for (Provider provider : providerPriority)
        {
            if (isProviderConfigured(provider))
            {
                return Optional.of(provider);
            }
        }
        return Optional.empty();
    }

    /**
     * Gets all configured providers.
     *
     * @return list of configured providers in priority order
     */
    public List<Provider> getConfiguredProviders()
    {
        List<Provider> configured = new ArrayList<>();
        for (Provider provider : providerPriority)
        {
            if (isProviderConfigured(provider))
            {
                configured.add(provider);
            }
        }
        return configured;
    }

    /**
     * Gets the provider priority order.
     *
     * @return list of providers in priority order
     */
    public List<Provider> getProviderPriority()
    {
        return Collections.unmodifiableList(providerPriority);
    }
    // ========== Model Selection ==========

    /**
     * Gets the model for a provider and tier, with fallback logic.
     * Fallback order: requested tier → balanced → fast → best → any available.
     *
     * @param provider
     *            the provider
     * @param tier
     *            the desired tier
     * @return the model name
     * @throws IllegalStateException
     *             if no model is available for the provider
     */
    public String getModel(Provider provider, ModelTier tier)
    {
        ProviderConfig config = providerConfigs.get(provider);
        if (config == null)
        {
            throw new IllegalStateException("No configuration for provider: " + provider);
        }
        // Try requested tier first
        String model = config.getModel(tier);
        if (model != null)
        {
            return model;
        }
        // Fallback order: balanced → fast → best
        ModelTier[] fallbackOrder =
        {ModelTier.BALANCED, ModelTier.FAST, ModelTier.BEST};
        for (ModelTier fallback : fallbackOrder)
        {
            if (fallback != tier)
            {
                model = config.getModel(fallback);
                if (model != null)
                {
                    return model;
                }
            }
        }
        // Last resort: first available tier
        if (!config.getTiers().isEmpty())
        {
            return config.getTiers().values().iterator().next();
        }
        throw new IllegalStateException("No models configured for provider: " + provider);
    }

    /**
     * Gets the default model for a provider using the default tier.
     * If a global default model override is set, it takes precedence.
     *
     * @param provider
     *            the provider
     * @return the default model name
     */
    public String getDefaultModel(Provider provider)
    {
        if (defaultModelOverride != null)
        {
            return defaultModelOverride;
        }
        return getModel(provider, defaultTier);
    }

    /**
     * Gets the default model for the first available provider.
     *
     * @return the default model name
     * @throws IllegalStateException
     *             if no provider is configured
     */
    public String getDefaultModel()
    {
        return getDefaultModel(detectProvider());
    }

    /**
     * Sets a global default model override.
     * When set, this model is used regardless of tier settings.
     *
     * @param model
     *            the model name to use as default, or null to clear
     */
    public void setDefaultModel(String model)
    {
        this.defaultModelOverride = model;
    }

    /**
     * Gets the current default tier.
     *
     * @return the default model tier
     */
    public ModelTier getDefaultTier()
    {
        return defaultTier;
    }

    /**
     * Sets the default model tier.
     *
     * @param tier
     *            the tier to use by default
     */
    public void setDefaultTier(ModelTier tier)
    {
        if (tier == null)
        {
            throw new IllegalArgumentException("Tier cannot be null");
        }
        this.defaultTier = tier;
    }

    /**
     * Gets the provider configuration.
     *
     * @param provider
     *            the provider
     * @return the provider configuration, or null if not configured
     */
    public ProviderConfig getProviderConfig(Provider provider)
    {
        return providerConfigs.get(provider);
    }
    // ========== Temperature ==========

    /**
     * Gets the default temperature for AI requests.
     *
     * @return the default temperature, or null if not set
     */
    public Double getDefaultTemperature()
    {
        return defaultTemperature;
    }

    /**
     * Sets the default temperature for AI requests.
     *
     * @param temperature
     *            the temperature (0.0-2.0), or null to clear
     */
    public void setDefaultTemperature(Double temperature)
    {
        this.defaultTemperature = temperature;
    }
    // ========== Max Output Tokens ==========

    /**
     * Gets the max output tokens for a provider.
     * Priority: provider override → global default.
     *
     * @param provider
     *            the provider
     * @return the max output tokens to use
     */
    public Integer getMaxOutputTokens(Provider provider)
    {
        ProviderConfig config = providerConfigs.get(provider);
        if (config != null && config.getMaxOutputTokens() != null)
        {
            return config.getMaxOutputTokens();
        }
        return defaultMaxOutputTokens;
    }

    /**
     * Gets the global default max output tokens.
     *
     * @return the default max output tokens
     */
    public Integer getDefaultMaxOutputTokens()
    {
        return defaultMaxOutputTokens;
    }

    /**
     * Sets the global default max output tokens.
     *
     * @param maxOutputTokens
     *            the max output tokens, or null to reset to default (16384)
     */
    public void setDefaultMaxOutputTokens(Integer maxOutputTokens)
    {
        this.defaultMaxOutputTokens = maxOutputTokens != null ? maxOutputTokens : 16384;
    }
    // ========== Introspection ==========

    /**
     * Returns the current configuration as a map (for ai-config primitive).
     *
     * @return configuration map with current settings
     */
    public Map<String, Object> toMap()
    {
        Map<String, Object> config = new LinkedHashMap<>();
        // Provider status
        List<String> configuredProviders = new ArrayList<>();
        for (Provider p : getConfiguredProviders())
        {
            configuredProviders.add(p.name().toLowerCase());
        }
        config.put("configured-providers", configuredProviders);
        // Provider priority
        List<String> priorityList = new ArrayList<>();
        for (Provider p : providerPriority)
        {
            priorityList.add(p.name().toLowerCase());
        }
        config.put("provider-priority", priorityList);
        // Default tier
        config.put("default-tier", defaultTier.getJsonName());
        // Available tiers
        List<String> availableTiers = new ArrayList<>();
        for (ModelTier t : ModelTier.values())
        {
            availableTiers.add(t.getJsonName());
        }
        config.put("available-tiers", availableTiers);
        // Default model
        Optional<Provider> detected = detectProviderOptional();
        if (detected.isPresent())
        {
            config.put("default-provider", detected.get().name().toLowerCase());
            config.put("default-model", getDefaultModel(detected.get()));
        }
        // Overrides
        if (defaultModelOverride != null)
        {
            config.put("default-model-override", defaultModelOverride);
        }
        if (defaultTemperature != null)
        {
            config.put("default-temperature", defaultTemperature);
        }
        // Sources loaded (for debugging)
        config.put("config-sources", loadedSources);
        return config;
    }

    /**
     * Returns all model configurations as a nested map (for ai-models primitive).
     *
     * @return map of provider name to tier-model mappings
     */
    public Map<String, Map<String, String>> getModelsMap()
    {
        Map<String, Map<String, String>> result = new LinkedHashMap<>();
        for (Map.Entry<Provider, ProviderConfig> entry : providerConfigs.entrySet())
        {
            Map<String, String> tierMap = new LinkedHashMap<>();
            for (Map.Entry<ModelTier, String> tierEntry : entry.getValue().getTiers().entrySet())
            {
                tierMap.put(tierEntry.getKey().getJsonName(), tierEntry.getValue());
            }
            result.put(entry.getKey().name().toLowerCase(), tierMap);
        }
        return result;
    }

    /**
     * Gets the sources from which configuration was loaded.
     *
     * @return list of source descriptions
     */
    public List<String> getLoadedSources()
    {
        return Collections.unmodifiableList(loadedSources);
    }

    /**
     * Resets all programmatic overrides and reloads configuration from files.
     */
    public void reset()
    {
        keyOverrides.clear();
        defaultModelOverride = null;
        defaultTemperature = null;
        loadConfiguration();
    }
}
