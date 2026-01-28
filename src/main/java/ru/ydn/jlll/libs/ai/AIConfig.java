package ru.ydn.jlll.libs.ai;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Global configuration for AI services.
 *
 * <p>
 * Manages API keys, default models, and provider detection. API keys are read from
 * environment variables by default but can be overridden programmatically.
 * </p>
 *
 * <p>
 * Provider priority order for auto-detection:
 * </p>
 * <ol>
 * <li>OpenAI (OPENAI_API_KEY) - default model: gpt-4o-mini</li>
 * <li>Anthropic (ANTHROPIC_API_KEY) - default model: claude-3-haiku-20240307</li>
 * <li>Google AI (GOOGLE_AI_API_KEY) - default model: gemini-1.5-flash</li>
 * <li>Ollama (OLLAMA_BASE_URL) - default model: llama3.2</li>
 * </ol>
 */
public class AIConfig
{
    /** Singleton instance */
    private static final AIConfig INSTANCE = new AIConfig();

    /** Provider enumeration with env var names and default models */
    public enum Provider
    {
        OPENAI("OPENAI_API_KEY", "gpt-4o-mini"), ANTHROPIC("ANTHROPIC_API_KEY", "claude-3-haiku-20240307"), GOOGLE_AI(
                "GOOGLE_AI_API_KEY", "gemini-1.5-flash"), OLLAMA("OLLAMA_BASE_URL", "llama3.2");

        private final String envVar;
        private final String defaultModel;

        Provider(String envVar, String defaultModel)
        {
            this.envVar = envVar;
            this.defaultModel = defaultModel;
        }

        public String getEnvVar()
        {
            return envVar;
        }

        public String getDefaultModel()
        {
            return defaultModel;
        }
    }

    /** Provider priority order for auto-detection */
    private static final List<Provider> PROVIDER_PRIORITY = List.of(Provider.OPENAI, Provider.ANTHROPIC,
            Provider.GOOGLE_AI, Provider.OLLAMA);
    /** Programmatic overrides for API keys (takes precedence over env vars) */
    private final Map<Provider, String> keyOverrides = new ConcurrentHashMap<>();
    /** Default model override (if set, used instead of provider default) */
    private volatile String defaultModelOverride = null;
    /** Default temperature for AI requests */
    private volatile Double defaultTemperature = null;

    private AIConfig()
    {
        // Private constructor for singleton
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
        // Fall back to environment variable
        String envValue = System.getenv(provider.getEnvVar());
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

    /**
     * Detects the first available provider based on priority order.
     *
     * @return the first configured provider
     * @throws IllegalStateException
     *             if no provider is configured
     */
    public Provider detectProvider()
    {
        for (Provider provider : PROVIDER_PRIORITY)
        {
            if (isProviderConfigured(provider))
            {
                return provider;
            }
        }
        throw new IllegalStateException("No AI provider configured. Set one of: OPENAI_API_KEY, ANTHROPIC_API_KEY, "
                + "GOOGLE_AI_API_KEY, or OLLAMA_BASE_URL environment variable.");
    }

    /**
     * Detects the first available provider, returning empty if none configured.
     *
     * @return the first configured provider, or empty if none
     */
    public Optional<Provider> detectProviderOptional()
    {
        for (Provider provider : PROVIDER_PRIORITY)
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
        for (Provider provider : PROVIDER_PRIORITY)
        {
            if (isProviderConfigured(provider))
            {
                configured.add(provider);
            }
        }
        return configured;
    }

    /**
     * Gets the default model for a provider.
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
        return provider.getDefaultModel();
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
     *
     * @param model
     *            the model name to use as default, or null to clear
     */
    public void setDefaultModel(String model)
    {
        this.defaultModelOverride = model;
    }

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
        return config;
    }

    /**
     * Resets all programmatic overrides.
     */
    public void reset()
    {
        keyOverrides.clear();
        defaultModelOverride = null;
        defaultTemperature = null;
    }
}
