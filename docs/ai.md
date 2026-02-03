# AI Library

LLM integration using LangChain4j. Provides session-based AI interactions with conversation memory, streaming responses, and tool calling.

## Configuration

API keys are read from environment variables:

| Environment Variable | Provider |
|---------------------|----------|
| `ANTHROPIC_API_KEY` | Anthropic (Claude) - **default provider** |
| `OPENAI_API_KEY` | OpenAI (GPT-4, GPT-4o, etc.) |
| `GOOGLE_AI_API_KEY` | Google AI (Gemini) |
| `OLLAMA_BASE_URL` | Ollama (local models) |

Keys can also be set programmatically:

```lisp
(ai-configure :anthropic-api-key "sk-ant-...")
(ai-configure :openai-api-key "sk-...")
(ai-configure :google-ai-api-key "...")
(ai-configure :ollama-base-url "http://localhost:11434")
```

## Model Tiers

Models are organized into capability tiers:

| Tier | Description | Anthropic | OpenAI | Google |
|------|-------------|-----------|--------|--------|
| `best` | Most capable, highest cost | claude-opus-4-5 | o1 | gemini-1.5-pro |
| `balanced` | Good balance (default) | claude-sonnet-4-5 | gpt-4o | gemini-1.5-pro |
| `fast` | Fastest, lowest cost | claude-haiku-4-5 | gpt-4o-mini | gemini-1.5-flash |

Use tiers for semantic model selection:

```lisp
(ai-session-create :tier "best")      ; Most capable
(ai-session-create :tier "fast")      ; Quick responses
(ai-session-create :model "gpt-4o")   ; Explicit model override
```

### Custom Model Configuration

Override defaults via `models.json`:

**Override Priority (highest to lowest):**
1. `JLLL_AI_MODELS_PATH` environment variable
2. `./models.json` (project directory)
3. `~/.jlll/models.json` (user home)
4. Bundled resource (default)

Example `~/.jlll/models.json`:
```json
{
  "defaultTier": "fast",
  "providers": {
    "anthropic": {
      "tiers": {
        "balanced": "claude-sonnet-4-0"
      }
    }
  }
}
```

## Session Management

Sessions encapsulate conversation history, configuration, and tools.

### Creating Sessions

```lisp
;; Basic session (uses default settings)
(ai-session-create)

;; Named session with options
(define coder (ai-session-create 
  :name "coding-helper"
  :system "You are a coding assistant. Be concise."
  :tier "balanced"
  :tools (list my-tool)))

;; Without eval tool (AI cannot execute code)
(ai-session-create :eval false)
```

**Session Options:**

| Option | Description |
|--------|-------------|
| `:name "name"` | Session name (auto-generated if omitted) |
| `:system "prompt"` | Custom system message |
| `:tier "balanced"` | Model tier: "best", "balanced", "fast" |
| `:model "gpt-4o"` | Explicit model override |
| `:tools (list ...)` | Additional custom tools |
| `:eval true/false` | Enable/disable eval tool (default: true) |
| `:auto-save "path"` | Auto-save after each interaction |

### Activating Sessions

```lisp
;; Make session active for current environment
(ai-session-activate coder)

;; Get current active session
(ai-session-current)  ; => session or nil

;; List all sessions
(ai-sessions)  ; => list of sessions

;; Deactivate
(ai-session-deactivate)
```

### Session Identity

```lisp
(ai-session-name coder)   ; => "coding-helper"
(ai-session-id coder)     ; => "sess-abc123"
(ai-session? coder)       ; => true
```

## Chatting with AI

### Console-Friendly: `ai`

The `ai` function streams output to the console and returns the full response:

```lisp
(ai "Explain closures in 2 sentences")
;; Output streams to console as it arrives...
;; => "A closure is a function... They enable..."
```

### Programmatic: `ai-prompt`

Returns a lazy sequence of text chunks:

```lisp
(define response (ai-prompt "Hello"))
(realize response)  ; => ("Hello" "!" " How" " can" " I" " help" "?")
(string-join (realize response) "")  ; => "Hello! How can I help?"

;; Custom streaming
(for-each print (ai-prompt "Count to 3"))
```

### Options

Both `ai` and `ai-prompt` accept options:

```lisp
(ai "Be creative" :temperature 0.9)
(ai "Use specific model" :model "claude-sonnet-4-5")
(ai "Use different session" :session other-session)
```

## Conversation History

```lisp
;; Get history (list of hash-maps with :role and :content)
(ai-history)
(ai-history coder)  ; specific session

;; Clear history (keeps session config)
(ai-clear)
(ai-clear coder)
```

## Tools

Tools allow the LLM to call JLLL functions.

### Built-in Eval Tool

By default, sessions include an `eval` tool:

```lisp
(ai "What is 123 * 456?")
;; AI uses eval tool: (eval "(* 123 456)")
;; => "123 * 456 = 56088"
```

Disable with `:eval false`:

```lisp
(ai-session-create :eval false)
```

### Custom Tools

```lisp
(define weather-tool
  (ai-tool "get-weather"
    :description "Get current weather for a city"
    :parameters '((city "string" "City name"))
    :fn (lambda (city) 
          (hash-map :city city :temp 72 :conditions "sunny"))))

;; Add to session
(ai-tool-add weather-tool)

;; Now AI can check weather
(ai "What's the weather in Paris?")
```

### Tool Management

```lisp
(ai-tools)                    ; List tools in current session
(ai-tools coder)              ; List tools in specific session
(ai-tool-remove "get-weather") ; Remove by name
(ai-tool? weather-tool)       ; => true
```

### Tool Tracing

Debug what tools the AI is using:

```lisp
(ai-session-trace-tools coder true)
(ai "Calculate 2 + 2")
;; TOOL CALL: eval
;; Arguments: {"code": "(+ 2 2)"}
;; Result: 4

(ai-session-trace-tools coder false)  ; Disable
```

## Session Persistence

Save and restore conversations.

### Saving Sessions

```lisp
;; Save session to file
(ai-session-save coder "sessions/coder.json")

;; Pretty-printed JSON
(ai-session-save coder "sessions/coder.json" :pretty true)

;; Save current active session
(ai-session-save "current.json")
```

**What's saved:**
- Session name and ID
- System prompt
- Model/tier configuration
- Conversation history
- Custom tools (with procedure source)
- Auto-save path

### Loading Sessions

```lisp
;; Load session
(define restored (ai-session-load "sessions/coder.json"))

;; Load and activate immediately
(ai-session-load "sessions/coder.json" :activate true)

;; Override name on load
(ai-session-load "sessions/coder.json" :name "restored-coder")

;; Load without eval tool
(ai-session-load "sessions/coder.json" :eval false)
```

### Auto-Save

Sessions can auto-save after each interaction:

```lisp
;; Enable at creation
(ai-session-create :name "coder" :auto-save ".jlll/coder.json")

;; Enable on existing session
(ai-session-auto-save ".jlll/session.json")

;; Disable
(ai-session-auto-save false)

;; Query current path
(ai-session-auto-save)  ; => ".jlll/session.json" or false
```

## Complete Example

```lisp
;; Configure (if not using environment variables)
(ai-configure :anthropic-api-key "sk-ant-...")

;; Create a coding assistant
(define coder (ai-session-create 
  :name "my-coder"
  :system "You are a helpful coding assistant. Be concise. Use eval to verify code."
  :tier "balanced"
  :auto-save ".jlll/coder-session.json"))

(ai-session-activate coder)

;; Chat
(ai "Write a function to reverse a list")

;; Add a custom tool
(define file-tool
  (ai-tool "read-file"
    :description "Read contents of a file"
    :parameters '((path "string" "File path"))
    :fn slurp))
(ai-tool-add file-tool)

(ai "What's in my package.json?")

;; Check history
(ai-history)

;; Later, restore the session
(ai-session-load ".jlll/coder-session.json" :activate true)
(ai "Continue from where we left off")
```

## Best Practices

1. **Use tiers over explicit models** - Tiers allow updating models without code changes

2. **Prefer JLLL primitives** - Guide AI to use `slurp`/`spit` over shell commands

3. **Enable auto-save for important sessions** - Prevents loss of conversation context

4. **Use custom tools for domain operations** - More reliable than having AI construct shell commands

5. **Disable eval for untrusted contexts** - Use `:eval false` when AI shouldn't execute arbitrary code

## Primitives Reference

| Primitive | Description |
|-----------|-------------|
| **Session Management** | |
| `ai-session-create` | Create new session |
| `ai-session-activate` | Set as current session |
| `ai-session-deactivate` | Clear current session |
| `ai-session-current` | Get current session |
| `ai-sessions` | List all sessions |
| `ai-session-name` | Get session name |
| `ai-session-id` | Get session ID |
| `ai-session?` | Test if value is session |
| **Persistence** | |
| `ai-session-save` | Save session to file |
| `ai-session-load` | Load session from file |
| `ai-session-restore` | Load and activate (convenience) |
| `ai-session-auto-save` | Enable/disable/query auto-save |
| **Core Operations** | |
| `ai` | Chat (console streaming, returns string) |
| `ai-prompt` | Chat (returns lazy sequence) |
| `ai-history` | Get conversation history |
| `ai-clear` | Clear history |
| **Tools** | |
| `ai-tool` | Create custom tool |
| `ai-tool-add` | Add tool to session |
| `ai-tool-remove` | Remove tool by name |
| `ai-tools` | List session tools |
| `ai-tool?` | Test if value is tool |
| `ai-session-trace-tools` | Enable/disable tool tracing |
| **Configuration** | |
| `ai-configure` | Set config options |
| `ai-config` | Get current config |
| `ai-models` | Get model configurations |
