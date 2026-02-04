# Web Console

JLLL includes a browser-based REPL (Read-Eval-Print Loop) that provides an interactive development environment accessible from any web browser.

## Quick Start

### From Command Line

```bash
# Start web console on default port (8080)
java -jar jlll-cli.jar web

# Custom port
java -jar jlll-cli.jar web --port 3000

# Allow external connections (use with caution!)
java -jar jlll-cli.jar web --bind 0.0.0.0

# Isolated environments per connection
java -jar jlll-cli.jar web --isolated
```

### From JLLL

```lisp
;; Start web console
(web-console)                    ; default port 8080

;; Custom port
(web-console :port 3000)

;; Allow external connections
(web-console :bind "0.0.0.0")

;; Get status
(web-console :status true)       ; => {:running true :port 8080 :url "..."}

;; Stop web console
(web-console :stop true)
```

## Features

- **Syntax Highlighting**: CodeMirror-based editor with Lisp mode
- **Auto-Complete**: Tab completion for symbols from current environment
- **Command History**: Up/Down arrows to navigate previous commands
- **Multi-line Input**: Shift+Enter for continuation, Enter when expression is complete
- **Streaming Output**: Server-Sent Events (SSE) for real-time output
- **Mobile-Friendly**: Responsive design for tablets and phones

## CLI Options

| Option | Default | Description |
|--------|---------|-------------|
| `--port`, `-p` | 8080 | Port to listen on |
| `--bind`, `-b` | 127.0.0.1 | Host/interface to bind to |
| `--isolated` | false | Each connection gets isolated environment |
| `--rc FILE` | ~/.jlllrc | Custom init file |
| `--no-rc` | false | Skip loading init file |

## Security

By default, the web console binds to `127.0.0.1` (localhost only), making it inaccessible from other machines.

**Warning**: Using `--bind 0.0.0.0` or `:bind "0.0.0.0"` allows anyone with network access to execute arbitrary code. Only use this:

- On trusted networks
- Behind a firewall
- For short demonstrations

When external binding is enabled, a security warning is displayed:

```
WARNING: Web console bound to 0.0.0.0
  Anyone with network access can execute code!
  Use only in trusted networks or behind a firewall.
```

## Environment Modes

### Shared Environment (Default)

All browser connections share the same JLLL environment. Definitions made by one user are visible to all users.

```bash
java -jar jlll-cli.jar web
```

### Isolated Environments

Each browser connection gets its own child environment. Definitions are private to that session, but all inherit from the parent environment (including loaded libraries).

```bash
java -jar jlll-cli.jar web --isolated
```

```lisp
(web-console :isolated true)
```

## Using the Web Console

1. Open your browser to `http://localhost:8080` (or your configured port)
2. Type JLLL expressions in the input area
3. Press **Enter** to evaluate (when the expression is complete)
4. Press **Shift+Enter** for multi-line input
5. Press **Tab** for auto-completion
6. Use **Up/Down** arrows to navigate command history

### Example Session

```
JLLL Web Console

> (+ 1 2 3)
6

> (define (square x) (* x x))
square

> (map square (range 5))
(0 1 4 9 16)

> (println "Hello, World!")
Hello, World!
null
```

## API Endpoints

The web console exposes these HTTP endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | Web UI (HTML page) |
| `/eval?code=...` | GET (SSE) | Evaluate code, returns event stream |
| `/complete?prefix=...` | GET | Auto-complete suggestions |
| `/status` | GET | Server status |
| `/interrupt` | POST | Interrupt running evaluation |

### SSE Event Types

The `/eval` endpoint returns a Server-Sent Events stream with these event types:

| Event | Data | Description |
|-------|------|-------------|
| `output` | `{"text": "..."}` | Printed output (from `println`, etc.) |
| `result` | `{"value": "...", "type": "..."}` | Evaluation result |
| `error` | `{"message": "...", "type": "..."}` | Error message |
| `done` | `{}` | Stream complete |

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| Enter | Evaluate (if expression complete) |
| Shift+Enter | Insert newline |
| Tab | Auto-complete |
| Up Arrow | Previous history |
| Down Arrow | Next history |
| Escape | Close autocomplete popup |

## JLLL Functions

### web-console

Start or control the browser-based REPL.

```lisp
;; Start with defaults
(web-console)

;; Start with options
(web-console :port 3000 :bind "127.0.0.1" :isolated true)

;; Get status
(web-console :status true)
;; => {:running true :port 3000 :host "127.0.0.1" :isolated true :url "http://localhost:3000"}

;; Stop
(web-console :stop true)
;; => {:running false :message "Web console stopped"}
```

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `:port` | Integer | 8080 | Port to listen on |
| `:bind` | String | "127.0.0.1" | Host/interface to bind to |
| `:isolated` | Boolean | false | Isolated environments per connection |

### web-console-running?

Test if the web console server is currently running.

```lisp
(web-console-running?)  ; => true or false
```

## Troubleshooting

### Port Already in Use

If port 8080 is already in use, specify a different port:

```bash
java -jar jlll-cli.jar web --port 3000
```

### Connection Refused

Make sure the web console is running and you're using the correct port:

```lisp
(web-console :status true)  ; Check if running and what port
```

### No Syntax Highlighting

CodeMirror is bundled with the web console. If syntax highlighting doesn't appear, check the browser's developer console for JavaScript errors.

## Future Enhancements

- **Authentication**: Token or password protection
- **Multiple Sessions**: Support concurrent users with isolated environments
- **Notebook Mode**: Save/load evaluation history as notebooks
- **File Browser**: Navigate and edit files in the project
