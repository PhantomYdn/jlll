# Shell Execution

JLLL provides shell command execution through the `bash` function, allowing you to run system commands and capture their output.

## Basic Usage

The `bash` function executes a shell command and returns a structured result:

```lisp
(bash "echo hello")
;; => {:stdout "hello\n" :stderr "" :exit-code 0}

(bash "ls -la")
;; => {:stdout "total 48\ndrwxr-xr-x..." :stderr "" :exit-code 0}
```

## Return Value

The function returns a hash-map with three keys:

| Key | Type | Description |
|-----|------|-------------|
| `:stdout` | String | Standard output (may be empty) |
| `:stderr` | String | Standard error (may be empty) |
| `:exit-code` | Integer | Exit code (0 = success) |

## Options

### Timeout

Set a maximum execution time in milliseconds (default: 120000 = 2 minutes):

```lisp
;; Short timeout for quick commands
(bash "ping -c 1 localhost" :timeout 5000)

;; Long timeout for builds
(bash "npm install" :timeout 300000)
```

When a command times out:
- The process is forcibly terminated
- Exit code is set to -1
- stderr contains a timeout message

### Working Directory

Execute in a specific directory:

```lisp
(bash "npm test" :cwd "/path/to/project")

;; Tilde expansion is supported
(bash "ls" :cwd "~/projects")
```

### Standard Input

Pipe data to the command's stdin:

```lisp
;; String input
(bash "wc -l" :input "line1\nline2\nline3")
;; => {:stdout "       3" :stderr "" :exit-code 0}

;; Input from a port
(let ((port (open-input-file "data.txt")))
  (let ((result (bash "sort" :input port)))
    (close-input-port port)
    result))
```

### Environment Variables

Add environment variables for the command:

```lisp
(bash "echo $MY_VAR" :env (hash-map "MY_VAR" "hello"))
;; => {:stdout "hello\n" :stderr "" :exit-code 0}

;; Keywords are converted to string keys
(bash "echo $FOO" :env (hash-map :FOO "bar"))
;; => {:stdout "bar\n" :stderr "" :exit-code 0}
```

## Convenience Functions

### shell

Alias for `bash`:

```lisp
(shell "echo test")
;; Same as (bash "echo test")
```

### shell-stdout

Returns only the stdout content:

```lisp
(shell-stdout "echo hello")
;; => "hello\n"

(shell-stdout "pwd" :cwd "~")
;; => "/Users/yourname\n"
```

### shell-ok?

Returns true if the command succeeded (exit code 0):

```lisp
(shell-ok? "test -f file.txt")
;; => true if file exists

(if (shell-ok? "git status")
    (println "Git repository")
    (println "Not a git repository"))
```

## Combined Options

Multiple options can be combined:

```lisp
(bash "npm test" 
  :cwd "/path/to/project"
  :timeout 60000
  :env (hash-map "NODE_ENV" "test"))
```

## Piping and Complex Commands

Shell syntax like pipes, redirections, and command chaining work:

```lisp
;; Pipe
(bash "cat file.txt | grep pattern | wc -l")

;; Command chaining
(bash "mkdir -p dir && cd dir && touch file.txt")

;; Redirect stderr to stdout
(bash "command 2>&1")
```

## Error Handling

Check exit codes to handle failures:

```lisp
(let ((result (bash "git commit -m 'message'")))
  (if (zero? (hash-ref result :exit-code))
      (println "Commit successful")
      (println "Commit failed:" (hash-ref result :stderr))))
```

Or use exception handling:

```lisp
(define (bash-checked command . options)
  (let ((result (apply bash (cons command options))))
    (if (not (zero? (hash-ref result :exit-code)))
        (error "Command failed: " command "\n" (hash-ref result :stderr))
        (hash-ref result :stdout))))

(bash-checked "git status")
```

## AI Tool Integration

The `bash` function is automatically available as an AI tool in AI sessions:

```lisp
;; AI can execute shell commands
(ai "List all Java files in the src directory")
;; AI uses: (bash "find src -name '*.java'")

(ai "What's the current git branch?")
;; AI uses: (bash "git rev-parse --abbrev-ref HEAD")
```

## Platform Notes

- **Unix/Linux/macOS:** Commands run via `/bin/sh -c`
- **Windows:** Commands run via `cmd /c`

The shell interprets the command string, so shell features like globbing, variable expansion, and pipes work as expected.

## Security Considerations

The `bash` function can execute arbitrary commands with the same permissions as the JLLL process. Be careful with:

- User-provided input in commands (injection risk)
- Commands that modify the filesystem
- Commands with network access
- Exposing `bash` to untrusted code

When using with AI sessions, the AI can execute any command. Consider:
- Running in sandboxed environments
- Limiting file system access
- Using network isolation
- Reviewing AI-generated commands before execution

## Future Enhancements

Phase 2 will add interactive command handling with:
- Real-time stdout/stderr streaming
- Incremental input writing
- Process control (signals)
