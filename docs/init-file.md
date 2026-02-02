# Init File (~/.jlllrc)

JLLL automatically loads `~/.jlllrc` (if it exists) when starting the REPL.
This allows you to customize your environment with aliases, utility functions,
library imports, and preferences.

## When the Init File is Loaded

| Command | Init File Loaded? |
|---------|-------------------|
| `jlll` | Yes (if exists) |
| `jlll -i script.jlll` | Yes (after script, if exists) |
| `jlll script.jlll` | No |
| `jlll -e '(+ 1 2)'` | No |
| `jlll --no-rc` | No |
| `jlll --rc custom.jlll` | Yes (must exist) |

**Key points:**

- The init file is loaded in **REPL mode only** (not when running scripts)
- The `-i` (interactive) flag loads the init file after executing the script
- If the default `~/.jlllrc` doesn't exist, JLLL starts normally (no error)
- If `--rc` specifies a custom file, that file **must exist** (error if missing)

## CLI Options

### `--rc FILE`

Specify an alternate init file instead of `~/.jlllrc`:

```bash
jlll --rc ~/my-custom-init.jlll
jlll --rc /path/to/project/init.jlll
```

The `~` character is expanded to the user's home directory.

### `--no-rc`

Skip loading the init file entirely:

```bash
jlll --no-rc
```

This is useful for:
- Debugging init file issues
- Starting with a clean environment
- Testing behavior without customizations

## Example Init File

```lisp
;; ~/.jlllrc - JLLL initialization file

;; Load frequently used libraries
;; (require "~/jlll-libs/utils.jlll")

;; Custom utility functions
(define (reload-init)
  :doc "Reload the init file"
  (load "~/.jlllrc"))

;; Convenience aliases
(define (cls)
  :doc "Clear the screen"
  (print "\033[2J\033[H"))

;; Configure environment
;; (ai-configure :default-model "gpt-4o")

;; Print welcome message
(println "JLLL initialized!")
```

## Error Handling

If the init file contains errors, JLLL will:

1. Print a clear error message with the file path
2. Show the specific error and cause (if available)
3. Exit with code 1 (non-zero exit code)

Example error output:

```
Error in init file /home/user/.jlllrc: Symbol is unbound: undefined-function
Caused by: undefined-function
```

This "fail hard" behavior ensures you're immediately aware of configuration
problems rather than silently starting with a broken environment.

## Path Expansion

Both `--rc` and the default path support `~` expansion:

| Path | Expands To |
|------|------------|
| `~/.jlllrc` | `/home/user/.jlllrc` |
| `~/init.jlll` | `/home/user/init.jlll` |
| `/absolute/path` | `/absolute/path` (unchanged) |

## Tips

### Debugging Init File Issues

If your init file has problems, start without it and load manually:

```bash
jlll --no-rc
```

Then in the REPL:

```lisp
(load "~/.jlllrc")  ; See the error with full context
```

### Conditional Configuration

You can check for features before using them:

```lisp
;; Only configure AI if the library is available
(when (bound? 'ai-configure)
  (ai-configure :default-model "gpt-4o"))
```

### Project-Specific Init

Use `--rc` for project-specific initialization:

```bash
cd my-project
jlll --rc ./project-init.jlll
```
