# JLLL Cookbook

Common patterns and recipes for JLLL development.

## Building Multiline Strings

JLLL strings don't support multiline literals. Use `string-append` to build longer content:

```lisp
(define html (string-append
  "<!DOCTYPE html>"
  "<html><head><title>My App</title></head>"
  "<body>"
  "<h1>Hello World</h1>"
  "</body></html>"))
```

For lists of strings, use `string-join`:

```lisp
(define lines '("Line 1" "Line 2" "Line 3"))
(string-join lines "\n")  ; => "Line 1\nLine 2\nLine 3"
```

## Working with Temp Files

Get the system temp directory via Java interop:

```lisp
(define temp-dir (invoke-static 'java.lang.System "getProperty" "java.io.tmpdir"))
; => "/var/folders/.../T/" (macOS) or "/tmp" (Linux) or "C:\Users\...\Temp" (Windows)

(define file-path (string-append temp-dir "/myfile.txt"))
(spit file-path "content")
```

## Static Web UI Pattern

Generate HTML files and open them in the browser:

```lisp
; 1. Build HTML content
(define html-content (string-append
  "<!DOCTYPE html>"
  "<html><head>"
  "<style>body { font-family: sans-serif; padding: 2rem; }</style>"
  "</head><body>"
  "<h1>My App</h1>"
  "<p>Hello from JLLL!</p>"
  "</body></html>"))

; 2. Write to temp file
(define temp-dir (invoke-static 'java.lang.System "getProperty" "java.io.tmpdir"))
(define file-path (string-append temp-dir "/myapp.html"))
(spit file-path html-content)

; 3. Open in browser
(open-browser (string-append "file://" file-path))
```

## File I/O

### Reading files

```lisp
(define content (slurp "path/to/file.txt"))     ; Read entire file
(define data (json-read-file "config.json"))    ; Read and parse JSON
```

### Writing files

```lisp
(spit "output.txt" "content")                   ; Write/overwrite file
(spit "log.txt" "new line\n" :append true)      ; Append to file
(json-write-file "data.json" my-hash-map)       ; Write as JSON
```

### File operations

```lisp
(file-exists? "path/to/file")                   ; Check existence
(directory? "path/to/dir")                      ; Check if directory
(directory-list ".")                            ; List directory contents
```

## Working with JSON

```lisp
; Parse JSON string to JLLL data
(define data (json-parse "{\"name\": \"Alice\", \"age\": 30}"))
(hash-ref data :name)  ; => "Alice"

; Convert JLLL data to JSON string
(define json-str (json-stringify (hash-map :name "Bob" :age 25)))
; => "{\"name\":\"Bob\",\"age\":25}"

; File I/O with JSON
(json-write-file "users.json" (list user1 user2))
(define users (json-read-file "users.json"))
```

## Hash Maps

```lisp
; Create
(define h (hash-map :name "Alice" :age 30))

; Read
(hash-ref h :name)                ; => "Alice"
(hash-ref h :missing "default")   ; => "default"
(hash-keys h)                     ; => (:name :age)
(hash-values h)                   ; => ("Alice" 30)

; Update (mutates in place)
(hash-set! h :email "alice@example.com")
(hash-remove! h :age)

; Merge (returns new hash)
(hash-merge h (hash-map :city "NYC"))
```

## Date and Time

```lisp
; Current time
(now)                             ; Milliseconds since epoch
(current-time)                    ; Hash-map with components

; Create dates
(make-date 2024 3 15)             ; March 15, 2024
(make-date 2024 3 15 14 30 0)     ; 2:30 PM

; Extract components
(date-year ts)                    ; => 2024
(date-month ts)                   ; => 3
(date-day ts)                     ; => 15

; Format and parse
(date-format (now) "yyyy-MM-dd")  ; => "2024-03-15"
(date-parse "2024-03-15" "yyyy-MM-dd")

; Arithmetic
(date-add ts :days 7)             ; Add 7 days
(date-diff ts1 ts2 :hours)        ; Difference in hours
```
