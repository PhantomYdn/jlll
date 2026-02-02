# Web Server

JLLL includes an embedded HTTP server powered by [Javalin](https://javalin.io/). This enables building REST APIs, serving static files, and handling real-time events with Server-Sent Events (SSE).

## Quick Start

```lisp
;; Create server with port configuration
(define server (http-server :port 8080))

;; Define a simple route
(http-get server "/hello"
  (lambda (ctx) (http-result ctx "Hello, World!")))

;; Start the server
(http-start server)

;; Server now running at http://localhost:8080/hello
```

## Server Lifecycle

### Creating a Server

```lisp
(http-server)                           ; Defaults: port 8080, host 0.0.0.0
(http-server :port 3000)                ; Custom port
(http-server :host "127.0.0.1")         ; Localhost only
(http-server :port 3000 :host "127.0.0.1")  ; Both
(http-server :context-path "/api")      ; Base path for all routes
```

**Options:**
| Option | Default | Description |
|--------|---------|-------------|
| `:port` | 8080 | Port number to listen on |
| `:host` | "0.0.0.0" | Host/interface to bind to |
| `:context-path` | null | Base path prefix for all routes |

### Starting and Stopping

```lisp
(http-start server)                     ; Start with configured port/host
(http-start server :port 3000)          ; Override port for this start
(http-start server :port 3000 :host "127.0.0.1")  ; Override both
(http-stop server)                      ; Stop the server
(http-restart server)                   ; Stop and start (preserves routes)
```

**Start Options (override server configuration):**
| Option | Description |
|--------|-------------|
| `:port` | Override configured port |
| `:host` | Override configured host |

### Retry After Failed Start

If `http-start` fails (e.g., port already in use), you can retry with a different port:

```lisp
(define server (http-server :port 8080))
(http-get server "/hello" (lambda (ctx) (http-result ctx "Hello")))

;; First attempt fails - port 8080 is in use
(http-start server)  ; Error: Port already in use

;; Retry with different port - works! Routes are preserved.
(http-start server :port 8081)
```

### Predicates

```lisp
(http-server? obj)     ; Test if obj is a server instance
(http-running? obj)    ; Test if server is currently running
(http-context? obj)    ; Test if obj is a request context
(sse-client? obj)      ; Test if obj is an SSE client
```

## Route Registration

### HTTP Methods

```lisp
(http-get server "/path" handler)
(http-post server "/path" handler)
(http-put server "/path" handler)
(http-delete server "/path" handler)
(http-patch server "/path" handler)
(http-head server "/path" handler)
(http-options server "/path" handler)
```

### Path Parameters

Use `{name}` syntax for path parameters:

```lisp
(http-get server "/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (http-json ctx (hash-map :user-id id))))

;; GET /users/123 returns {"user-id":"123"}
```

### Wildcard Paths

```lisp
(http-get server "/files/*"
  (lambda (ctx)
    (http-result ctx (concat "Path: " (http-path ctx)))))

;; GET /files/docs/readme.txt returns "Path: /files/docs/readme.txt"
```

## Request Context

The handler receives a context object (`ctx`) that provides access to request data.

### Request Information

```lisp
(http-method ctx)              ; => :get, :post, :put, etc.
(http-path ctx)                ; => "/users/123"
(http-matched-path ctx)        ; => "/users/{id}" (route pattern)
(http-remote-addr ctx)         ; => "192.168.1.100"
```

### Path Parameters

```lisp
(http-path-param ctx "id")     ; Get path parameter by name
```

### Query Parameters

```lisp
(http-query-param ctx "q")           ; Get single query param (null if missing)
(http-query-params ctx)              ; Get all as hash-map

;; Convenience wrappers
(http-query-param? ctx "q")          ; Check if param exists
(http-query-param-or ctx "q" "default") ; With default value
```

Example:
```lisp
;; GET /search?q=lisp&limit=10
(http-get server "/search"
  (lambda (ctx)
    (define query (http-query-param ctx "q"))
    (define limit (http-query-param-or ctx "limit" "20"))
    (http-json ctx (hash-map :query query :limit limit))))
```

### Request Headers

```lisp
(http-header ctx "Authorization")    ; Get header by name (null if missing)
(http-headers ctx)                   ; Get all headers as hash-map
```

### Request Body

```lisp
(http-body ctx)                      ; Get body as string
(http-body-json ctx)                 ; Parse body as JSON (hash-map or list)
```

Example:
```lisp
(http-post server "/api/users"
  (lambda (ctx)
    (define data (http-body-json ctx))
    (define name (hash-ref data :name))
    (http-json ctx (hash-map :created name))))
```

## Response Helpers

### Setting Response Body

```lisp
(http-result ctx "plain text")       ; Set body as plain text
(http-json ctx data)                 ; Set JSON response with Content-Type
(http-html ctx "<h1>Hello</h1>")     ; Set HTML response with Content-Type
```

### Status Code

```lisp
(http-status ctx 201)                ; Set status code
(http-status ctx)                    ; Get current status code
```

### Response Headers

```lisp
(http-header! ctx "X-Custom" "value")
(http-content-type ctx "text/csv")
```

### Redirects

```lisp
(http-redirect ctx "/login")         ; 302 redirect
(http-redirect ctx "/new-location" 301) ; 301 permanent redirect
```

### Convenience Response Functions

```lisp
(respond-json ctx data)              ; JSON with 200
(respond-html ctx html)              ; HTML with 200
(respond-text ctx text)              ; Plain text with 200
(respond-error ctx 404 "Not Found")  ; JSON error response
(respond-not-found ctx)              ; 404 response
(respond-bad-request ctx "Invalid input") ; 400 response
(respond-server-error ctx "Oops")    ; 500 response
```

## Filters (Before/After)

### Before Filters

Run before route handlers:

```lisp
;; Global before filter (all routes)
(http-before server
  (lambda (ctx)
    (println "Request:" (http-method ctx) (http-path ctx))))

;; Path-specific before filter
(http-before server "/api/*"
  (lambda (ctx)
    (define auth (http-header ctx "Authorization"))
    (when (null? auth)
      (http-status ctx 401)
      (http-json ctx (hash-map :error "Unauthorized")))))
```

### After Filters

Run after route handlers:

```lisp
(http-after server
  (lambda (ctx)
    (http-header! ctx "X-Response-Time" "10ms")))
```

## Error Handling

### Status Code Handlers

Handle specific HTTP status codes:

```lisp
(http-error server 404
  (lambda (ctx)
    (http-json ctx (hash-map :error "Resource not found"))))

(http-error server 500
  (lambda (ctx)
    (http-json ctx (hash-map :error "Internal server error"))))
```

### Exception Handler

Catch all exceptions:

```lisp
(http-exception server
  (lambda (ctx message)
    (println "Error:" message)
    (http-status ctx 500)
    (http-json ctx (hash-map :error message))))
```

## Static Files

Serve static files from a directory:

```lisp
(http-static server "/assets" "./public/")

;; Now /assets/style.css serves ./public/style.css
```

The server automatically sets appropriate Content-Type headers based on file extension.

## Server-Sent Events (SSE)

SSE enables server-to-client streaming for real-time updates.

### Creating an SSE Endpoint

```lisp
(http-sse server "/events"
  (lambda (client)
    ;; Keep connection alive
    (sse-keep-alive client)
    
    ;; Send events
    (sse-send client "Connected!")
    (sse-send client "update" "New data available")
    (sse-send client "message" "Hello" "event-id-123")))
```

### SSE Functions

```lisp
(sse-send client data)                   ; Send simple message
(sse-send client event-name data)        ; Send named event
(sse-send client event-name data id)     ; Send with event ID
(sse-keep-alive client)                  ; Keep connection alive
(sse-close client)                       ; Close connection
(sse-client? obj)                        ; Test if SSE client
```

### Client-Side JavaScript

```javascript
const eventSource = new EventSource('/events');

eventSource.onmessage = (event) => {
    console.log('Message:', event.data);
};

eventSource.addEventListener('update', (event) => {
    console.log('Update:', event.data);
});
```

## Complete Example: REST API

```lisp
;; In-memory data store
(define users (atom (hash-map)))
(define next-id (atom 1))

;; Create server
(define server (http-server :port 3000))

;; List all users
(http-get server "/api/users"
  (lambda (ctx)
    (http-json ctx (hash-values (deref users)))))

;; Get user by ID
(http-get server "/api/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (define user (hash-ref (deref users) id null))
    (if (null? user)
        (respond-not-found ctx)
        (http-json ctx user))))

;; Create user
(http-post server "/api/users"
  (lambda (ctx)
    (define body (http-body-json ctx))
    (define id (to-string (deref next-id)))
    (swap! next-id (lambda (n) (+ n 1)))
    (define user (hash-merge body (hash-map :id id)))
    (swap! users (lambda (u) (hash-set! u id user) u))
    (http-status ctx 201)
    (http-json ctx user)))

;; Update user
(http-put server "/api/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (define body (http-body-json ctx))
    (define existing (hash-ref (deref users) id null))
    (if (null? existing)
        (respond-not-found ctx)
        (begin
          (define updated (hash-merge existing body))
          (swap! users (lambda (u) (hash-set! u id updated) u))
          (http-json ctx updated)))))

;; Delete user
(http-delete server "/api/users/{id}"
  (lambda (ctx)
    (define id (http-path-param ctx "id"))
    (swap! users (lambda (u) (hash-remove! u id) u))
    (http-status ctx 204)
    (http-result ctx "")))

;; Error handling
(http-error server 404
  (lambda (ctx)
    (http-json ctx (hash-map :error "Not Found"))))

;; Start server
(http-start server)
(println "API running at http://localhost:3000/api/users")
```

## Function Reference

### Server Lifecycle
| Function | Description |
|----------|-------------|
| `http-server` | Create server with options |
| `http-start` | Start server (can override port/host) |
| `http-stop` | Stop server |
| `http-restart` | Stop and restart server |
| `http-server?` | Test if server |
| `http-running?` | Test if server is running |

### Route Registration
| Function | Description |
|----------|-------------|
| `http-get` | Register GET handler |
| `http-post` | Register POST handler |
| `http-put` | Register PUT handler |
| `http-delete` | Register DELETE handler |
| `http-patch` | Register PATCH handler |
| `http-head` | Register HEAD handler |
| `http-options` | Register OPTIONS handler |

### Filters and Errors
| Function | Description |
|----------|-------------|
| `http-before` | Register before filter |
| `http-after` | Register after filter |
| `http-error` | Register error handler |
| `http-exception` | Register exception handler |

### Context Accessors
| Function | Description |
|----------|-------------|
| `http-method` | Get HTTP method as keyword |
| `http-path` | Get request path |
| `http-matched-path` | Get route pattern |
| `http-path-param` | Get path parameter |
| `http-query-param` | Get query parameter |
| `http-query-params` | Get all query params |
| `http-header` | Get request header |
| `http-headers` | Get all headers |
| `http-body` | Get body as string |
| `http-body-json` | Get body as JSON |
| `http-remote-addr` | Get client IP |

### Context Setters
| Function | Description |
|----------|-------------|
| `http-result` | Set response body |
| `http-json` | Set JSON response |
| `http-html` | Set HTML response |
| `http-status` | Set/get status code |
| `http-header!` | Set response header |
| `http-content-type` | Set content type |
| `http-redirect` | Redirect to URL |

### Static Files
| Function | Description |
|----------|-------------|
| `http-static` | Serve static files |

### SSE
| Function | Description |
|----------|-------------|
| `http-sse` | Create SSE endpoint |
| `sse-send` | Send SSE event |
| `sse-keep-alive` | Keep connection alive |
| `sse-close` | Close connection |
| `sse-client?` | Test if SSE client |
