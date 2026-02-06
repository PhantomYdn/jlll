/**
 * JLLL Web Console - Browser-based REPL
 */
(function() {
    'use strict';

    // DOM elements
    const output = document.getElementById('output');
    const editorTextarea = document.getElementById('editor');
    const statusEl = document.getElementById('status');
    const autocompleteEl = document.getElementById('autocomplete');

    // State
    let editor = null;
    let history = [];
    let historyIndex = -1;
    let currentInput = '';
    let isEvaluating = false;
    let eventSource = null;
    let currentOutputDiv = null; // For streaming output - reuse same div

    // Autocomplete state
    let autocompleteItems = [];
    let autocompleteIndex = -1;
    let autocompletePrefix = '';
    let autocompleteDebounce = null;

    // Symbol cache for syntax highlighting (matching JlllHighlighter.java behavior)
    let knownSymbols = { callables: new Set(), variables: new Set() };

    // ========================================
    // JLLL Syntax Highlighting
    // ========================================

    /**
     * Fetch symbols from server for syntax highlighting.
     * Called on init and after each evaluation.
     */
    async function fetchSymbols() {
        try {
            const response = await fetch('/symbols?type=all');
            const data = await response.json();
            knownSymbols.callables = new Set(data.callables || []);
            knownSymbols.variables = new Set(data.variables || []);
            // Refresh editor highlighting if CodeMirror is active
            if (editor) {
                editor.refresh();
            }
        } catch (e) {
            console.error('Failed to fetch symbols:', e);
        }
    }

    /**
     * Check if a character is a delimiter (ends a token).
     * Matches JlllHighlighter.isDelimiter()
     */
    function isDelimiter(c) {
        return c === '(' || c === ')' || c === '[' || c === ']' ||
               c === '\'' || c === '`' || c === ',' || c === '@' ||
               c === '"' || c === ';' || /\s/.test(c);
    }

    /**
     * Check if a string is a number.
     * Matches JlllHighlighter.isNumber()
     */
    function isNumber(token) {
        if (!token || token.length === 0) return false;
        return /^[+-]?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(token);
    }

    /**
     * Tokenize JLLL code into an array of {type, value} tokens.
     * Matches JlllHighlighter.java logic.
     */
    function tokenizeJlll(code) {
        const tokens = [];
        let i = 0;

        while (i < code.length) {
            const c = code[i];

            // Comment (starts with ;)
            if (c === ';') {
                let end = code.indexOf('\n', i);
                if (end === -1) end = code.length;
                tokens.push({ type: 'comment', value: code.substring(i, end) });
                i = end;
                continue;
            }

            // String literal
            if (c === '"') {
                let end = i + 1;
                while (end < code.length) {
                    const sc = code[end];
                    if (sc === '\\' && end + 1 < code.length) {
                        end += 2; // Skip escaped character
                        continue;
                    }
                    if (sc === '"') {
                        end++;
                        break;
                    }
                    end++;
                }
                tokens.push({ type: 'string', value: code.substring(i, end) });
                i = end;
                continue;
            }

            // Quote characters
            if (c === '\'' || c === '`') {
                tokens.push({ type: 'quote', value: c });
                i++;
                continue;
            }

            // Comma (unquote) - handle ,@
            if (c === ',') {
                if (i + 1 < code.length && code[i + 1] === '@') {
                    tokens.push({ type: 'quote', value: ',@' });
                    i += 2;
                } else {
                    tokens.push({ type: 'quote', value: ',' });
                    i++;
                }
                continue;
            }

            // Parentheses
            if (c === '(' || c === ')') {
                tokens.push({ type: 'paren', value: c });
                i++;
                continue;
            }

            // Brackets
            if (c === '[' || c === ']') {
                tokens.push({ type: 'bracket', value: c });
                i++;
                continue;
            }

            // Whitespace
            if (/\s/.test(c)) {
                let end = i + 1;
                while (end < code.length && /\s/.test(code[end])) {
                    end++;
                }
                tokens.push({ type: 'whitespace', value: code.substring(i, end) });
                i = end;
                continue;
            }

            // Symbol or number - read until delimiter
            let start = i;
            while (i < code.length && !isDelimiter(code[i])) {
                i++;
            }
            const token = code.substring(start, i);

            if (isNumber(token)) {
                tokens.push({ type: 'number', value: token });
            } else if (token.startsWith(':')) {
                tokens.push({ type: 'keyword', value: token });
            } else if (knownSymbols.callables.has(token)) {
                tokens.push({ type: 'callable', value: token });
            } else if (knownSymbols.variables.has(token)) {
                tokens.push({ type: 'variable', value: token });
            } else {
                tokens.push({ type: 'unbound', value: token });
            }
        }

        return tokens;
    }

    /**
     * Highlight JLLL code - returns HTML with span elements.
     * Used for history/output highlighting.
     */
    function highlightJlll(code) {
        const tokens = tokenizeJlll(code);
        return tokens.map(t => {
            if (t.type === 'whitespace') {
                return escapeHtml(t.value);
            }
            return `<span class="jlll-${t.type}">${escapeHtml(t.value)}</span>`;
        }).join('');
    }

    /**
     * Define custom CodeMirror mode for JLLL.
     * Uses knownSymbols for context-aware highlighting.
     */
    function defineJlllMode() {
        if (typeof CodeMirror === 'undefined') return;

        CodeMirror.defineMode('jlll', function() {
            return {
                startState: function() {
                    return {};
                },
                token: function(stream, state) {
                    // Comment
                    if (stream.match(/;.*/)) {
                        return 'comment';
                    }

                    // String
                    if (stream.peek() === '"') {
                        stream.next(); // consume opening quote
                        while (!stream.eol()) {
                            const c = stream.next();
                            if (c === '\\') {
                                stream.next(); // skip escaped char
                            } else if (c === '"') {
                                break;
                            }
                        }
                        return 'string';
                    }

                    // Quote characters
                    if (stream.eat('\'') || stream.eat('`')) {
                        return 'quote';
                    }

                    // Comma (unquote)
                    if (stream.eat(',')) {
                        stream.eat('@'); // optional @ for unquote-splicing
                        return 'quote';
                    }

                    // Parentheses
                    if (stream.eat('(') || stream.eat(')')) {
                        return 'paren';
                    }

                    // Brackets
                    if (stream.eat('[') || stream.eat(']')) {
                        return 'bracket';
                    }

                    // Whitespace
                    if (stream.eatSpace()) {
                        return null;
                    }

                    // Symbol or number
                    let word = '';
                    while (!stream.eol()) {
                        const c = stream.peek();
                        if (c === '(' || c === ')' || c === '[' || c === ']' ||
                            c === '\'' || c === '`' || c === ',' || c === '@' ||
                            c === '"' || c === ';' || /\s/.test(c)) {
                            break;
                        }
                        word += stream.next();
                    }

                    if (word.length === 0) {
                        stream.next(); // consume unknown char
                        return null;
                    }

                    // Classify the token
                    if (isNumber(word)) {
                        return 'number';
                    }
                    if (word.startsWith(':')) {
                        return 'keyword';
                    }
                    if (knownSymbols.callables.has(word)) {
                        return 'callable';
                    }
                    if (knownSymbols.variables.has(word)) {
                        return 'variable';
                    }
                    return 'unbound';
                }
            };
        });
    }

    // ========================================
    // Editor Initialization
    // ========================================

    // Initialize CodeMirror if available
    function initEditor() {
        // Define custom JLLL mode first
        defineJlllMode();

        if (typeof CodeMirror !== 'undefined') {
            editor = CodeMirror.fromTextArea(editorTextarea, {
                mode: 'jlll',
                theme: 'default',
                lineNumbers: false,
                lineWrapping: true,
                matchBrackets: true,
                autoCloseBrackets: true,
                extraKeys: {
                    'Enter': handleEnter,
                    'Shift-Enter': handleShiftEnter,
                    'Up': handleUp,
                    'Down': handleDown,
                    'Tab': handleTab,
                    'Escape': hideAutocomplete
                }
            });
            editor.setSize('100%', 'auto');
            editor.on('change', handleEditorChange);
            editor.on('cursorActivity', handleCursorActivity);
        } else {
            // Fallback to plain textarea
            editorTextarea.addEventListener('keydown', handleTextareaKeydown);
            editorTextarea.addEventListener('input', handleTextareaInput);
        }
    }

    // Handle editor content change
    function handleEditorChange() {
        adjustEditorHeight();
        // Auto-show completions after typing
        triggerAutocompleteDebounced();
    }

    // Handle cursor movement
    function handleCursorActivity() {
        // Hide autocomplete if cursor moved significantly
        if (autocompleteEl.classList.contains('hidden')) return;
        const cursor = editor.getCursor();
        const line = editor.getLine(cursor.line);
        const currentWord = getWordAtCursor(line, cursor.ch);
        if (!currentWord || !currentWord.startsWith(autocompletePrefix.substring(0, 1))) {
            hideAutocomplete();
        }
    }

    // Trigger autocomplete with debounce
    function triggerAutocompleteDebounced() {
        if (autocompleteDebounce) {
            clearTimeout(autocompleteDebounce);
        }
        autocompleteDebounce = setTimeout(() => {
            const cursor = editor ? editor.getCursor() : null;
            if (!cursor) return;
            const line = editor.getLine(cursor.line);
            const prefix = getWordAtCursor(line, cursor.ch);
            // Auto-show after 2+ characters
            if (prefix && prefix.length >= 2) {
                showAutocomplete(prefix);
            } else {
                hideAutocomplete();
            }
        }, 150);
    }

    // Adjust editor height based on content
    function adjustEditorHeight() {
        if (editor) {
            const lines = editor.lineCount();
            const height = Math.max(24, Math.min(lines * 20, 200));
            editor.setSize('100%', height + 'px');
        }
    }

    // Get current input
    function getInput() {
        return editor ? editor.getValue() : editorTextarea.value;
    }

    // Set input
    function setInput(value) {
        if (editor) {
            editor.setValue(value);
            editor.setCursor(editor.lineCount(), 0);
        } else {
            editorTextarea.value = value;
        }
    }

    // Clear input
    function clearInput() {
        setInput('');
        if (editor) {
            editor.setSize('100%', '24px');
        }
    }

    // Focus input
    function focusInput() {
        if (editor) {
            editor.focus();
        } else {
            editorTextarea.focus();
        }
    }

    // Handle Enter key
    function handleEnter(cm) {
        // If autocomplete is open and item is selected, confirm it
        if (!autocompleteEl.classList.contains('hidden') && autocompleteIndex >= 0) {
            confirmAutocomplete();
            return;
        }

        const code = getInput().trim();
        if (code && isExpressionComplete(code)) {
            evaluate(code);
            return;
        }
        // Insert newline for multi-line input
        return CodeMirror.Pass;
    }

    // Handle Shift+Enter (always insert newline)
    function handleShiftEnter(cm) {
        return CodeMirror.Pass;
    }

    // Handle Up arrow (history or autocomplete navigation)
    function handleUp(cm) {
        // If autocomplete is open, navigate up
        if (!autocompleteEl.classList.contains('hidden')) {
            navigateAutocomplete(-1);
            return;
        }

        const cursor = cm.getCursor();
        if (cursor.line === 0) {
            navigateHistory(1);  // Go back in history (older commands)
            return;
        }
        return CodeMirror.Pass;
    }

    // Handle Down arrow (history or autocomplete navigation)
    function handleDown(cm) {
        // If autocomplete is open, navigate down
        if (!autocompleteEl.classList.contains('hidden')) {
            navigateAutocomplete(1);
            return;
        }

        const cursor = cm.getCursor();
        if (cursor.line === cm.lineCount() - 1) {
            navigateHistory(-1);  // Go forward in history (newer commands)
            return;
        }
        return CodeMirror.Pass;
    }

    // Handle Tab (autocomplete)
    function handleTab(cm) {
        // If autocomplete is open and item is selected, confirm it
        if (!autocompleteEl.classList.contains('hidden') && autocompleteIndex >= 0) {
            confirmAutocomplete();
            return;
        }

        const cursor = cm.getCursor();
        const line = cm.getLine(cursor.line);
        const prefix = getWordAtCursor(line, cursor.ch);
        if (prefix) {
            showAutocomplete(prefix);
            return;
        }
        return CodeMirror.Pass;
    }

    // Handle textarea keydown (fallback)
    function handleTextareaKeydown(e) {
        // Handle autocomplete navigation in textarea mode
        if (!autocompleteEl.classList.contains('hidden')) {
            if (e.key === 'ArrowUp') {
                e.preventDefault();
                navigateAutocomplete(-1);
                return;
            } else if (e.key === 'ArrowDown') {
                e.preventDefault();
                navigateAutocomplete(1);
                return;
            } else if (e.key === 'Enter' && autocompleteIndex >= 0) {
                e.preventDefault();
                confirmAutocomplete();
                return;
            } else if (e.key === 'Tab' && autocompleteIndex >= 0) {
                e.preventDefault();
                confirmAutocomplete();
                return;
            } else if (e.key === 'Escape') {
                e.preventDefault();
                hideAutocomplete();
                return;
            }
        }

        if (e.key === 'Enter' && !e.shiftKey) {
            const code = getInput().trim();
            if (code && isExpressionComplete(code)) {
                e.preventDefault();
                evaluate(code);
            }
        } else if (e.key === 'ArrowUp' && editorTextarea.selectionStart === 0) {
            e.preventDefault();
            navigateHistory(1);  // Go back in history (older commands)
        } else if (e.key === 'ArrowDown' && editorTextarea.selectionStart === editorTextarea.value.length) {
            e.preventDefault();
            navigateHistory(-1);  // Go forward in history (newer commands)
        } else if (e.key === 'Tab') {
            e.preventDefault();
            const pos = editorTextarea.selectionStart;
            const text = editorTextarea.value;
            const prefix = getWordAtCursorTextarea(text, pos);
            if (prefix) {
                showAutocomplete(prefix);
            }
        }
    }

    // Handle textarea input (for auto-show completions)
    function handleTextareaInput() {
        if (autocompleteDebounce) {
            clearTimeout(autocompleteDebounce);
        }
        autocompleteDebounce = setTimeout(() => {
            const pos = editorTextarea.selectionStart;
            const text = editorTextarea.value;
            const prefix = getWordAtCursorTextarea(text, pos);
            if (prefix && prefix.length >= 2) {
                showAutocomplete(prefix);
            } else {
                hideAutocomplete();
            }
        }, 150);
    }

    // Get word at cursor for textarea
    function getWordAtCursorTextarea(text, pos) {
        let start = pos;
        while (start > 0 && /[a-zA-Z0-9_\-!?*+<>=:]/.test(text[start - 1])) {
            start--;
        }
        return text.substring(start, pos);
    }

    // Check if expression is complete (balanced parens)
    function isExpressionComplete(code) {
        let parenCount = 0;
        let bracketCount = 0;
        let inString = false;
        let escape = false;

        for (const c of code) {
            if (escape) {
                escape = false;
                continue;
            }
            if (c === '\\') {
                escape = true;
                continue;
            }
            if (c === '"') {
                inString = !inString;
                continue;
            }
            if (inString) continue;

            if (c === '(') parenCount++;
            else if (c === ')') parenCount--;
            else if (c === '[') bracketCount++;
            else if (c === ']') bracketCount--;
        }

        return parenCount <= 0 && bracketCount <= 0 && !inString;
    }

    // Navigate history
    function navigateHistory(direction) {
        if (history.length === 0) return;

        if (historyIndex === -1) {
            currentInput = getInput();
        }

        historyIndex += direction;
        historyIndex = Math.max(-1, Math.min(historyIndex, history.length - 1));

        if (historyIndex === -1) {
            setInput(currentInput);
        } else {
            setInput(history[history.length - 1 - historyIndex]);
        }
    }

    // Get word at cursor position
    function getWordAtCursor(line, ch) {
        // Find start of word (Lisp symbol characters, including : for keywords)
        let start = ch;
        while (start > 0 && /[a-zA-Z0-9_\-!?*+<>=:]/.test(line[start - 1])) {
            start--;
        }
        return line.substring(start, ch);
    }

    // Navigate autocomplete list
    function navigateAutocomplete(direction) {
        if (autocompleteItems.length === 0) return;

        // Remove selection from current item
        const items = autocompleteEl.querySelectorAll('.autocomplete-item');
        if (autocompleteIndex >= 0 && autocompleteIndex < items.length) {
            items[autocompleteIndex].classList.remove('selected');
        }

        // Update index
        autocompleteIndex += direction;
        if (autocompleteIndex < 0) {
            autocompleteIndex = autocompleteItems.length - 1;
        } else if (autocompleteIndex >= autocompleteItems.length) {
            autocompleteIndex = 0;
        }

        // Add selection to new item
        if (autocompleteIndex >= 0 && autocompleteIndex < items.length) {
            items[autocompleteIndex].classList.add('selected');
            // Scroll into view
            items[autocompleteIndex].scrollIntoView({ block: 'nearest' });
        }
    }

    // Confirm autocomplete selection
    function confirmAutocomplete() {
        if (autocompleteIndex >= 0 && autocompleteIndex < autocompleteItems.length) {
            const item = autocompleteItems[autocompleteIndex];
            insertCompletion(autocompletePrefix, item.value);
        }
        hideAutocomplete();
    }

    // Show autocomplete popup
    async function showAutocomplete(prefix) {
        try {
            const response = await fetch(`/complete?prefix=${encodeURIComponent(prefix)}`);
            const data = await response.json();
            
            if (!data.completions || data.completions.length === 0) {
                hideAutocomplete();
                return;
            }

            autocompleteItems = data.completions;
            autocompletePrefix = prefix;
            autocompleteIndex = -1;

            autocompleteEl.innerHTML = '';
            for (let i = 0; i < autocompleteItems.length; i++) {
                const item = autocompleteItems[i];
                const div = document.createElement('div');
                div.className = 'autocomplete-item';
                div.dataset.index = i;

                // Highlight matching prefix
                const value = item.value;
                const matchEnd = prefix.length;
                const matchHtml = `<span class="match">${escapeHtml(value.substring(0, matchEnd))}</span>${escapeHtml(value.substring(matchEnd))}`;

                div.innerHTML = `<span class="value">${matchHtml}</span>
                    <span class="desc">${escapeHtml(item.description || '')}</span>`;
                div.addEventListener('click', () => {
                    autocompleteIndex = i;
                    confirmAutocomplete();
                });
                div.addEventListener('mouseenter', () => {
                    // Update visual selection on hover
                    const items = autocompleteEl.querySelectorAll('.autocomplete-item');
                    items.forEach(el => el.classList.remove('selected'));
                    div.classList.add('selected');
                    autocompleteIndex = i;
                });
                autocompleteEl.appendChild(div);
            }
            autocompleteEl.classList.remove('hidden');
        } catch (e) {
            console.error('Autocomplete error:', e);
            hideAutocomplete();
        }
    }

    // Hide autocomplete popup
    function hideAutocomplete() {
        autocompleteEl.classList.add('hidden');
        autocompleteItems = [];
        autocompleteIndex = -1;
        autocompletePrefix = '';
    }

    // Insert completion
    function insertCompletion(prefix, value) {
        if (editor) {
            const cursor = editor.getCursor();
            const line = editor.getLine(cursor.line);
            const start = cursor.ch - prefix.length;
            editor.replaceRange(value, {line: cursor.line, ch: start}, cursor);
        } else {
            const pos = editorTextarea.selectionStart;
            const text = editorTextarea.value;
            const start = pos - prefix.length;
            editorTextarea.value = text.substring(0, start) + value + text.substring(pos);
            editorTextarea.selectionStart = editorTextarea.selectionEnd = start + value.length;
        }
        focusInput();
    }

    // Evaluate code
    function evaluate(code) {
        if (isEvaluating) return;
        isEvaluating = true;
        hideAutocomplete();

        // Reset streaming output div for new evaluation
        currentOutputDiv = null;

        // Add to history
        if (code !== history[history.length - 1]) {
            history.push(code);
        }
        historyIndex = -1;

        // Show input in output (with syntax highlighting frozen at current state)
        appendOutput('input', code);
        clearInput();

        // Start SSE connection
        const url = `/eval?code=${encodeURIComponent(code)}`;
        eventSource = new EventSource(url);

        eventSource.addEventListener('output', (e) => {
            const data = JSON.parse(e.data);
            appendStreamingOutput(data.text);
        });

        eventSource.addEventListener('result', (e) => {
            const data = JSON.parse(e.data);
            appendOutput('result', data.value);
        });

        eventSource.addEventListener('error', (e) => {
            if (e.data) {
                const data = JSON.parse(e.data);
                appendOutput('error', 'Error: ' + data.message);
            }
        });

        eventSource.addEventListener('clear', () => {
            // Clear the output area
            output.innerHTML = '';
            appendWelcome();
        });

        eventSource.addEventListener('done', () => {
            eventSource.close();
            eventSource = null;
            isEvaluating = false;
            currentOutputDiv = null; // Reset streaming div
            // Refresh symbols after evaluation (new defines, etc.)
            fetchSymbols();
            focusInput();
        });

        eventSource.onerror = (e) => {
            if (eventSource) {
                eventSource.close();
                eventSource = null;
            }
            isEvaluating = false;
            appendOutput('error', 'Connection error');
            focusInput();
        };

        updateStatus('evaluating');
    }

    // Append welcome message
    function appendWelcome() {
        const div = document.createElement('div');
        div.className = 'welcome';
        div.innerHTML = `<span class="jlll-callable">JLLL</span> - <span class="jlll-variable">Java Lisp Like Language</span><br>
            <span class="hint">Type (help) for commands</span>`;
        output.appendChild(div);
    }

    // Append streaming output (reuses same div for continuous output)
    function appendStreamingOutput(text) {
        if (!currentOutputDiv) {
            // Create new output div for this evaluation's output
            currentOutputDiv = document.createElement('div');
            currentOutputDiv.className = 'output-line output';
            output.appendChild(currentOutputDiv);
        }
        // Append text to existing div (preserves streaming effect)
        currentOutputDiv.textContent += text;
        output.scrollTop = output.scrollHeight;
    }

    // Append to output
    function appendOutput(type, text) {
        // Clear streaming div reference when adding non-output content
        if (type !== 'output') {
            currentOutputDiv = null;
        }

        const div = document.createElement('div');
        div.className = 'output-line ' + type;

        if (type === 'input') {
            // Apply syntax highlighting to input lines (frozen at current symbol state)
            div.innerHTML = '<span class="prompt">&gt; </span>' + highlightJlll(text);
            div.dataset.code = text;
            div.title = 'Click to edit';
        } else {
            div.textContent = text;
        }

        output.appendChild(div);
        output.scrollTop = output.scrollHeight;
        
        if (type === 'result' || type === 'error') {
            updateStatus('connected');
        }
    }

    // Update status indicator
    function updateStatus(status) {
        statusEl.className = 'status ' + status;
        statusEl.textContent = status === 'evaluating' ? 'Evaluating...' : 
                               status === 'connected' ? 'Connected' : 'Disconnected';
    }

    // Escape HTML
    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    // Check server status
    async function checkStatus() {
        try {
            const response = await fetch('/status');
            const data = await response.json();
            updateStatus(data.running ? 'connected' : 'disconnected');
        } catch (e) {
            updateStatus('disconnected');
        }
    }

    // Initialize
    document.addEventListener('DOMContentLoaded', async () => {
        // Fetch symbols first for syntax highlighting
        await fetchSymbols();

        initEditor();
        focusInput();
        checkStatus();

        // Periodic status check
        setInterval(checkStatus, 30000);

        // Handle clicks on output area
        output.addEventListener('click', (e) => {
            const inputLine = e.target.closest('.output-line.input');
            if (inputLine && inputLine.dataset.code) {
                setInput(inputLine.dataset.code);
                focusInput();
                return;
            }
        });

        // Click anywhere else to focus input and hide autocomplete
        document.addEventListener('click', (e) => {
            if (!autocompleteEl.contains(e.target)) {
                hideAutocomplete();
            }
            if (e.target.closest('#output') || e.target.closest('#app')) {
                if (!e.target.closest('.output-line.input')) {
                    focusInput();
                }
            }
        });
    });
})();
