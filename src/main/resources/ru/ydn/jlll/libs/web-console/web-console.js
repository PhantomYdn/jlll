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

    // Initialize CodeMirror if available
    function initEditor() {
        if (typeof CodeMirror !== 'undefined') {
            editor = CodeMirror.fromTextArea(editorTextarea, {
                mode: 'commonlisp',
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
            editor.on('change', () => {
                adjustEditorHeight();
                hideAutocomplete();
            });
        } else {
            // Fallback to plain textarea
            editorTextarea.addEventListener('keydown', handleTextareaKeydown);
        }
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

    // Handle Up arrow (history)
    function handleUp(cm) {
        const cursor = cm.getCursor();
        if (cursor.line === 0) {
            navigateHistory(-1);
            return;
        }
        return CodeMirror.Pass;
    }

    // Handle Down arrow (history)
    function handleDown(cm) {
        const cursor = cm.getCursor();
        if (cursor.line === cm.lineCount() - 1) {
            navigateHistory(1);
            return;
        }
        return CodeMirror.Pass;
    }

    // Handle Tab (autocomplete)
    function handleTab(cm) {
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
        if (e.key === 'Enter' && !e.shiftKey) {
            const code = getInput().trim();
            if (code && isExpressionComplete(code)) {
                e.preventDefault();
                evaluate(code);
            }
        } else if (e.key === 'ArrowUp' && editorTextarea.selectionStart === 0) {
            e.preventDefault();
            navigateHistory(-1);
        } else if (e.key === 'ArrowDown' && editorTextarea.selectionStart === editorTextarea.value.length) {
            e.preventDefault();
            navigateHistory(1);
        }
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
        // Find start of word (Lisp symbol characters)
        let start = ch;
        while (start > 0 && /[a-zA-Z0-9_\-!?*+<>=]/.test(line[start - 1])) {
            start--;
        }
        return line.substring(start, ch);
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

            autocompleteEl.innerHTML = '';
            for (const item of data.completions) {
                const div = document.createElement('div');
                div.className = 'autocomplete-item';
                div.innerHTML = `<span class="value">${escapeHtml(item.value)}</span>
                    <span class="desc">${escapeHtml(item.description || '')}</span>`;
                div.addEventListener('click', () => {
                    insertCompletion(prefix, item.value);
                    hideAutocomplete();
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

        // Add to history
        if (code !== history[history.length - 1]) {
            history.push(code);
        }
        historyIndex = -1;

        // Show input in output
        appendOutput('input', '> ' + code);
        clearInput();

        // Start SSE connection
        const url = `/eval?code=${encodeURIComponent(code)}`;
        eventSource = new EventSource(url);

        eventSource.addEventListener('output', (e) => {
            const data = JSON.parse(e.data);
            appendOutput('output', data.text);
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

        eventSource.addEventListener('done', () => {
            eventSource.close();
            eventSource = null;
            isEvaluating = false;
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

    // Append to output
    function appendOutput(type, text) {
        const div = document.createElement('div');
        div.className = 'output-line ' + type;
        div.textContent = text;
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
    document.addEventListener('DOMContentLoaded', () => {
        initEditor();
        focusInput();
        checkStatus();

        // Periodic status check
        setInterval(checkStatus, 30000);

        // Click anywhere to focus input
        document.addEventListener('click', (e) => {
            if (!autocompleteEl.contains(e.target)) {
                hideAutocomplete();
            }
            if (e.target.closest('#output') || e.target.closest('#app')) {
                focusInput();
            }
        });
    });
})();
