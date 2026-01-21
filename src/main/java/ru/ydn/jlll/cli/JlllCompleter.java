package ru.ydn.jlll.cli;

import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import org.jline.reader.Candidate;
import org.jline.reader.Completer;
import org.jline.reader.LineReader;
import org.jline.reader.ParsedLine;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Macros;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;

/**
 * Tab completion for JLLL REPL.
 * Completes symbols, primitives, and keywords from the current environment.
 */
public class JlllCompleter implements Completer
{
    private final Enviroment env;
    // Built-in Lisp keywords that are always available
    private static final String[] KEYWORDS =
    {"define", "defmacro", "lambda", "if", "cond", "case", "let", "let*", "letrec", "begin", "quote", "quasiquote",
            "set", "set!", "and", "or", "not", "car", "cdr", "cons", "list", "append", "map", "filter", "apply", "eval",
            "true", "false", "null", "nil", "load-lib", "load-url", "load-system-script", "help", "quit", "exit",
            "env"};

    /**
     * Creates a completer for the given environment.
     *
     * @param env
     *            the JLLL environment to provide completions from
     */
    public JlllCompleter(Enviroment env)
    {
        this.env = env;
    }

    /**
     * Provides completion candidates for the current input.
     * Matches symbols and keywords that start with the current word prefix.
     *
     * @param reader
     *            the line reader
     * @param line
     *            the parsed line
     * @param candidates
     *            list to add completion candidates to
     */
    @Override
    public void complete(LineReader reader, ParsedLine line, List<Candidate> candidates)
    {
        // Use the word from the parser directly - it now correctly identifies Lisp symbols
        String prefix = line.word();
        // Collect all possible completions
        TreeSet<String> completions = new TreeSet<>();
        // Add keywords
        for (String keyword : KEYWORDS)
        {
            if (keyword.startsWith(prefix))
            {
                completions.add(keyword);
            }
        }
        // Add symbols from current environment
        Map<Symbol, Object> bindings = env.getAllBindings();
        for (Map.Entry<Symbol, Object> entry : bindings.entrySet())
        {
            String name = entry.getKey().getName();
            if (name.startsWith(prefix))
            {
                completions.add(name);
            }
        }
        // Convert to candidates with descriptions
        for (String completion : completions)
        {
            String description = getDescription(completion, bindings);
            candidates.add(new Candidate(completion, // value
                    completion, // display
                    null, // group
                    description, // description
                    null, // suffix
                    null, // key
                    true // complete - adds trailing space
            ));
        }
    }

    /**
     * Get a short description for a symbol.
     * Prefers :doc metadata when available for cleaner completion hints.
     */
    private String getDescription(String name, Map<Symbol, Object> bindings)
    {
        // Check special REPL commands
        switch (name)
        {
            case "help" :
                return "Show help message";
            case "quit" :
            case "exit" :
                return "Exit the REPL";
            case "env" :
                return "Show environment bindings (with optional filter)";
            case "doc" :
                return "Show documentation for a symbol";
        }
        // Look up in environment
        Symbol symbol = Symbol.intern(name);
        Object value = bindings.get(symbol);
        // Try to get doc from metadata first
        Object doc = env.getMeta(symbol, Symbol.intern("doc"));
        if (doc != null && !doc.toString().isEmpty())
        {
            return truncateDoc(doc.toString(), 60);
        }
        // Fall back to type-based description
        if (value == null)
        {
            return "keyword";
        }
        else if (value instanceof Primitive)
        {
            return "primitive";
        }
        else if (value instanceof Macros)
        {
            return "macro";
        }
        else if (value instanceof Procedure)
        {
            return "procedure";
        }
        else if (value instanceof Boolean)
        {
            return "boolean: " + value;
        }
        else if (value instanceof Number)
        {
            return "number: " + value;
        }
        else if (value instanceof String)
        {
            return "string";
        }
        else
        {
            return value.getClass().getSimpleName();
        }
    }

    /**
     * Truncates documentation to fit in completion description.
     * Prefers ending at sentence boundary.
     */
    private String truncateDoc(String doc, int maxLen)
    {
        if (doc.length() <= maxLen)
        {
            return doc;
        }
        // Try to end at first sentence
        int period = doc.indexOf('.');
        if (period > 0 && period < maxLen)
        {
            return doc.substring(0, period + 1);
        }
        // Otherwise truncate at word boundary
        int end = doc.lastIndexOf(' ', maxLen - 3);
        if (end < maxLen / 2)
        {
            end = maxLen - 3;
        }
        return doc.substring(0, end) + "...";
    }
}
