package ru.ydn.jlll.libs;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Symbol;

/**
 * Web Console specific library providing commands for browser-based REPL.
 *
 * <p>
 * This library is loaded only by the web console and provides:
 * </p>
 * <ul>
 * <li><b>help:</b> Shows web console keyboard shortcuts and commands</li>
 * <li><b>clear:</b> Clears the output area</li>
 * </ul>
 *
 * <p>
 * Unlike {@link ru.ydn.jlll.cli.ReplLib}, this library does not include
 * quit/exit commands as they are meaningless in a browser context.
 * </p>
 */
public class WebConsoleReplLib implements Library
{
    /** Symbol used to signal the web console to clear the output area. */
    public static final Symbol CLEAR_MARKER = Symbol.intern("__web-console-clear__");

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        // Web console specific: help command
        new Primitive("help", env, "Shows web console help message with available commands and keyboard shortcuts.")
        {
            private static final long serialVersionUID = 9173645091827364523L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Console console = KernelLib.getConsole(env);
                console.println();
                console.printHeader("Web Console Commands:");
                console.println("  (help)            Show this help message");
                console.println("  (clear)           Clear the output area");
                console.println("  (env)             Show all environment bindings");
                console.println("  (env \"prefix\")    Show bindings matching prefix");
                console.println("  (env :primitives) Show only primitives");
                console.println("  (env :macros)     Show only macros");
                console.println("  (env :procedures) Show only procedures");
                console.println("  (env :variables)  Show only variables");
                console.println("  (doc 'symbol)     Show documentation for symbol");
                console.println();
                console.printHeader("Keyboard Shortcuts:");
                console.println("  Enter             Evaluate expression (when complete)");
                console.println("  Shift+Enter       Insert newline");
                console.println("  Tab               Show/trigger autocomplete");
                console.println("  Up/Down           Navigate autocomplete or history");
                console.println("  Escape            Hide autocomplete");
                console.println();
                console.printHeader("Tips:");
                console.println("  - Click on any previous input to edit it again");
                console.println("  - Autocomplete shows after typing 2+ characters");
                console.println("  - Use arrow keys to select from autocomplete list");
                console.println();
                console.flush();
                return Null.NULL;
            }
        };
        // Web console specific: clear command
        new Primitive("clear", env, "Clears the output area.")
        {
            private static final long serialVersionUID = 9173645091827364524L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                // Return a special marker that the web console recognizes
                // The WebConsoleLib will detect this and send a 'clear' SSE event
                return CLEAR_MARKER;
            }
        };
    }
}
