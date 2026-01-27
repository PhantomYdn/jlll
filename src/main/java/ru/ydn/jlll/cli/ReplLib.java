package ru.ydn.jlll.cli;

import org.jline.reader.EndOfFileException;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.libs.KernelLib;

/**
 * REPL-specific library providing commands for interactive use.
 *
 * <p>
 * This library is loaded only by the REPL and provides:
 * </p>
 * <ul>
 * <li><b>help:</b> Shows REPL keyboard shortcuts and commands</li>
 * <li><b>quit/exit:</b> Exits the REPL</li>
 * </ul>
 *
 * <p>
 * Note: The {@code env} and {@code doc} commands use Console for styled output,
 * so they work with colors automatically when a JLineConsole is bound.
 * </p>
 */
public class ReplLib implements Library
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        // REPL-specific: help command
        new Primitive("help", env, "Shows REPL help message with available commands and keyboard shortcuts.")
        {
            private static final long serialVersionUID = 8273645091827364523L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                Console console = KernelLib.getConsole(env);
                console.println();
                console.printHeader("REPL Commands:");
                console.println("  (help)            Show this help message");
                console.println("  (env)             Show all environment bindings");
                console.println("  (env \"prefix\")    Show bindings matching prefix");
                console.println("  (env :primitives) Show only primitives");
                console.println("  (env :macros)     Show only macros");
                console.println("  (env :procedures) Show only procedures");
                console.println("  (env :variables)  Show only variables");
                console.println("  (doc 'symbol)     Show documentation for symbol");
                console.println("  (quit)            Exit the REPL");
                console.println("  (exit)            Exit the REPL");
                console.println();
                console.printHeader("Keyboard Shortcuts:");
                console.println("  Ctrl+D            Exit the REPL");
                console.println("  Ctrl+C            Cancel current input");
                console.println("  Ctrl+R            Search command history");
                console.println("  Tab               Auto-complete symbols");
                console.println("  Up/Down           Navigate command history");
                console.println();
                console.flush();
                return Null.NULL;
            }
        };
        // REPL-specific: quit command
        new Primitive("quit", env, "Exits the REPL.")
        {
            private static final long serialVersionUID = 8273645091827364524L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                throw new JlllException(new EndOfFileException("quit"));
            }
        };
        // REPL-specific: exit command (alias for quit)
        new Primitive("exit", env, "Exits the REPL (alias for quit).")
        {
            private static final long serialVersionUID = 8273645091827364525L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                throw new JlllException(new EndOfFileException("exit"));
            }
        };
    }
}
