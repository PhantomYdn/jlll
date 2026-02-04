package ru.ydn.jlll.cli;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Callable;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.IVersionProvider;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.libs.KernelLib;
import ru.ydn.jlll.libs.WebConsoleLib;

/**
 * Command-line interface for JLLL interpreter.
 * Provides a modern CLI with proper argument parsing, help, and version support.
 */
@Command(name = "jlll", mixinStandardHelpOptions = true, versionProvider = JlllCli.VersionProvider.class, subcommands =
{JlllCli.WebCommand.class}, description = "JLLL - Java Lisp Like Language interpreter", footer =
{"", "Examples:", "  jlll                        Start interactive REPL (loads ~/.jlllrc)",
        "  jlll script.jlll            Execute script file (no init file)",
        "  jlll -i script.jlll         Execute script, then REPL (loads init)",
        "  jlll -e '(+ 1 2 3)'         Evaluate expression", "  jlll --rc custom.jlll       Use custom init file",
        "  jlll --no-rc                Skip loading init file",
        "  jlll web                    Start web console (browser-based REPL)",
        "  jlll web --port 3000        Start web console on custom port"})
public class JlllCli implements Callable<Integer>
{
    @Option(names =
    {"-e", "--eval"}, description = "Evaluate expression(s) before files", paramLabel = "EXPR")
    private List<String> expressions;
    @Option(names =
    {"-i", "--interactive"}, description = "Enter REPL after executing files/expressions")
    private boolean forceInteractive;
    @Option(names =
    {"--no-color"}, description = "Disable ANSI colors in output")
    private boolean noColor;
    @Option(names =
    {"-q", "--quiet"}, description = "Suppress startup banner in REPL")
    private boolean quiet;
    @Option(names =
    {"--rc"}, description = "Specify alternate init file (default: ~/.jlllrc)", paramLabel = "FILE")
    private String rcFile;
    @Option(names =
    {"--no-rc"}, description = "Skip loading init file")
    private boolean noRc;
    @Parameters(paramLabel = "FILE", description = "Script file(s) to execute")
    private List<File> files;
    private Environment env;

    /**
     * Executes the CLI command - evaluates expressions, runs files, and/or starts REPL.
     *
     * @return exit code (0 for success, 1 for error)
     * @throws Exception
     *             if unexpected error occurs
     */
    @Override
    public Integer call() throws Exception
    {
        env = new Environment(Environment.top);
        boolean hasWork = false;
        // Evaluate -e expressions first
        if (expressions != null && !expressions.isEmpty())
        {
            hasWork = true;
            for (String expr : expressions)
            {
                try
                {
                    Object result = Jlll.eval(new StringReader(expr), env);
                    if (result != null)
                    {
                        System.out.println(result);
                    }
                }
                catch (JlllException e)
                {
                    System.err.println("Error: " + e.getMessage());
                    if (e.getCause() != null)
                    {
                        System.err.println("Caused by: " + e.getCause().getMessage());
                    }
                    return 1;
                }
            }
        }
        // Execute files
        if (files != null && !files.isEmpty())
        {
            hasWork = true;
            for (File file : files)
            {
                if (!file.exists())
                {
                    System.err.println("Error: File not found: " + file.getPath());
                    return 1;
                }
                if (!file.canRead())
                {
                    System.err.println("Error: Cannot read file: " + file.getPath());
                    return 1;
                }
                try (FileReader reader = new FileReader(file))
                {
                    Jlll.eval(reader, env);
                }
                catch (JlllException e)
                {
                    System.err.println("Error in " + file.getName() + ": " + e.getMessage());
                    if (e.getCause() != null)
                    {
                        System.err.println("Caused by: " + e.getCause().getMessage());
                    }
                    return 1;
                }
            }
        }
        // Start REPL if no work done or --interactive flag
        if (!hasWork || forceInteractive)
        {
            // Load init file before starting REPL (unless --no-rc)
            if (!noRc)
            {
                if (!loadInitFile())
                {
                    return 1;
                }
            }
            JlllRepl repl = new JlllRepl(env);
            repl.setColorEnabled(!noColor);
            repl.setQuiet(quiet);
            return repl.run();
        }
        return 0;
    }

    /**
     * Expands ~ to user home directory in a path string.
     *
     * @param path
     *            the path that may start with ~
     * @return the expanded path
     */
    private String expandTilde(String path)
    {
        if (path.startsWith("~"))
        {
            String home = System.getProperty("user.home");
            if (path.equals("~"))
            {
                return home;
            }
            if (path.startsWith("~/") || path.startsWith("~" + File.separator))
            {
                return home + path.substring(1);
            }
        }
        return path;
    }

    /**
     * Loads the init file (~/.jlllrc or custom path specified by --rc).
     * If --rc is specified, the file must exist. If using default ~/.jlllrc,
     * missing file is silently ignored.
     *
     * @return true if successful or default file doesn't exist, false on error
     */
    private boolean loadInitFile()
    {
        File initFile;
        boolean isExplicit = (rcFile != null);
        if (isExplicit)
        {
            String expandedPath = expandTilde(rcFile);
            initFile = new File(expandedPath);
        }
        else
        {
            String home = System.getProperty("user.home");
            initFile = new File(home, ".jlllrc");
        }
        // Check if file exists
        if (!initFile.exists())
        {
            if (isExplicit)
            {
                // Explicit --rc file must exist
                System.err.println("Error: Init file not found: " + initFile.getAbsolutePath());
                return false;
            }
            // Default file missing is OK
            return true;
        }
        // Check if file is readable
        if (!initFile.canRead())
        {
            System.err.println("Error: Cannot read init file: " + initFile.getAbsolutePath());
            return false;
        }
        // Load the init file
        try (FileReader reader = new FileReader(initFile))
        {
            Jlll.eval(reader, env);
            return true;
        }
        catch (JlllException e)
        {
            System.err.println("Error in init file " + initFile.getAbsolutePath() + ": " + e.getMessage());
            if (e.getCause() != null)
            {
                System.err.println("Caused by: " + e.getCause().getMessage());
            }
            return false;
        }
        catch (IOException e)
        {
            System.err.println("Error reading init file " + initFile.getAbsolutePath() + ": " + e.getMessage());
            return false;
        }
    }

    /**
     * Main entry point for the JLLL CLI.
     */
    public static void main(String[] args)
    {
        int exitCode = new CommandLine(new JlllCli()).setCaseInsensitiveEnumValuesAllowed(true).execute(args);
        System.exit(exitCode);
    }

    /**
     * Provides version information from the Maven-filtered properties file.
     * Reads jlll.version and jlll.name from version.properties resource.
     */
    static class VersionProvider implements IVersionProvider
    {
        /**
         * Returns version strings for --version option.
         *
         * @return array containing version string
         * @throws Exception
         *             if resource loading fails
         */
        @Override
        public String[] getVersion() throws Exception
        {
            Properties props = new Properties();
            try (InputStream is = getClass().getResourceAsStream("version.properties"))
            {
                if (is != null)
                {
                    props.load(is);
                    String version = props.getProperty("jlll.version", "unknown");
                    String name = props.getProperty("jlll.name", "JLLL");
                    return new String[]
                    {name + " " + version};
                }
            }
            catch (IOException e)
            {
                // Fall through to default
            }
            return new String[]
            {"JLLL (version unknown)"};
        }
    }

    /**
     * Subcommand for starting the web console (browser-based REPL).
     */
    @Command(name = "web", description = "Start browser-based REPL", mixinStandardHelpOptions = true, footer =
    {"", "Examples:", "  jlll web                    Start on default port (8080, localhost only)",
            "  jlll web --port 3000        Custom port",
            "  jlll web --bind 0.0.0.0     Allow external connections (use with caution!)",
            "  jlll web --isolated         Each browser connection gets isolated environment"})
    static class WebCommand implements Callable<Integer>
    {
        @Option(names =
        {"-p", "--port"}, description = "Port to listen on (default: 8080)", defaultValue = "8080")
        private int port;
        @Option(names =
        {"-b", "--bind"}, description = "Host/interface to bind to (default: 127.0.0.1)", defaultValue = "127.0.0.1")
        private String host;
        @Option(names =
        {"--isolated"}, description = "Each connection gets isolated environment")
        private boolean isolated;
        @Option(names =
        {"--rc"}, description = "Specify alternate init file (default: ~/.jlllrc)", paramLabel = "FILE")
        private String rcFile;
        @Option(names =
        {"--no-rc"}, description = "Skip loading init file")
        private boolean noRc;
        private Environment env;

        @Override
        public Integer call() throws Exception
        {
            env = new Environment(Environment.top);
            // Load init file (unless --no-rc)
            if (!noRc)
            {
                if (!loadInitFile())
                {
                    return 1;
                }
            }
            // Load web console library
            try
            {
                new WebConsoleLib().load(env);
            }
            catch (JlllException e)
            {
                System.err.println("Error loading web console library: " + e.getMessage());
                return 1;
            }
            // Build the command to start web console
            StringBuilder cmd = new StringBuilder("(web-console");
            cmd.append(" :port ").append(port);
            cmd.append(" :bind \"").append(host).append("\"");
            if (isolated)
            {
                cmd.append(" :isolated true");
            }
            cmd.append(")");
            try
            {
                Jlll.eval(new StringReader(cmd.toString()), env);
            }
            catch (JlllException e)
            {
                System.err.println("Error starting web console: " + e.getMessage());
                return 1;
            }
            // Keep running until interrupted
            Console console = KernelLib.getConsole(env);
            console.println();
            console.printHint("Press Ctrl+C to stop the web console");
            console.println();
            console.flush();
            try
            {
                // Wait forever (until Ctrl+C)
                Thread.currentThread().join();
            }
            catch (InterruptedException e)
            {
                // Shutdown
                console.println();
                console.println("Web console stopped.");
                console.flush();
            }
            return 0;
        }

        /**
         * Expands ~ to user home directory in a path string.
         */
        private String expandTilde(String path)
        {
            if (path.startsWith("~"))
            {
                String home = System.getProperty("user.home");
                if (path.equals("~"))
                {
                    return home;
                }
                if (path.startsWith("~/") || path.startsWith("~" + File.separator))
                {
                    return home + path.substring(1);
                }
            }
            return path;
        }

        /**
         * Loads the init file (~/.jlllrc or custom path specified by --rc).
         */
        private boolean loadInitFile()
        {
            File initFile;
            boolean isExplicit = (rcFile != null);
            if (isExplicit)
            {
                String expandedPath = expandTilde(rcFile);
                initFile = new File(expandedPath);
            }
            else
            {
                String home = System.getProperty("user.home");
                initFile = new File(home, ".jlllrc");
            }
            // Check if file exists
            if (!initFile.exists())
            {
                if (isExplicit)
                {
                    System.err.println("Error: Init file not found: " + initFile.getAbsolutePath());
                    return false;
                }
                return true;
            }
            // Check if file is readable
            if (!initFile.canRead())
            {
                System.err.println("Error: Cannot read init file: " + initFile.getAbsolutePath());
                return false;
            }
            // Load the init file
            try (FileReader reader = new FileReader(initFile))
            {
                Jlll.eval(reader, env);
                return true;
            }
            catch (JlllException e)
            {
                System.err.println("Error in init file " + initFile.getAbsolutePath() + ": " + e.getMessage());
                if (e.getCause() != null)
                {
                    System.err.println("Caused by: " + e.getCause().getMessage());
                }
                return false;
            }
            catch (IOException e)
            {
                System.err.println("Error reading init file " + initFile.getAbsolutePath() + ": " + e.getMessage());
                return false;
            }
        }
    }
}
