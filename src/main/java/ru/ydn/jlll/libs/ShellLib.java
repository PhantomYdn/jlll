package ru.ydn.jlll.libs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.ParameterParser;
import ru.ydn.jlll.common.ParameterParser.KeywordExtraction;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.ReflectionLibrary;

/**
 * Shell command execution library.
 *
 * <p>
 * Provides the {@code bash} primitive for executing shell commands and capturing output.
 * </p>
 *
 * <p>
 * The {@code bash} function executes a shell command and returns a hash-map with:
 * </p>
 * <ul>
 * <li><b>:stdout</b> - Standard output as string</li>
 * <li><b>:stderr</b> - Standard error as string</li>
 * <li><b>:exit-code</b> - Exit code (0 = success)</li>
 * </ul>
 *
 * <p>
 * Supported options:
 * </p>
 * <ul>
 * <li><b>:timeout</b> - Timeout in milliseconds (default: 120000)</li>
 * <li><b>:cwd</b> - Working directory</li>
 * <li><b>:input</b> - Data to pipe to stdin (string or Reader)</li>
 * <li><b>:env</b> - Additional environment variables (hash-map)</li>
 * <li><b>:stream</b> - If true, print output in real-time and return only exit code</li>
 * </ul>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>
 * (bash "ls -la")
 * ;; =&gt; {:stdout "total 48\n..." :stderr "" :exit-code 0}
 *
 * (bash "grep pattern" :input "line1\nline2\npattern here\nline4")
 * ;; =&gt; {:stdout "pattern here\n" :stderr "" :exit-code 0}
 *
 * (bash "npm test" :cwd "/path/to/project" :timeout 60000)
 *
 * ;; Streaming mode - prints output in real-time, returns exit code
 * (bash "for i in 1 2 3; do echo $i; sleep 1; done" :stream true)
 * ;; Prints: 1, 2, 3 (with delays)
 * ;; =&gt; 0
 * </pre>
 */
public class ShellLib extends ReflectionLibrary
{
    /** Default timeout in milliseconds (2 minutes). */
    private static final long DEFAULT_TIMEOUT = 120000L;
    /** Shell command for Unix systems. */
    private static final String[] UNIX_SHELL =
    {"/bin/sh", "-c"};
    /** Shell command for Windows systems. */
    private static final String[] WINDOWS_SHELL =
    {"cmd", "/c"};

    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        new Primitive("bash", env,
                "Execute shell command. Returns hash-map with :stdout, :stderr, :exit-code. "
                        + "Options: :timeout (ms, default 120000), :cwd (working directory), "
                        + ":input (string or Reader to pipe to stdin), :env (hash-map of additional env vars), "
                        + ":stream (if true, print output in real-time and return only exit code).")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                if (values == null || values.isNull())
                {
                    throw new JlllException("bash: command string required");
                }
                KeywordExtraction extraction = ParameterParser.extractKeywords(values);
                if (extraction.positional.isEmpty())
                {
                    throw new JlllException("bash: command string required");
                }
                String command = extraction.positional.get(0).toString();
                long timeout = ParameterParser.getLong(extraction, "timeout", DEFAULT_TIMEOUT);
                String cwd = ParameterParser.getString(extraction, "cwd", null);
                Object input = ParameterParser.getObject(extraction, "input", null);
                Object envVars = ParameterParser.getObject(extraction, "env", null);
                boolean stream = ParameterParser.getBoolean(extraction, "stream", false);
                Console console = stream ? KernelLib.getConsole(env) : null;
                return executeCommand(command, timeout, cwd, input, envVars, stream, console);
            }
        };
        // Load JLLL wrappers (shell alias, etc.)
        Jlll.eval("(load-system-script \"shell.jlll\")", env);
    }

    /**
     * Executes a shell command and returns the result.
     *
     * @param command
     *            the shell command to execute
     * @param timeout
     *            timeout in milliseconds
     * @param cwd
     *            working directory (null for current directory)
     * @param input
     *            data to pipe to stdin (String or Reader, null for none)
     * @param envVars
     *            additional environment variables (Map, null for none)
     * @param stream
     *            if true, print output in real-time to console
     * @param console
     *            the console for streaming output (required if stream is true)
     * @return hash-map with :stdout, :stderr, :exit-code (or just exit code if streaming)
     * @throws JlllException
     *             if command execution fails
     */
    @SuppressWarnings("unchecked")
    private Object executeCommand(String command, long timeout, String cwd, Object input, Object envVars,
            boolean stream, Console console) throws JlllException
    {
        String[] shellCmd = getShellCommand();
        String[] fullCommand = new String[shellCmd.length + 1];
        System.arraycopy(shellCmd, 0, fullCommand, 0, shellCmd.length);
        fullCommand[shellCmd.length] = command;
        ProcessBuilder pb = new ProcessBuilder(fullCommand);
        // Set working directory
        if (cwd != null)
        {
            Path cwdPath = Paths.get(expandPath(cwd));
            if (!cwdPath.toFile().isDirectory())
            {
                throw new JlllException("bash: working directory does not exist: " + cwd);
            }
            pb.directory(cwdPath.toFile());
        }
        // Set additional environment variables
        if (envVars != null)
        {
            if (!(envVars instanceof Map))
            {
                throw new JlllException("bash: :env must be a hash-map");
            }
            Map<?, ?> envMap = (Map<?, ?>) envVars;
            Map<String, String> processEnv = pb.environment();
            for (Map.Entry<?, ?> entry : envMap.entrySet())
            {
                String key = entry.getKey().toString();
                // Handle keyword keys (strip leading colon)
                if (entry.getKey() instanceof Keyword)
                {
                    key = ((Keyword) entry.getKey()).toSymbol().getName();
                }
                processEnv.put(key, entry.getValue().toString());
            }
        }
        try
        {
            Process process = pb.start();
            // Handle stdin input in a separate thread
            Thread inputThread = null;
            if (input != null)
            {
                inputThread = new Thread(() ->
                {
                    try (OutputStream os = process.getOutputStream())
                    {
                        if (input instanceof String)
                        {
                            os.write(((String) input).getBytes(StandardCharsets.UTF_8));
                        }
                        else if (input instanceof Reader)
                        {
                            Reader reader = (Reader) input;
                            char[] buffer = new char[8192];
                            int read;
                            while ((read = reader.read(buffer)) != -1)
                            {
                                os.write(new String(buffer, 0, read).getBytes(StandardCharsets.UTF_8));
                            }
                        }
                        os.flush();
                    }
                    catch (IOException e)
                    {
                        // Ignore - process may have closed stdin early
                    }
                }, "bash-stdin-writer");
                inputThread.setDaemon(true);
                inputThread.start();
            }
            else
            {
                // Close stdin immediately if no input
                process.getOutputStream().close();
            }
            // Capture stdout and stderr in parallel threads
            StringBuilder stdout = new StringBuilder();
            StringBuilder stderr = new StringBuilder();
            Thread stdoutThread = createStreamReader(process.getInputStream(), stdout, stream, console, false);
            Thread stderrThread = createStreamReader(process.getErrorStream(), stderr, stream, console, true);
            stdoutThread.start();
            stderrThread.start();
            // Wait for process with timeout
            boolean finished = process.waitFor(timeout, TimeUnit.MILLISECONDS);
            int exitCode;
            if (!finished)
            {
                // Timeout - destroy the process
                process.destroyForcibly();
                try
                {
                    process.waitFor(5, TimeUnit.SECONDS); // Give it time to die
                }
                catch (InterruptedException ie)
                {
                    Thread.currentThread().interrupt();
                }
                exitCode = -1;
                stderr.append("\n[Process timed out after ").append(timeout).append("ms and was terminated]");
            }
            else
            {
                exitCode = process.exitValue();
            }
            // Wait for output threads to finish
            stdoutThread.join(5000);
            stderrThread.join(5000);
            if (inputThread != null)
            {
                inputThread.join(1000);
            }
            // Return just exit code when streaming, full hash-map otherwise
            if (stream)
            {
                return exitCode;
            }
            // Build result map
            Map<Object, Object> result = new LinkedHashMap<>();
            result.put(Keyword.intern("stdout"), stdout.toString());
            result.put(Keyword.intern("stderr"), stderr.toString());
            result.put(Keyword.intern("exit-code"), exitCode);
            return result;
        }
        catch (IOException e)
        {
            throw new JlllException("bash: failed to execute command: " + e.getMessage());
        }
        catch (InterruptedException e)
        {
            Thread.currentThread().interrupt();
            throw new JlllException("bash: command execution interrupted");
        }
    }

    /**
     * Creates a thread that reads from an input stream into a StringBuilder.
     * Optionally streams output to console in real-time.
     *
     * @param input
     *            the input stream to read from
     * @param output
     *            the StringBuilder to write to
     * @param stream
     *            if true, print output to console in real-time
     * @param console
     *            the console for streaming output (may be null if stream is false)
     * @param isStderr
     *            if true, this is stderr (print in red when streaming)
     * @return the reader thread (not started)
     */
    private Thread createStreamReader(InputStream input, StringBuilder output, boolean stream, Console console,
            boolean isStderr)
    {
        Thread thread = new Thread(() ->
        {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(input, StandardCharsets.UTF_8)))
            {
                char[] buffer = new char[8192];
                int read;
                while ((read = reader.read(buffer)) != -1)
                {
                    String chunk = new String(buffer, 0, read);
                    output.append(chunk);
                    // Stream to console if requested
                    if (stream && console != null)
                    {
                        if (isStderr)
                        {
                            console.printColored(chunk, Console.Color.RED);
                        }
                        else
                        {
                            console.print(chunk);
                        }
                        console.flush();
                    }
                }
            }
            catch (IOException e)
            {
                // Stream closed, ignore
            }
        }, "bash-stream-reader");
        thread.setDaemon(true);
        return thread;
    }

    /**
     * Gets the shell command array for the current operating system.
     *
     * @return shell command array (e.g., {"/bin/sh", "-c"})
     */
    private String[] getShellCommand()
    {
        String os = System.getProperty("os.name").toLowerCase();
        if (os.contains("win"))
        {
            return WINDOWS_SHELL;
        }
        return UNIX_SHELL;
    }

    /**
     * Expands ~ to user home directory in paths.
     *
     * @param path
     *            the path to expand
     * @return expanded path
     */
    private String expandPath(String path)
    {
        if (path.startsWith("~"))
        {
            return System.getProperty("user.home") + path.substring(1);
        }
        return path;
    }
}
