package ru.ydn.jlll.libs;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.stream.Collectors;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Keyword;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.ReflectionLibrary;
import ru.ydn.jlll.common.annotation.JlllName;
import ru.ydn.jlll.util.ListUtil;

/**
 * File and path I/O primitives.
 *
 * <p>
 * Provides file system operations:
 * </p>
 * <ul>
 * <li><b>slurp:</b> read entire file/URL/classpath resource to string</li>
 * <li><b>spit:</b> write string to file (with optional append mode)</li>
 * <li><b>open-input-file/open-output-file:</b> create file ports</li>
 * <li><b>close-input-port/close-output-port:</b> close ports</li>
 * <li><b>call-with-input-file/call-with-output-file:</b> safe resource handling</li>
 * <li><b>file-exists?/directory?:</b> file system predicates</li>
 * <li><b>delete-file/rename-file/copy-file:</b> file operations</li>
 * <li><b>path-join/path-directory/path-filename:</b> path utilities</li>
 * </ul>
 */
public class FileLib extends ReflectionLibrary
{
    /** {@inheritDoc} */
    @Override
    public void load(Environment env) throws JlllException
    {
        super.load(env);
        // spit - needs special handling for keyword arguments
        new Primitive("spit", env, "Writes content to a file. (spit \"file.txt\" \"content\") writes/overwrites. "
                + "(spit \"file.txt\" \"more\" :append true) appends to file.")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                try
                {
                    String path = values.get(0).toString();
                    String content = values.get(1).toString();
                    boolean append = false;
                    // Check for :append keyword argument
                    int len = values.length();
                    for (int i = 2; i < len - 1; i++)
                    {
                        Object key = values.get(i);
                        if (key instanceof Keyword && "append".equals(((Keyword) key).getName()))
                        {
                            Object val = values.get(i + 1);
                            append = Boolean.TRUE.equals(val);
                            break;
                        }
                    }
                    Path filePath = Paths.get(path);
                    if (append)
                    {
                        Files.writeString(filePath, content, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
                                StandardOpenOption.APPEND);
                    }
                    else
                    {
                        Files.writeString(filePath, content, StandardCharsets.UTF_8);
                    }
                    return path;
                }
                catch (IOException e)
                {
                    throw new JlllException("spit: I/O error writing to file", e);
                }
            }
        };
        // call-with-input-file - needs to execute a procedure
        new Primitive("call-with-input-file", env, "Opens file, calls proc with port, ensures port is closed. "
                + "(call-with-input-file \"file.txt\" (lambda (port) (read-line port)))")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String path = values.get(0).toString();
                Object proc = values.get(1);
                BufferedReader reader = null;
                try
                {
                    reader = new BufferedReader(new FileReader(path, StandardCharsets.UTF_8));
                    Cons args = new Cons(reader, new Cons());
                    if (proc instanceof Procedure)
                    {
                        return ((Procedure) proc).applyEvaluated(args, env);
                    }
                    else if (proc instanceof Primitive)
                    {
                        return ((Primitive) proc).applyEvaluated(args, env);
                    }
                    else
                    {
                        throw new JlllException("call-with-input-file: second argument must be a procedure");
                    }
                }
                catch (IOException e)
                {
                    throw new JlllException("call-with-input-file: I/O error", e);
                }
                finally
                {
                    if (reader != null)
                    {
                        try
                        {
                            reader.close();
                        }
                        catch (IOException e)
                        {
                            // Ignore close errors
                        }
                    }
                }
            }
        };
        // call-with-output-file - needs to execute a procedure
        new Primitive("call-with-output-file", env,
                "Opens file for writing, calls proc with port, ensures port is closed. "
                        + "(call-with-output-file \"file.txt\" (lambda (port) (display \"hello\" port)))")
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String path = values.get(0).toString();
                Object proc = values.get(1);
                BufferedWriter writer = null;
                try
                {
                    writer = new BufferedWriter(new FileWriter(path, StandardCharsets.UTF_8));
                    Cons args = new Cons(writer, new Cons());
                    Object result;
                    if (proc instanceof Procedure)
                    {
                        result = ((Procedure) proc).applyEvaluated(args, env);
                    }
                    else if (proc instanceof Primitive)
                    {
                        result = ((Primitive) proc).applyEvaluated(args, env);
                    }
                    else
                    {
                        throw new JlllException("call-with-output-file: second argument must be a procedure");
                    }
                    writer.flush();
                    return result;
                }
                catch (IOException e)
                {
                    throw new JlllException("call-with-output-file: I/O error", e);
                }
                finally
                {
                    if (writer != null)
                    {
                        try
                        {
                            writer.close();
                        }
                        catch (IOException e)
                        {
                            // Ignore close errors
                        }
                    }
                }
            }
        };
    }

    /**
     * Reads entire resource to string. Supports file paths, URLs, and classpath resources.
     * {@code (slurp "file.txt")} reads a file.
     * {@code (slurp "https://example.com")} reads from URL.
     * {@code (slurp "classpath:config.jlll")} reads from classpath.
     *
     * @param resource
     *            the resource path (file, URL, or classpath: prefixed)
     * @return the entire content as a string
     * @throws JlllException
     *             if reading fails
     */
    @JlllName("slurp")
    public String slurp(String resource) throws JlllException
    {
        try
        {
            if (resource.startsWith("classpath:"))
            {
                String path = resource.substring("classpath:".length());
                try (InputStream is = getClass().getClassLoader().getResourceAsStream(path))
                {
                    if (is == null)
                    {
                        throw new JlllException("slurp: classpath resource not found: " + path);
                    }
                    return new String(is.readAllBytes(), StandardCharsets.UTF_8);
                }
            }
            else if (resource.startsWith("http://") || resource.startsWith("https://"))
            {
                URL url = new URL(resource);
                try (InputStream is = url.openStream())
                {
                    return new String(is.readAllBytes(), StandardCharsets.UTF_8);
                }
            }
            else
            {
                return Files.readString(Paths.get(resource), StandardCharsets.UTF_8);
            }
        }
        catch (IOException e)
        {
            throw new JlllException("slurp: I/O error reading resource: " + resource, e);
        }
    }

    /**
     * Opens a file for reading and returns a BufferedReader port.
     * {@code (open-input-file "file.txt")} returns a reader.
     *
     * @param path
     *            the file path
     * @return a BufferedReader for the file
     * @throws JlllException
     *             if file cannot be opened
     */
    @JlllName("open-input-file")
    public BufferedReader openInputFile(String path) throws JlllException
    {
        try
        {
            return new BufferedReader(new FileReader(path, StandardCharsets.UTF_8));
        }
        catch (IOException e)
        {
            throw new JlllException("open-input-file: cannot open file: " + path, e);
        }
    }

    /**
     * Opens a file for writing and returns a BufferedWriter port.
     * {@code (open-output-file "file.txt")} returns a writer.
     *
     * @param path
     *            the file path
     * @return a BufferedWriter for the file
     * @throws JlllException
     *             if file cannot be opened
     */
    @JlllName("open-output-file")
    public BufferedWriter openOutputFile(String path) throws JlllException
    {
        try
        {
            return new BufferedWriter(new FileWriter(path, StandardCharsets.UTF_8));
        }
        catch (IOException e)
        {
            throw new JlllException("open-output-file: cannot open file: " + path, e);
        }
    }

    /**
     * Closes an input port (Reader).
     * {@code (close-input-port port)}.
     *
     * @param port
     *            the Reader to close
     * @return true
     * @throws JlllException
     *             if closing fails
     */
    @JlllName("close-input-port")
    public Boolean closeInputPort(Reader port) throws JlllException
    {
        try
        {
            port.close();
            return true;
        }
        catch (IOException e)
        {
            throw new JlllException("close-input-port: error closing port", e);
        }
    }

    /**
     * Closes an output port (Writer).
     * {@code (close-output-port port)}.
     *
     * @param port
     *            the Writer to close
     * @return true
     * @throws JlllException
     *             if closing fails
     */
    @JlllName("close-output-port")
    public Boolean closeOutputPort(Writer port) throws JlllException
    {
        try
        {
            port.close();
            return true;
        }
        catch (IOException e)
        {
            throw new JlllException("close-output-port: error closing port", e);
        }
    }

    /**
     * Tests if a file exists.
     * {@code (file-exists? "file.txt")} returns true or false.
     *
     * @param path
     *            the file path
     * @return true if file exists
     */
    @JlllName("file-exists?")
    public Boolean fileExists(String path)
    {
        return Files.exists(Paths.get(path));
    }

    /**
     * Tests if a path is a directory.
     * {@code (directory? "mydir")} returns true or false.
     *
     * @param path
     *            the path to test
     * @return true if path is a directory
     */
    @JlllName("directory?")
    public Boolean isDirectory(String path)
    {
        return Files.isDirectory(Paths.get(path));
    }

    /**
     * Tests if a file is readable.
     * {@code (file-readable? "file.txt")} returns true or false.
     *
     * @param path
     *            the file path
     * @return true if file is readable
     */
    @JlllName("file-readable?")
    public Boolean fileReadable(String path)
    {
        return Files.isReadable(Paths.get(path));
    }

    /**
     * Tests if a file is writable.
     * {@code (file-writable? "file.txt")} returns true or false.
     *
     * @param path
     *            the file path
     * @return true if file is writable
     */
    @JlllName("file-writable?")
    public Boolean fileWritable(String path)
    {
        return Files.isWritable(Paths.get(path));
    }

    /**
     * Deletes a file.
     * {@code (delete-file "file.txt")} deletes the file.
     *
     * @param path
     *            the file path
     * @return true if deleted successfully
     * @throws JlllException
     *             if deletion fails
     */
    @JlllName("delete-file")
    public Boolean deleteFile(String path) throws JlllException
    {
        try
        {
            return Files.deleteIfExists(Paths.get(path));
        }
        catch (IOException e)
        {
            throw new JlllException("delete-file: cannot delete file: " + path, e);
        }
    }

    /**
     * Renames/moves a file.
     * {@code (rename-file "old.txt" "new.txt")} renames the file.
     *
     * @param oldPath
     *            the source file path
     * @param newPath
     *            the target file path
     * @return the new path
     * @throws JlllException
     *             if rename fails
     */
    @JlllName("rename-file")
    public String renameFile(String oldPath, String newPath) throws JlllException
    {
        try
        {
            Files.move(Paths.get(oldPath), Paths.get(newPath));
            return newPath;
        }
        catch (IOException e)
        {
            throw new JlllException("rename-file: cannot rename file", e);
        }
    }

    /**
     * Copies a file.
     * {@code (copy-file "src.txt" "dst.txt")} copies the file.
     *
     * @param srcPath
     *            the source file path
     * @param dstPath
     *            the destination file path
     * @return the destination path
     * @throws JlllException
     *             if copy fails
     */
    @JlllName("copy-file")
    public String copyFile(String srcPath, String dstPath) throws JlllException
    {
        try
        {
            Files.copy(Paths.get(srcPath), Paths.get(dstPath));
            return dstPath;
        }
        catch (IOException e)
        {
            throw new JlllException("copy-file: cannot copy file", e);
        }
    }

    /**
     * Creates a directory.
     * {@code (make-directory "newdir")} creates the directory.
     *
     * @param path
     *            the directory path
     * @return the path
     * @throws JlllException
     *             if creation fails
     */
    @JlllName("make-directory")
    public String makeDirectory(String path) throws JlllException
    {
        try
        {
            Files.createDirectories(Paths.get(path));
            return path;
        }
        catch (IOException e)
        {
            throw new JlllException("make-directory: cannot create directory: " + path, e);
        }
    }

    /**
     * Returns the size of a file in bytes.
     * {@code (file-size "file.txt")} returns the size.
     *
     * @param path
     *            the file path
     * @return file size in bytes
     * @throws JlllException
     *             if file cannot be accessed
     */
    @JlllName("file-size")
    public Long fileSize(String path) throws JlllException
    {
        try
        {
            return Files.size(Paths.get(path));
        }
        catch (IOException e)
        {
            throw new JlllException("file-size: cannot get size of file: " + path, e);
        }
    }

    /**
     * Lists the contents of a directory.
     * {@code (directory-list "mydir")} returns a list of filenames.
     *
     * @param path
     *            the directory path
     * @return list of filenames as a Cons list
     * @throws JlllException
     *             if directory cannot be read
     */
    @JlllName("directory-list")
    public Cons directoryList(String path) throws JlllException
    {
        try
        {
            java.util.List<String> files = Files.list(Paths.get(path)).map(p -> p.getFileName().toString())
                    .collect(Collectors.toList());
            return ListUtil.listToCons(files);
        }
        catch (IOException e)
        {
            throw new JlllException("directory-list: cannot list directory: " + path, e);
        }
    }

    /**
     * Returns the current working directory.
     * {@code (current-directory)} returns the path.
     *
     * @return current directory path as string
     */
    @JlllName("current-directory")
    public String currentDirectory()
    {
        return System.getProperty("user.dir");
    }

    /**
     * Joins path components.
     * {@code (path-join "dir" "subdir" "file.txt")} returns "dir/subdir/file.txt".
     *
     * @param parts
     *            path components
     * @return joined path
     */
    @JlllName("path-join")
    public String pathJoin(String... parts)
    {
        if (parts.length == 0)
        {
            return "";
        }
        Path result = Paths.get(parts[0]);
        for (int i = 1; i < parts.length; i++)
        {
            result = result.resolve(parts[i]);
        }
        return result.toString();
    }

    /**
     * Returns the directory part of a path.
     * {@code (path-directory "/a/b/c.txt")} returns "/a/b".
     *
     * @param path
     *            the file path
     * @return parent directory path, or empty string if none
     */
    @JlllName("path-directory")
    public String pathDirectory(String path)
    {
        Path p = Paths.get(path).getParent();
        return p == null ? "" : p.toString();
    }

    /**
     * Returns the filename part of a path.
     * {@code (path-filename "/a/b/c.txt")} returns "c.txt".
     *
     * @param path
     *            the file path
     * @return filename
     */
    @JlllName("path-filename")
    public String pathFilename(String path)
    {
        Path p = Paths.get(path).getFileName();
        return p == null ? "" : p.toString();
    }

    /**
     * Returns the extension of a file.
     * {@code (path-extension "/a/b/c.txt")} returns "txt".
     *
     * @param path
     *            the file path
     * @return extension without dot, or empty string if none
     */
    @JlllName("path-extension")
    public String pathExtension(String path)
    {
        String filename = pathFilename(path);
        int dotIndex = filename.lastIndexOf('.');
        if (dotIndex > 0 && dotIndex < filename.length() - 1)
        {
            return filename.substring(dotIndex + 1);
        }
        return "";
    }
}
