package ru.ydn.jlll.deps;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.maven.repository.internal.MavenRepositorySystemUtils;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.impl.DefaultServiceLocator;
import org.eclipse.aether.repository.LocalRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.resolution.DependencyResult;
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory;
import org.eclipse.aether.spi.connector.transport.TransporterFactory;
import org.eclipse.aether.transport.file.FileTransporterFactory;
import org.eclipse.aether.transport.http.HttpTransporterFactory;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;
import ru.ydn.jlll.common.JlllException;

/**
 * Resolves Maven dependencies and creates classloaders for them.
 *
 * <p>
 * Uses Maven Resolver (Eclipse Aether) to download and resolve transitive dependencies from Maven
 * Central and custom repositories.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>
 * DependencyResolver resolver = new DependencyResolver();
 * resolver.addRepository("https://repo.example.com/maven2");
 * List&lt;File&gt; jars = resolver.resolve("com.google.code.gson:gson:2.10.1");
 * ClassLoader loader = resolver.createClassLoader(jars, parentClassLoader);
 * </pre>
 */
public class DependencyResolver
{
    private final RepositorySystem repoSystem;
    private final DefaultRepositorySystemSession session;
    private final List<RemoteRepository> repositories;

    /**
     * Creates a new dependency resolver with default settings.
     *
     * <p>
     * Uses {@code ~/.m2/repository} as the local cache and Maven Central as the default remote
     * repository.
     * </p>
     */
    public DependencyResolver()
    {
        this(getDefaultLocalRepoPath());
    }

    /**
     * Creates a new dependency resolver with a custom local repository path.
     *
     * @param localRepoPath
     *            path to local Maven repository cache
     */
    public DependencyResolver(String localRepoPath)
    {
        this.repoSystem = newRepositorySystem();
        this.session = newSession(repoSystem, localRepoPath);
        this.repositories = new ArrayList<>();
        // Add Maven Central as default
        repositories.add(newCentralRepository());
    }

    /**
     * Adds a custom remote repository.
     *
     * @param url
     *            repository URL (e.g., "https://repo.example.com/maven2")
     */
    public void addRepository(String url)
    {
        addRepository("custom-" + repositories.size(), url);
    }

    /**
     * Adds a custom remote repository with a specific ID.
     *
     * @param id
     *            repository identifier
     * @param url
     *            repository URL
     */
    public void addRepository(String id, String url)
    {
        repositories.add(new RemoteRepository.Builder(id, "default", url).build());
    }

    /**
     * Resolves a single Maven coordinate and its transitive dependencies.
     *
     * @param coordinate
     *            Maven coordinate in format "groupId:artifactId:version" or
     *            "groupId:artifactId:classifier:version"
     * @return list of JAR files (resolved artifacts)
     * @throws JlllException
     *             if resolution fails
     */
    public List<File> resolve(String coordinate) throws JlllException
    {
        return resolve(Collections.singletonList(coordinate));
    }

    /**
     * Resolves multiple Maven coordinates and their transitive dependencies.
     *
     * @param coordinates
     *            list of Maven coordinates
     * @return list of JAR files (all resolved artifacts, deduplicated)
     * @throws JlllException
     *             if resolution fails
     */
    public List<File> resolve(List<String> coordinates) throws JlllException
    {
        try
        {
            CollectRequest collectRequest = new CollectRequest();
            collectRequest.setRepositories(repositories);
            for (String coord : coordinates)
            {
                Artifact artifact = parseCoordinate(coord);
                collectRequest.addDependency(new Dependency(artifact, JavaScopes.RUNTIME));
            }
            DependencyFilter classpathFilter = DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME);
            DependencyRequest dependencyRequest = new DependencyRequest(collectRequest, classpathFilter);
            DependencyResult result = repoSystem.resolveDependencies(session, dependencyRequest);
            List<File> files = new ArrayList<>();
            for (ArtifactResult artifactResult : result.getArtifactResults())
            {
                files.add(artifactResult.getArtifact().getFile());
            }
            return files;
        }
        catch (DependencyResolutionException e)
        {
            throw new JlllException("Failed to resolve dependencies: " + e.getMessage(), e);
        }
    }

    /**
     * Creates a URLClassLoader for the resolved JAR files.
     *
     * @param jars
     *            list of JAR files to include in classpath
     * @param parent
     *            parent classloader
     * @return new classloader with the JARs in its classpath
     * @throws JlllException
     *             if URL conversion fails
     */
    public ClassLoader createClassLoader(List<File> jars, ClassLoader parent) throws JlllException
    {
        URL[] urls = new URL[jars.size()];
        for (int i = 0; i < jars.size(); i++)
        {
            try
            {
                urls[i] = jars.get(i).toURI().toURL();
            }
            catch (MalformedURLException e)
            {
                throw new JlllException("Invalid JAR path: " + jars.get(i), e);
            }
        }
        return new URLClassLoader(urls, parent);
    }

    /**
     * Resolves dependencies and creates a classloader in one step.
     *
     * @param coordinates
     *            list of Maven coordinates
     * @param parent
     *            parent classloader
     * @return new classloader with resolved dependencies
     * @throws JlllException
     *             if resolution fails
     */
    public ClassLoader resolveAndCreateClassLoader(List<String> coordinates, ClassLoader parent) throws JlllException
    {
        List<File> jars = resolve(coordinates);
        return createClassLoader(jars, parent);
    }

    /**
     * Parses a Maven coordinate string into an Artifact.
     *
     * <p>
     * Supported formats:
     * </p>
     * <ul>
     * <li>{@code groupId:artifactId:version}</li>
     * <li>{@code groupId:artifactId:classifier:version}</li>
     * <li>{@code groupId:artifactId:type:classifier:version}</li>
     * </ul>
     *
     * @param coordinate
     *            Maven coordinate string
     * @return Artifact object
     * @throws JlllException
     *             if format is invalid
     */
    private Artifact parseCoordinate(String coordinate) throws JlllException
    {
        try
        {
            return new DefaultArtifact(coordinate);
        }
        catch (IllegalArgumentException e)
        {
            throw new JlllException(
                    "Invalid Maven coordinate '" + coordinate + "'. Expected format: groupId:artifactId:version", e);
        }
    }

    /**
     * Returns the default local repository path (~/.m2/repository).
     */
    private static String getDefaultLocalRepoPath()
    {
        String userHome = System.getProperty("user.home");
        return userHome + File.separator + ".m2" + File.separator + "repository";
    }

    /**
     * Creates a new Maven repository system.
     */
    @SuppressWarnings("deprecation")
    private static RepositorySystem newRepositorySystem()
    {
        DefaultServiceLocator locator = MavenRepositorySystemUtils.newServiceLocator();
        locator.addService(RepositoryConnectorFactory.class, BasicRepositoryConnectorFactory.class);
        locator.addService(TransporterFactory.class, FileTransporterFactory.class);
        locator.addService(TransporterFactory.class, HttpTransporterFactory.class);
        return locator.getService(RepositorySystem.class);
    }

    /**
     * Creates a new repository session.
     */
    private static DefaultRepositorySystemSession newSession(RepositorySystem system, String localRepoPath)
    {
        DefaultRepositorySystemSession session = MavenRepositorySystemUtils.newSession();
        LocalRepository localRepo = new LocalRepository(localRepoPath);
        session.setLocalRepositoryManager(system.newLocalRepositoryManager(session, localRepo));
        return session;
    }

    /**
     * Creates the Maven Central repository reference.
     */
    private static RemoteRepository newCentralRepository()
    {
        return new RemoteRepository.Builder("central", "default", "https://repo.maven.apache.org/maven2/").build();
    }
}
