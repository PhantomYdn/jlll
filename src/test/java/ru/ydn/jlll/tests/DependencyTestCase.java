package ru.ydn.jlll.tests;

import static org.junit.Assert.*;
import java.io.File;
import java.util.List;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.ModuleEnvironment;
import ru.ydn.jlll.deps.DependencyResolver;

/**
 * Tests for the dynamic classpath and dependency loading feature (Section 8).
 *
 * <p>
 * Tests cover:
 * </p>
 * <ul>
 * <li>DependencyResolver for Maven dependency resolution</li>
 * <li>Environment classloader support</li>
 * <li>(env :depends ...) for loading dependencies in child environments</li>
 * <li>(eval :env ...) for evaluating in specific environments</li>
 * <li>(module ... :depends ...) for modules with dependencies</li>
 * <li>env-switch!, env-classpath, env-parent primitives</li>
 * </ul>
 *
 * <p>
 * Note: Some tests require network access to download dependencies from Maven Central.
 * </p>
 */
public class DependencyTestCase
{
    private Environment env;

    @Before
    public void setUp()
    {
        env = new Environment(Environment.top);
    }

    @After
    public void tearDown()
    {
        // Clean up any modules created during tests
        Environment.removeModule("json-utils");
        Environment.removeModule("deps-test");
    }
    // ============== DependencyResolver Tests ==============

    @Test
    public void testDependencyResolverBasic() throws Exception
    {
        // Test resolving a simple, well-known dependency
        DependencyResolver resolver = new DependencyResolver();
        List<File> jars = resolver.resolve("org.slf4j:slf4j-api:2.0.9");
        assertNotNull("Should return list of JARs", jars);
        assertFalse("Should have at least one JAR", jars.isEmpty());
        // Verify the JAR file exists
        File jar = jars.get(0);
        assertTrue("JAR file should exist: " + jar, jar.exists());
        assertTrue("Should be a JAR file", jar.getName().endsWith(".jar"));
    }

    @Test
    public void testDependencyResolverTransitive() throws Exception
    {
        // Test resolving a dependency with transitive dependencies
        DependencyResolver resolver = new DependencyResolver();
        // Gson has minimal transitive deps, good for testing
        List<File> jars = resolver.resolve("com.google.code.gson:gson:2.10.1");
        assertNotNull(jars);
        assertFalse(jars.isEmpty());
    }

    @Test(expected = JlllException.class)
    public void testDependencyResolverInvalidCoordinate() throws Exception
    {
        DependencyResolver resolver = new DependencyResolver();
        resolver.resolve("invalid-coordinate");
    }

    @Test(expected = JlllException.class)
    public void testDependencyResolverNonexistent() throws Exception
    {
        DependencyResolver resolver = new DependencyResolver();
        resolver.resolve("nonexistent.group:nonexistent.artifact:99.99.99");
    }

    @Test
    public void testCreateClassLoader() throws Exception
    {
        DependencyResolver resolver = new DependencyResolver();
        List<File> jars = resolver.resolve("com.google.code.gson:gson:2.10.1");
        ClassLoader loader = resolver.createClassLoader(jars, getClass().getClassLoader());
        assertNotNull("Should create classloader", loader);
        // Verify we can load Gson class
        Class<?> gsonClass = loader.loadClass("com.google.gson.Gson");
        assertNotNull("Should load Gson class", gsonClass);
        assertEquals("com.google.gson.Gson", gsonClass.getName());
    }
    // ============== Environment Classloader Tests ==============

    @Test
    public void testEnvironmentClassloaderInheritance() throws Exception
    {
        // Create parent env with custom classloader
        DependencyResolver resolver = new DependencyResolver();
        List<File> jars = resolver.resolve("com.google.code.gson:gson:2.10.1");
        ClassLoader gsonLoader = resolver.createClassLoader(jars, getClass().getClassLoader());
        Environment parent = new Environment(Environment.top);
        parent.setClassLoader(gsonLoader, jars);
        // Child should inherit classloader
        Environment child = new Environment(parent);
        assertSame("Child should inherit parent's classloader", gsonLoader, child.getClassLoader());
        // Child can load Gson
        Class<?> gsonClass = child.loadClass("com.google.gson.Gson");
        assertNotNull(gsonClass);
    }

    @Test
    public void testEnvironmentClassloaderOverride() throws Exception
    {
        // Parent has one classloader
        Environment parent = new Environment(Environment.top);
        ClassLoader parentLoader = new ClassLoader(getClass().getClassLoader())
        {
        };
        parent.setClassLoader(parentLoader);
        // Child has different classloader
        Environment child = new Environment(parent);
        ClassLoader childLoader = new ClassLoader(getClass().getClassLoader())
        {
        };
        child.setClassLoader(childLoader);
        assertSame("Child should use its own classloader", childLoader, child.getClassLoader());
        assertSame("Parent should keep its classloader", parentLoader, parent.getClassLoader());
    }
    // ============== env :depends Tests ==============

    @Test
    public void testEnvDependsWithBody() throws Exception
    {
        // Execute code in child environment with Gson dependency
        // Test with a list to ensure JSON array output
        Object result = Jlll.eval("(env :depends '(\"com.google.code.gson:gson:2.10.1\") "
                + "(define Gson (class \"com.google.gson.Gson\")) " + "(define gson (new Gson)) "
                + "(invoke gson \"toJson\" \"hello\"))", env);
        // Gson.toJson("hello") returns the string "hello" (with quotes in JSON)
        assertEquals("\"hello\"", result);
    }

    @Test
    public void testEnvDependsReturnsEnv() throws Exception
    {
        // Without body, should return the environment object
        Object result = Jlll.eval("(env :depends '(\"com.google.code.gson:gson:2.10.1\"))", env);
        assertTrue("Should return Environment", result instanceof Environment);
        Environment childEnv = (Environment) result;
        assertTrue("Child env should have classloader", childEnv.hasOwnClassLoader());
        assertFalse("Child env should have classpath JARs", childEnv.getClasspathJars().isEmpty());
    }

    @Test
    public void testEnvDependsClassIsolation() throws Exception
    {
        // Verify that joda-time class is NOT available in parent env
        // (Using joda-time as Gson is now part of base classpath for JSON support)
        try
        {
            env.loadClass("org.joda.time.DateTime");
            fail("joda-time should not be loadable in parent environment");
        }
        catch (ClassNotFoundException expected)
        {
            // Expected
        }
        // But IS available in child env with dependency
        Object childEnv = Jlll.eval("(env :depends '(\"joda-time:joda-time:2.12.7\"))", env);
        assertTrue(childEnv instanceof Environment);
        Class<?> jodaClass = ((Environment) childEnv).loadClass("org.joda.time.DateTime");
        assertNotNull(jodaClass);
    }
    // ============== eval :env Tests ==============

    @Test
    public void testEvalWithEnv() throws Exception
    {
        // Create child environment with dependency
        Jlll.eval("(define json-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        // Evaluate expression in that environment
        Object result = Jlll.eval("(eval :env json-env '(class \"com.google.gson.Gson\"))", env);
        assertTrue("Should return Class", result instanceof Class);
        assertEquals("com.google.gson.Gson", ((Class<?>) result).getName());
    }

    @Test
    public void testEvalWithEnvDefineBindings() throws Exception
    {
        // Create child environment
        Jlll.eval("(define child-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        // Define something in child env
        Jlll.eval("(eval :env child-env '(define my-gson (new (class \"com.google.gson.Gson\"))))", env);
        // Verify it's not in parent
        assertNull("my-gson should not be in parent", env.lookup("my-gson"));
        // But can be accessed through child env
        Object gson = Jlll.eval("(eval :env child-env 'my-gson)", env);
        assertNotNull("my-gson should be accessible in child env", gson);
    }
    // ============== module :depends Tests ==============

    @Test
    public void testModuleWithDependencies() throws Exception
    {
        // Define module with Gson dependency
        // NOTE: Due to how JLLL procedures work, we test the module's classloader
        // functionality by verifying we can load classes within the module context
        Jlll.eval("(module json-utils " + ":depends '(\"com.google.code.gson:gson:2.10.1\") "
                + "(export get-gson-class-name) " + "(define GsonClass (class \"com.google.gson.Gson\")) "
                + "(define (get-gson-class-name) \"com.google.gson.Gson\"))", env);
        ModuleEnvironment mod = Environment.getModule("json-utils");
        assertNotNull("Module should be registered", mod);
        assertTrue("Module should have classloader", mod.hasOwnClassLoader());
        assertFalse("Module should have classpath JARs", mod.getClasspathJars().isEmpty());
        // Verify the class was loaded in the module (it's bound to GsonClass)
        Object gsonClass = mod.lookup("GsonClass");
        assertNotNull("GsonClass should be bound in module", gsonClass);
        assertTrue("GsonClass should be a Class", gsonClass instanceof Class);
        assertEquals("com.google.gson.Gson", ((Class<?>) gsonClass).getName());
        // Verify module has the dependency JAR in its classpath
        List<File> jars = mod.getClasspathJars();
        boolean hasGson = false;
        for (File jar : jars)
        {
            if (jar.getName().contains("gson"))
            {
                hasGson = true;
                break;
            }
        }
        assertTrue("Module should have Gson JAR in classpath", hasGson);
    }

    @Test(expected = JlllException.class)
    public void testModuleWithDepsCannotBeRedefined() throws Exception
    {
        // Define module with dependencies
        Jlll.eval("(module deps-test :depends '(\"com.google.code.gson:gson:2.10.1\") (define x 1))", env);
        // Attempt to redefine should fail
        Jlll.eval("(module deps-test (define x 2))", env);
    }
    // ============== env-switch!, env-classpath, env-parent Tests ==============

    @Test
    public void testEnvClasspath() throws Exception
    {
        // Create environment with dependency
        Jlll.eval("(define test-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        // Get classpath
        Object classpath = Jlll.eval("(env-classpath test-env)", env);
        assertTrue("Should return a list", classpath instanceof Cons);
        Cons pathList = (Cons) classpath;
        assertTrue("Should have at least one JAR", pathList.length() > 0);
        String firstJar = (String) pathList.get(0);
        assertTrue("Should be a JAR file", firstJar.endsWith(".jar"));
        assertTrue("JAR should exist", new File(firstJar).exists());
    }

    @Test
    public void testEnvClasspathEmpty() throws Exception
    {
        // Current env without custom classpath
        Object classpath = Jlll.eval("(env-classpath)", env);
        // Should return null (no custom classpath) - Null.NULL in JLLL
        assertTrue("Should return null for env without classpath",
                classpath == null || classpath instanceof ru.ydn.jlll.common.Null);
    }

    @Test
    public void testEnvParent() throws Exception
    {
        // Create child environment
        Jlll.eval("(define test-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        // Get parent
        Object parent = Jlll.eval("(env-parent test-env)", env);
        assertTrue("Parent should be an Environment", parent instanceof Environment);
        assertSame("Parent should be current env", env, parent);
    }

    @Test
    public void testEnvPredicate() throws Exception
    {
        Jlll.eval("(define test-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        assertTrue("env should be recognized", (Boolean) Jlll.eval("(env? test-env)", env));
        assertFalse("number should not be env", (Boolean) Jlll.eval("(env? 42)", env));
        assertFalse("string should not be env", (Boolean) Jlll.eval("(env? \"hello\")", env));
    }

    @Test
    public void testEnvSwitch() throws Exception
    {
        // Create child environment with a binding
        Jlll.eval("(define test-env (env :depends '(\"com.google.code.gson:gson:2.10.1\")))", env);
        Jlll.eval("(eval :env test-env '(define child-var 42))", env);
        // Switch to child env
        Object result = Jlll.eval("(env-switch! test-env)", env);
        assertTrue("Should return the switched-to environment", result instanceof Environment);
        // Verify *current-env* was set
        Object currentEnv = Environment.top.lookup("*current-env*");
        assertNotNull("*current-env* should be set", currentEnv);
        assertTrue("*current-env* should be an Environment", currentEnv instanceof Environment);
    }
}
