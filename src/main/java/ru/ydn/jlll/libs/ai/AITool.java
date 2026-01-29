package ru.ydn.jlll.libs.ai;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import dev.langchain4j.agent.tool.ToolExecutionRequest;
import dev.langchain4j.agent.tool.ToolSpecification;
import dev.langchain4j.model.chat.request.json.JsonArraySchema;
import dev.langchain4j.model.chat.request.json.JsonBooleanSchema;
import dev.langchain4j.model.chat.request.json.JsonIntegerSchema;
import dev.langchain4j.model.chat.request.json.JsonNumberSchema;
import dev.langchain4j.model.chat.request.json.JsonObjectSchema;
import dev.langchain4j.model.chat.request.json.JsonSchemaElement;
import dev.langchain4j.model.chat.request.json.JsonStringSchema;
import ru.ydn.jlll.common.CapturingConsole;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Console;
import ru.ydn.jlll.common.Environment;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.common.Symbol;

/**
 * Wraps a JLLL procedure as an AI tool for function calling.
 *
 * <p>
 * Tools allow the LLM to invoke JLLL functions during conversation. Each tool has:
 * </p>
 * <ul>
 * <li>A unique name</li>
 * <li>A description for the LLM</li>
 * <li>Parameter specifications (name, type, description)</li>
 * <li>A JLLL procedure to execute</li>
 * </ul>
 *
 * <p>
 * When the LLM decides to call a tool, the parameters are extracted from JSON,
 * converted to JLLL values, and passed to the procedure.
 * </p>
 */
public class AITool implements Serializable
{
    private static final long serialVersionUID = 1L;
    private static final Gson GSON = new Gson();
    /** Tool name (used by LLM to reference the tool) */
    private final String name;
    /** Tool description (helps LLM understand when to use it) */
    private final String description;
    /** Parameter specifications */
    private final List<ToolParameter> parameters;
    /** The JLLL procedure to execute */
    private final Procedure procedure;
    /** The environment for procedure execution */
    private transient Environment environment;

    /**
     * Creates a new AI tool.
     *
     * @param name
     *            the tool name
     * @param description
     *            description of what the tool does
     * @param parameters
     *            list of parameter specifications
     * @param procedure
     *            the JLLL procedure to execute
     * @param environment
     *            the environment for execution
     */
    public AITool(String name, String description, List<ToolParameter> parameters, Procedure procedure,
            Environment environment)
    {
        this.name = name;
        this.description = description;
        this.parameters = parameters != null ? new ArrayList<>(parameters) : new ArrayList<>();
        this.procedure = procedure;
        this.environment = environment;
    }

    /**
     * Represents a tool parameter specification.
     */
    public static class ToolParameter implements Serializable
    {
        private static final long serialVersionUID = 1L;
        private final String name;
        private final String type; // "string", "integer", "number", "boolean", "array", "object"
        private final String description;
        private final boolean required;

        public ToolParameter(String name, String type, String description, boolean required)
        {
            this.name = name;
            this.type = type != null ? type : "string";
            this.description = description != null ? description : "";
            this.required = required;
        }

        public String getName()
        {
            return name;
        }

        public String getType()
        {
            return type;
        }

        public String getDescription()
        {
            return description;
        }

        public boolean isRequired()
        {
            return required;
        }
    }

    /**
     * Returns the tool name.
     *
     * @return the tool name
     */
    public String getName()
    {
        return name;
    }

    /**
     * Returns the tool description.
     *
     * @return the tool description
     */
    public String getDescription()
    {
        return description;
    }

    /**
     * Returns the parameter specifications.
     *
     * @return list of parameters
     */
    public List<ToolParameter> getParameters()
    {
        return new ArrayList<>(parameters);
    }

    /**
     * Returns the JLLL procedure.
     *
     * @return the procedure
     */
    public Procedure getProcedure()
    {
        return procedure;
    }

    /**
     * Returns the environment for execution.
     *
     * @return the environment
     */
    public Environment getEnvironment()
    {
        return environment;
    }

    /**
     * Sets the environment for execution.
     *
     * @param environment
     *            the environment
     */
    public void setEnvironment(Environment environment)
    {
        this.environment = environment;
    }

    /**
     * Builds a LangChain4j ToolSpecification for this tool.
     *
     * @return the tool specification
     */
    public ToolSpecification getSpecification()
    {
        ToolSpecification.Builder builder = ToolSpecification.builder().name(name).description(description);
        // Build JSON schema for parameters
        if (!parameters.isEmpty())
        {
            Map<String, JsonSchemaElement> properties = new LinkedHashMap<>();
            List<String> required = new ArrayList<>();
            for (ToolParameter param : parameters)
            {
                JsonSchemaElement propSchema = jsonTypeToSchema(param.getType(), param.getDescription());
                properties.put(param.getName(), propSchema);
                if (param.isRequired())
                {
                    required.add(param.getName());
                }
            }
            JsonObjectSchema paramsSchema = JsonObjectSchema.builder().addProperties(properties).required(required)
                    .build();
            builder.parameters(paramsSchema);
        }
        return builder.build();
    }

    /**
     * Converts JSON type string to JsonSchemaElement for LangChain4j.
     */
    private JsonSchemaElement jsonTypeToSchema(String jsonType, String description)
    {
        return switch (jsonType.toLowerCase())
        {
            case "string" -> JsonStringSchema.builder().description(description).build();
            case "integer" -> JsonIntegerSchema.builder().description(description).build();
            case "number" -> JsonNumberSchema.builder().description(description).build();
            case "boolean" -> JsonBooleanSchema.builder().description(description).build();
            case "array" ->
                JsonArraySchema.builder().items(JsonStringSchema.builder().build()).description(description).build();
            default -> JsonStringSchema.builder().description(description).build();
        };
    }

    /**
     * Executes the tool with the given arguments from LLM.
     *
     * @param request
     *            the tool execution request from LangChain4j
     * @return the result as a string (for sending back to LLM)
     */
    public String execute(ToolExecutionRequest request)
    {
        return execute(request.arguments());
    }

    /**
     * Executes the tool with raw JSON arguments string.
     * Captures any printed output and includes it in the response to the LLM.
     *
     * @param argumentsJson
     *            JSON string of arguments
     * @return the result as a string, including any captured output
     */
    public String execute(String argumentsJson)
    {
        // Save current console and install capturing console
        Object originalConsole = environment.lookup(Symbol.CONSOLE);
        Console parentConsole = originalConsole instanceof Console ? (Console) originalConsole : null;
        CapturingConsole capturingConsole = new CapturingConsole(parentConsole, false);
        environment.setBinding(Symbol.CONSOLE, capturingConsole);
        try
        {
            // Parse arguments from JSON
            JsonObject argsObj = GSON.fromJson(argumentsJson, JsonObject.class);
            // Convert JSON arguments to JLLL values
            List<Object> jlllArgs = new ArrayList<>();
            for (ToolParameter param : parameters)
            {
                JsonElement element = argsObj.get(param.getName());
                Object value = jsonElementToJlll(element);
                jlllArgs.add(value);
            }
            // Execute the JLLL procedure
            Cons argsCons = Cons.list(jlllArgs.toArray());
            Object result = procedure.applyEvaluated(argsCons, environment);
            // Build response with captured output and result
            return buildResponse(capturingConsole.getCapturedOutput(), result);
        }
        catch (JlllException e)
        {
            String output = capturingConsole.getCapturedOutput();
            if (!output.isEmpty())
            {
                return "Output:\n" + output + "\nError: " + e.getMessage();
            }
            return "Error: " + e.getMessage();
        }
        catch (Exception e)
        {
            String output = capturingConsole.getCapturedOutput();
            if (!output.isEmpty())
            {
                return "Output:\n" + output + "\nError executing tool: " + e.getMessage();
            }
            return "Error executing tool: " + e.getMessage();
        }
        finally
        {
            // Restore original console
            if (originalConsole != null)
            {
                environment.setBinding(Symbol.CONSOLE, originalConsole);
            }
        }
    }

    /**
     * Builds the response string for the LLM, combining captured output and result.
     *
     * @param output
     *            the captured console output (may be empty)
     * @param result
     *            the execution result
     * @return formatted response string
     */
    private String buildResponse(String output, Object result)
    {
        StringBuilder response = new StringBuilder();
        // Include captured output if any
        if (output != null && !output.isEmpty())
        {
            response.append("Output:\n").append(output);
            if (!output.endsWith("\n"))
            {
                response.append("\n");
            }
        }
        // Include result (skip if null/nil since output was the point)
        if (result != null && !Null.NULL.equals(result))
        {
            if (response.length() > 0)
            {
                response.append("Result: ");
            }
            response.append(resultToString(result));
        }
        else if (response.length() == 0)
        {
            // No output and null result - indicate success
            response.append("Done (no output)");
        }
        return response.toString();
    }

    /**
     * Converts a JSON element to a JLLL value.
     */
    private Object jsonElementToJlll(JsonElement element)
    {
        if (element == null || element.isJsonNull())
        {
            return null;
        }
        if (element.isJsonPrimitive())
        {
            JsonPrimitive prim = element.getAsJsonPrimitive();
            if (prim.isString())
            {
                return prim.getAsString();
            }
            if (prim.isNumber())
            {
                // Try to preserve integer type
                Number num = prim.getAsNumber();
                if (num.doubleValue() == num.longValue())
                {
                    long l = num.longValue();
                    if (l >= Integer.MIN_VALUE && l <= Integer.MAX_VALUE)
                    {
                        return (int) l;
                    }
                    return l;
                }
                return num.doubleValue();
            }
            if (prim.isBoolean())
            {
                return prim.getAsBoolean();
            }
        }
        if (element.isJsonArray())
        {
            List<Object> list = new ArrayList<>();
            for (JsonElement item : element.getAsJsonArray())
            {
                list.add(jsonElementToJlll(item));
            }
            return Cons.list(list.toArray());
        }
        if (element.isJsonObject())
        {
            // Convert to JLLL hash map (LinkedHashMap)
            Map<Object, Object> map = new LinkedHashMap<>();
            for (Map.Entry<String, JsonElement> entry : element.getAsJsonObject().entrySet())
            {
                // Use keywords for keys
                map.put(Symbol.intern(entry.getKey()), jsonElementToJlll(entry.getValue()));
            }
            return map;
        }
        return element.toString();
    }

    /**
     * Converts a JLLL result to a string for the LLM.
     */
    private String resultToString(Object result)
    {
        if (result == null)
        {
            return "null";
        }
        if (result instanceof String)
        {
            return (String) result;
        }
        // Use toString for JLLL values (Cons, Symbol, etc. have proper toString)
        return result.toString();
    }

    @Override
    public String toString()
    {
        return "AITool[name=" + name + ", params=" + parameters.size() + "]";
    }
    // ========== Factory Methods ==========

    /**
     * Creates the built-in eval tool that executes JLLL code.
     *
     * @param environment
     *            the environment for evaluation
     * @return the eval tool
     */
    public static AITool createEvalTool(Environment environment)
    {
        List<ToolParameter> params = List.of(new ToolParameter("code", "string", "JLLL code to evaluate", true));
        // Create a procedure that evaluates JLLL code
        Procedure evalProc = new Procedure()
        {
            private static final long serialVersionUID = 1L;

            @Override
            public Object applyEvaluated(Cons values, Environment env) throws JlllException
            {
                String code = (String) values.get(0);
                return Jlll.eval(code, env);
            }

            @Override
            public String describe()
            {
                return "Evaluates JLLL code";
            }
        };
        return new AITool("eval",
                "Evaluate JLLL code and return the result. "
                        + "Use this to perform calculations, manipulate data, or interact with the JLLL environment. "
                        + "The code is executed in the current session's environment.",
                params, evalProc, environment);
    }

    /**
     * Creates a tool from JLLL specification.
     *
     * @param name
     *            tool name
     * @param description
     *            tool description
     * @param paramSpecs
     *            list of parameter specs as (name type description required)
     * @param procedure
     *            the JLLL procedure
     * @param environment
     *            the environment
     * @return the new tool
     */
    public static AITool fromSpec(String name, String description, Cons paramSpecs, Procedure procedure,
            Environment environment)
    {
        List<ToolParameter> params = new ArrayList<>();
        if (paramSpecs != null && !paramSpecs.isNull())
        {
            for (Object spec : paramSpecs)
            {
                if (spec instanceof Cons specCons)
                {
                    String paramName = specCons.get(0).toString();
                    String type = specCons.length() > 1 ? specCons.get(1).toString() : "string";
                    String desc = specCons.length() > 2 ? specCons.get(2).toString() : "";
                    boolean required = specCons.length() > 3 ? Boolean.TRUE.equals(specCons.get(3)) : true;
                    params.add(new ToolParameter(paramName, type, desc, required));
                }
            }
        }
        return new AITool(name, description, params, procedure, environment);
    }
}
