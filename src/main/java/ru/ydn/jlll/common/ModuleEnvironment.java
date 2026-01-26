package ru.ydn.jlll.common;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Environment for module definitions with export control.
 *
 * <p>
 * A ModuleEnvironment extends Environment to support module-specific features:
 * </p>
 * <ul>
 * <li><b>Export control:</b> Only explicitly exported symbols are visible to importers</li>
 * <li><b>Export-all mode:</b> Optionally export all local bindings</li>
 * <li><b>Load tracking:</b> Supports partial loading for circular dependency handling</li>
 * </ul>
 *
 * <p>
 * Modules are registered in a global registry (see {@link Environment#registerModule})
 * and can be accessed by name for qualified symbol resolution.
 * </p>
 *
 * @see Environment
 */
public class ModuleEnvironment extends Environment implements Serializable
{
    private static final long serialVersionUID = 1L;
    private final String moduleName;
    private final Set<Symbol> exports = new HashSet<>();
    private boolean exportAll = false;
    private boolean loaded = false;

    /**
     * Creates a new module environment.
     *
     * @param name
     *            the module name (used for qualified access like module/symbol)
     * @param parent
     *            the parent environment (typically the top-level environment)
     */
    public ModuleEnvironment(String name, Environment parent)
    {
        super(parent);
        this.moduleName = name;
    }

    /**
     * Returns the module name.
     *
     * @return the module name
     */
    public String getModuleName()
    {
        return moduleName;
    }

    /**
     * Marks a symbol for export from this module.
     *
     * @param sym
     *            the symbol to export
     */
    public void export(Symbol sym)
    {
        exports.add(sym);
    }

    /**
     * Enables export-all mode where all local bindings are exported.
     */
    public void exportAll()
    {
        exportAll = true;
    }

    /**
     * Checks if export-all mode is enabled.
     *
     * @return true if all bindings are exported
     */
    public boolean isExportAll()
    {
        return exportAll;
    }

    /**
     * Checks if a symbol is exported from this module.
     *
     * @param sym
     *            the symbol to check
     * @return true if the symbol is exported (either explicitly or via export-all)
     */
    public boolean isExported(Symbol sym)
    {
        return exportAll || exports.contains(sym);
    }

    /**
     * Returns the set of exported symbol names.
     *
     * <p>
     * If export-all mode is enabled, returns all local binding symbols.
     * Otherwise, returns only explicitly exported symbols.
     * </p>
     *
     * @return set of exported symbols
     */
    public Set<Symbol> getExportedSymbols()
    {
        if (exportAll)
        {
            return new HashSet<>(current.keySet());
        }
        return new HashSet<>(exports);
    }

    /**
     * Returns a map of exported bindings (symbol to value).
     *
     * <p>
     * Only includes symbols that are both exported and have a value bound
     * in this module's local environment (not inherited from parent).
     * </p>
     *
     * @return map of exported symbol to value pairs
     */
    public Map<Symbol, Object> getExports()
    {
        Map<Symbol, Object> result = new HashMap<>();
        for (Symbol sym : getExportedSymbols())
        {
            Object val = current.get(sym);
            if (val != null)
            {
                result.put(sym, val);
            }
        }
        return result;
    }

    /**
     * Returns all metadata for exported bindings.
     *
     * @return map of symbol to metadata maps for exported symbols
     */
    public Map<Symbol, Map<Symbol, Object>> getExportedMetadata()
    {
        Map<Symbol, Map<Symbol, Object>> result = new HashMap<>();
        for (Symbol sym : getExportedSymbols())
        {
            if (current.containsKey(sym))
            {
                Map<Symbol, Object> meta = getAllMeta(sym);
                if (!meta.isEmpty())
                {
                    result.put(sym, meta);
                }
            }
        }
        return result;
    }

    /**
     * Marks this module as fully loaded.
     *
     * <p>
     * Used for cycle detection: a module is registered when its definition
     * starts, but marked loaded only after all its body forms are evaluated.
     * </p>
     *
     * @param loaded
     *            true if module loading is complete
     */
    public void setLoaded(boolean loaded)
    {
        this.loaded = loaded;
    }

    /**
     * Checks if this module is fully loaded.
     *
     * @return true if module loading is complete
     */
    public boolean isLoaded()
    {
        return loaded;
    }

    @Override
    public String toString()
    {
        return "#<module:" + moduleName + (loaded ? "" : " (loading)") + ">";
    }
}
