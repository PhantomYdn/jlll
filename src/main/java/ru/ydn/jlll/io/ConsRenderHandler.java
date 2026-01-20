package ru.ydn.jlll.io;

/**
 * Callback interface for rendering Cons structures as tables.
 * Implement this interface to customize table output format (HTML, CSV, text, etc.).
 */
public interface ConsRenderHandler
{
    /**
     * Determines table orientation based on dimensions.
     *
     * @param width
     *            the number of columns
     * @param height
     *            the number of rows
     * @return true for transposed (column-first) output, false for row-first
     */
    public boolean getOrientation(int width, int height);

    /**
     * Called at the start of table rendering.
     */
    public void startTable();

    /**
     * Called at the start of each row.
     */
    public void startRow();

    /**
     * Called to render a single cell value.
     *
     * @param value
     *            the cell value to render
     */
    public void renderCell(Object value);

    /**
     * Called at the end of each row.
     */
    public void finishRow();

    /**
     * Called at the end of table rendering.
     */
    public void finishTable();
}
