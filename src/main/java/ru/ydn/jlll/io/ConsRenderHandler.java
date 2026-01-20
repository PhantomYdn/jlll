package ru.ydn.jlll.io;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: 07.09.2007
 * Time: 12:34:20
 * To change this template use File | Settings | File Templates.
 */
public interface ConsRenderHandler
{
    public boolean getOrientation(int width, int height);

    public void startTable();

    public void startRow();

    public void renderCell(Object value);

    public void finishRow();

    public void finishTable();
}
