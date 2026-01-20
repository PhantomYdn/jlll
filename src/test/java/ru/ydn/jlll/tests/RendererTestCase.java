package ru.ydn.jlll.tests;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: 07.09.2007
 * Time: 14:02:35
 * To change this template use File | Settings | File Templates.
 */
import static org.junit.Assert.*;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Jlll;
import ru.ydn.jlll.io.ConsRenderHandler;
import ru.ydn.jlll.io.ConsRenderer;

public class RendererTestCase
{
    private ConsRenderer renderer = new ConsRenderer();

    private static class SimpleRenderHandler implements ConsRenderHandler
    {
        private StringBuffer content = new StringBuffer();
        public boolean orientation;

        public SimpleRenderHandler(boolean orientation)
        {
            this.orientation = orientation;
        }

        public String getContent()
        {
            return content.toString();
        }

        public boolean getOrientation(int width, int height)
        {
            return orientation;
        }

        public void startTable()
        {
            content = new StringBuffer();
            content.append("T");
        }

        public void startRow()
        {
            content.append("R");
        }

        public void renderCell(Object value)
        {
            content.append(value);
        }

        public void finishRow()
        {
            content.append("r");
        }

        public void finishTable()
        {
            content.append("t");
        }
    }

    @Test
    public void testRendererOneObject() throws Exception
    {
        assertEquals("TRTESTrt", render("TEST", true));
        assertEquals("TRTESTrt", render("TEST", false));
    }

    @Test
    public void testRendererOrientation() throws Exception
    {
        Object obj = Jlll.prepare("(a b)");
        assertEquals("TRabrt", render(obj, false));
        assertEquals("TRarRbrt", render(obj, true));
        Object obj2 = new Cons(obj);
        assertEquals("TRabrt", render(obj2, false));
        assertEquals("TRarRbrt", render(obj2, true));
    }

    @Test
    public void testRenderTable() throws Exception
    {
        Object obj = Jlll.prepare("((a b) (c d))");
        assertEquals("TRabrRcdrt", render(obj, false));
        assertEquals("TRacrRbdrt", render(obj, true));
    }

    private String render(Object val, boolean orientation) throws Exception
    {
        SimpleRenderHandler renderHandler = new SimpleRenderHandler(orientation);
        renderer.render(val, renderHandler);
        return renderHandler.getContent();
    }
}
