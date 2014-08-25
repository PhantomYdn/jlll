package ru.ydn.jlll.io;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Null;

/**
 * Created by IntelliJ IDEA.
 * User: naryzhny
 * Date: 07.09.2007
 * Time: 12:43:59
 * To change this template use File | Settings | File Templates.
 */
public class ConsRenderer
{

    public void render(Object content, ConsRenderHandler renderHandler)
    {
        if(content instanceof Cons)
        {
            Cons cons = (Cons) content;
            boolean oneRow=false;
            for (Object obj : cons)
            {
                if(!(obj instanceof Cons))
                {
                    oneRow=true;
                    break;
                }
            }
            if(oneRow)
            {
                cons = new Cons(cons);
            }
            int x=0;
            int y=0;
            for (Object rowObj : cons)
            {
                Cons row = (Cons)rowObj;
                x++;
                y = Math.max(y, row.length());
            }

            boolean orientation = renderHandler.getOrientation(x,y);
            renderHandler.startTable();
            if(!orientation)
            {
                for (Object rowObj : cons)
                {
                    Cons row = (Cons)rowObj;
                    renderHandler.startRow();
                    for (Object obj : row)
                    {
                        renderHandler.renderCell(obj);
                    }
                    renderHandler.finishRow();
                }
            }
            else
            {
                Object[][] table = new Object[x][y];
//                System.out.println("x,y="+x+", "+y);
                int i=0;
                int j=0;
                for (Object rowCons : cons)
                {
                    Cons row = (Cons)rowCons;
                    j=0;
                    for (Object o : row)
                    {
                        if(o == null) o = Null.NULL;
                        table[i][j++] = o;
                    }
                    i++;
                }
                for(j=0;j<y;j++)
                {
                    renderHandler.startRow();
                    for(i=0;i<x;i++)
                    {
                        Object o = table[i][j];
                        if(o==null) break;
                        renderHandler.renderCell(o);
                    }
                    renderHandler.finishRow();
                }
            }
            renderHandler.finishTable();
        }
        else
        {
            renderHandler.startTable();
            renderHandler.startRow();
            renderHandler.renderCell(content);
            renderHandler.finishRow();
            renderHandler.finishTable();
        }
    }

}
