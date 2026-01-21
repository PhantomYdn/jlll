package ru.ydn.jlll.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static ru.ydn.jlll.common.Jlll.eval;
import static ru.ydn.jlll.common.Jlll.prepare;
import java.sql.DriverManager;
import java.sql.SQLException;
import org.junit.Before;
import org.junit.Test;
import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Primitive;

public class SQLLibTestCase
{
    private Enviroment env = null;

    @Before
    public void setUp() throws Exception
    {
        env = new Enviroment(Enviroment.top);
        Class.forName(System.getProperty("dbdriver"));
        new Primitive("sql-get-connection", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = -2592105929018393189L;

            public Object applyEvaluated(Cons values, Enviroment env) throws JlllException
            {
                try
                {
                    String url = System.getProperty("dburl");
                    return DriverManager.getConnection(url);
                }
                catch (SQLException e)
                {
                    throw new JlllException("SQLException", e);
                }
            }
        };
    }

    @Test
    public void testInit() throws Exception
    {
        boolean catched = false;
        try
        {
            eval("(sql-execute \"select now()\")");
        }
        catch (JlllException exc)
        {
            catched = true;
        }
        assertTrue(catched);
        new Primitive("sql-get-connection", env)
        {
            /**
             *
             */
            private static final long serialVersionUID = 3226129210267326830L;

            public Object applyEvaluated(Cons values, Enviroment env) throws JlllException
            {
                try
                {
                    String url = System.getProperty("dburl");
                    return DriverManager.getConnection(url);
                }
                catch (SQLException e)
                {
                    throw new JlllException("SQLException", e);
                }
            }
        };
        eval("(sql-execute \"select now()\")", env);
    }

    @Test
    public void testCreateTable() throws Exception
    {
        try
        {
            eval("(sql-execute \"drop table jlll_testcase\") ", env);
        }
        catch (JlllException e)
        {
        }
        eval("(sql-execute \"create table jlll_testcase(id int4, name text)\") ", env);
    }

    @Test
    public void testInsert() throws Exception
    {
        eval("(sql-execute \"insert into jlll_testcase (id, name) values (1, 'aaa')\")", env);
        eval("(sql-execute \"insert into jlll_testcase (id, name) values (2, 'bbb')\")", env);
        eval("(sql-execute \"insert into jlll_testcase (id, name) values (3, 'ccc')\")", env);
        eval("(sql-execute \"insert into jlll_testcase (id, name) values (4, 'ddd')\")", env);
        Object ret = eval("(caar (sql-execute \"select count(1) from jlll_testcase\"))", env);
        assertEquals(4, ret);
        eval("(sql-execute-ps \"insert into jlll_testcase (id, name) values (?, ?)\" 5 \"eee\")", env);
        ret = eval("(caar (sql-execute \"select count(1) from jlll_testcase\"))", env);
        assertEquals(5, ret);
        eval("(define p6 6)" + "(define name6 \"fff\")"
                + "(sql-execute-mps \"insert into jlll_testcase (id, name) values ($p6$, $name6$)\")", env);
        ret = eval("(caar (sql-execute \"select count(1) from jlll_testcase\"))", env);
        assertEquals(6, ret);
    }

    @Test
    public void testSelect() throws Exception
    {
        Object exp = prepare("((1 \"aaa\") (2 \"bbb\") (3 \"ccc\") (4 \"ddd\") (5 \"eee\") (6 \"fff\"))");
        Object ret = eval("(sql-execute \"select id, name from jlll_testcase order by id\")", env);
        assertEquals(exp, ret);
    }

    @Test
    public void testPS() throws Exception
    {
        assertEquals(1, eval("(caar (sql-execute-ps \"select id from jlll_testcase where name=? \" \"aaa\"))", env));
        assertEquals("bbb", eval("(caar (sql-execute-ps \"select name from jlll_testcase where id=? \" 2))", env));
    }

    @Test
    public void testMPS() throws Exception
    {
        eval("(define param.name1 \"aaa\") ", env);
        eval("(define id1 2) ", env);
        assertEquals(1,
                eval("(caar (sql-execute-mps \"select id from jlll_testcase where name=$param.name1$ \"))", env));
        assertEquals("bbb", eval("(caar (sql-execute-mps \"select name from jlll_testcase where id=$id1$ \"))", env));
    }
}
