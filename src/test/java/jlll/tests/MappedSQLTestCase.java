package jlll.tests;

import static org.junit.Assert.assertEquals;
import jlll.libs.SQLLib;

import org.junit.Test;

public class MappedSQLTestCase
{
	@Test public void testPS() throws Exception
	{
		String sql = "select * from table where name<>$param.name$ and age>$age$";
		String expected = "select * from table where name<>? and age>?";
		SQLLib.MappedSQL mappedSql = new SQLLib.MappedSQL(sql);
		assertEquals(expected, mappedSql.getPS());
	}
	
	@Test public void testMap() throws Exception
	{
		String sql = "select * from table where name<>$param.name$ and age>$age$";
		SQLLib.MappedSQL mappedSql = new SQLLib.MappedSQL(sql);
		assertEquals(mappedSql.getMapping().get(0),"param.name");
		assertEquals(mappedSql.getMapping().get(1),"age");
	}
	
}
