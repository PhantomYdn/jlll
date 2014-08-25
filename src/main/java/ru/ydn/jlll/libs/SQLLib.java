package ru.ydn.jlll.libs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ru.ydn.jlll.common.Cons;
import ru.ydn.jlll.common.Enviroment;
import ru.ydn.jlll.common.Evaluator;
import ru.ydn.jlll.common.JlllException;
import ru.ydn.jlll.common.Library;
import ru.ydn.jlll.common.Null;
import ru.ydn.jlll.common.Primitive;
import ru.ydn.jlll.common.Procedure;
import ru.ydn.jlll.util.ListUtil;

public class SQLLib implements Library
{
	private enum SQLType
	{
		CREATE, DROP, SELECT, UPDATE, INSERT, DELETE, UNKNOWN;
	}

	public static class MappedSQL
	{
		private static final Pattern pattern = Pattern.compile("\\$(\\S*)\\$");
		
		private List<String> mapping = new ArrayList<String>();
		String ps = null;
		
		public MappedSQL(String sql)
		{ 
			Matcher matcher = pattern.matcher(sql);
			while(matcher.find())
			{
				String name = matcher.group(1);
				mapping.add(name);
			}
			ps = pattern.matcher(sql).replaceAll("?");
		}
		
		public String getPS()
		{
			return ps;
		}
		
		public List<String> getMapping()
		{
			return mapping;
		}
		
		public PreparedStatement getPreparedStatement(Connection conn, Map<String, Object> map, int gkFlag) throws SQLException
		{
			PreparedStatement pst = conn.prepareStatement(getPS(), gkFlag);
			substitute(pst, map);
			return pst;
		}
		
		public PreparedStatement getPreparedStatement(Connection conn, Map<String, Object> map) throws SQLException
		{
			PreparedStatement pst = conn.prepareStatement(getPS());
			substitute(pst, map);
			return pst;
		}
		
		public PreparedStatement getPreparedStatement(Connection conn, Enviroment env) throws SQLException
		{
			PreparedStatement pst = conn.prepareStatement(getPS());
			substitute(pst, env);
			return pst;
		}
		
		public PreparedStatement getPreparedStatement(Connection conn, Enviroment env, int gkFlag) throws SQLException
		{
			PreparedStatement pst = gkFlag==Statement.NO_GENERATED_KEYS?conn.prepareStatement(getPS()):conn.prepareStatement(getPS(), gkFlag);
			substitute(pst, env);
			return pst;
		}
		
		public void substitute(PreparedStatement ps, Map<String, Object> map) throws SQLException
		{
			for(int i=0;i<mapping.size();i++)
			{
				ps.setObject(i+1, map.get(mapping.get(i)));
			}
		}
		
		public void substitute(PreparedStatement ps, Enviroment env) throws SQLException
		{
			for(int i=0;i<mapping.size();i++)
			{
				ps.setObject(i+1, env.lookup(mapping.get(i)));
			}
		}
		
		
	}


	public void load(Enviroment env) throws JlllException
	{
		new Primitive("sql-execute", env)
		{
			/**
			 * 
			 */
			private static final long serialVersionUID = -1621139488315593668L;

			public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
			{
				String sql = values.get(0).toString();
				Connection conn = getConnection(env);
				Object ret = false;
				Statement st =null;
				ResultSet rs = null;
				try
				{
					//System.out.println("SQL="+sql);
					st = conn.createStatement();	
					SQLType type = getSQLType(sql);
					if(type==SQLType.INSERT || type==SQLType.UPDATE || type==SQLType.DELETE)
					{
						ret = getGKFlag(env)==Statement.NO_GENERATED_KEYS?st.executeUpdate(sql):st.executeUpdate(sql, getGKFlag(env));
						if(isAllowGeneratedKeys(env)) ret = resultSetToCons(st.getGeneratedKeys());
					}
					else if(type==SQLType.SELECT || type==SQLType.UNKNOWN)
					{
						rs = st.executeQuery(sql);
						ret = resultSetToCons(rs);
					}
					else if(type==SQLType.CREATE || type==SQLType.DROP)
					{
						ret = st.execute(sql);
					}
				} 
				catch (SQLException e)
				{
					throw new JlllException("SQLException",e);
				}
				finally
				{
					closeAll(conn, st, null);
				}
				return ret;
			}
		};
		
		new Primitive("sql-execute-ps", env)
		{
			/**
			 * 
			 */
			private static final long serialVersionUID = 8261547122480573498L;

			public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
			{
				String sql = values.get(0).toString();
				Connection conn = getConnection(env);
				Object ret = false;
				PreparedStatement ps =null;
				ResultSet rs = null;
				try
				{
					ps = getGKFlag(env)==Statement.NO_GENERATED_KEYS?conn.prepareStatement(sql):conn.prepareStatement(sql, getGKFlag(env));
					for(int i=1;i<values.length();i++)
					{
						Object value = values.get(i);
						if(value instanceof Null) value=null;
						ps.setObject(i, value);
					}
					SQLType type = getSQLType(sql);
					if(type==SQLType.INSERT || type==SQLType.UPDATE || type==SQLType.DELETE)
					{
						ret = ps.executeUpdate();
						if(isAllowGeneratedKeys(env)) ret = resultSetToCons(ps.getGeneratedKeys());
					}
					else if(type==SQLType.SELECT || type==SQLType.UNKNOWN)
					{
						rs = ps.executeQuery();
						ret = resultSetToCons(rs);
					}
					else if(type==SQLType.CREATE || type==SQLType.DROP)
					{
						ret = ps.execute();
					}
				} 
				catch (SQLException e)
				{
					throw new JlllException("SQLException",e);
				}
				finally
				{
					closeAll(conn, ps, null);
				}
				return ret;
			}
		};
		
		new Primitive("sql-execute-mps", env)
		{
			/**
			 * 
			 */
			private static final long serialVersionUID = -7111241343538667181L;

			public Object applayEvaluated(Cons values, Enviroment env) throws JlllException
			{
				String sql = values.get(0).toString();
				Connection conn = getConnection(env);
				Object ret = false;
				PreparedStatement ps =null;
				ResultSet rs = null;
				try
				{
					ps = new MappedSQL(sql).getPreparedStatement(conn, env, getGKFlag(env));
					SQLType type = getSQLType(sql);
					if(type==SQLType.INSERT || type==SQLType.UPDATE || type==SQLType.DELETE)
					{
						ret = ps.executeUpdate();
						if(isAllowGeneratedKeys(env)) ret = resultSetToCons(ps.getGeneratedKeys());
					}
					else if(type==SQLType.SELECT || type==SQLType.UNKNOWN)
					{
						rs = ps.executeQuery();
						ret = resultSetToCons(rs);
					}
					else if(type==SQLType.CREATE || type==SQLType.DROP)
					{
						ret = ps.execute();
					}
				} 
				catch (SQLException e)
				{
					throw new JlllException("SQLException",e);
				}
				finally
				{
					closeAll(conn, ps, null);
				}
				return ret;
			}
		};	
	}
	
	/*private Object retrieveData(Statement st,ResultSet rs, String sql) throws SQLException
	{
		Object ret = null;
		boolean isPS = st instanceof PreparedStatement;
		PreparedStatement ps = isPS?(PreparedStatement)st:null;
		SQLType type = getSQLType(sql);
		if(type==SQLType.INSERT)
		{
			isPS?ps.executeUpdate():st.executeUpdate(sql, Statement.RETURN_GENERATED_KEYS);
		}
		if(type==SQLType.UPDATE || type==SQLType.DELETE)
			ret = isPS?ps.executeUpdate():st.executeUpdate(sql);	
		else if(type==SQLType.SELECT || type==SQLType.UNKNOWN)
		{
			rs = isPS?ps.executeQuery():st.executeQuery(sql);
			ret = resultSetToCons(rs);
		}
		else if(type==SQLType.CREATE || type==SQLType.DROP)
		{
			ret = isPS?ps.execute():st.execute(sql);
		}
		return ret;
	}*/
	
	private Connection getConnection(Enviroment env) throws JlllException
	{
		Object getConnectionProcedure = env.lookup("sql-get-connection");
		if(getConnectionProcedure==null)
			throw new JlllException("Please define 'sql-get-connection' procedure before sql-* invokations");
		Object conn = Evaluator.eval(getConnectionProcedure, env);
		conn = ((Procedure)conn).applay(Null.NULL, env);
		if(conn instanceof Connection)
			return (Connection)conn;
		else
			throw new JlllException("sql-get-connection return not a connection, but: "+conn.getClass().getName());
	}
	
	private int getGKFlag(Enviroment env) throws JlllException
	{
		return isAllowGeneratedKeys(env)?Statement.RETURN_GENERATED_KEYS:Statement.NO_GENERATED_KEYS;
	}
	
	private boolean isAllowGeneratedKeys(Enviroment env) throws JlllException
	{
		Object ret = env.lookup("sql-allow-generatedkeys");
		return (ret==null?false:(ret instanceof Boolean?((Boolean)ret).booleanValue():false));
	}
	
	private void closeAll(Connection conn, Statement st, ResultSet rs)
	{
		if(rs!=null)
		{
			try{rs.close();} catch (SQLException e){}
		}
		if(st!=null)
		{
			try{st.close();} catch (SQLException e){}
		}
		if(conn!=null)
		{
			try{conn.close();} catch (SQLException e){}
		}
	}
	
	private Cons resultSetToCons(ResultSet rs) throws SQLException
	{
		ResultSetMetaData rsmd = rs.getMetaData();
		int cols = rsmd.getColumnCount();
		List<Cons> ret = new ArrayList<Cons>();
		while(rs.next())
		{
			List<Object> rowData = new ArrayList<Object>();
			for(int i=1;i<=cols;i++)
			{
				Object data = rs.getObject(i);
				//System.out.println("data="+data);
				rowData.add(convert(data));
			}
			//System.out.println(rowData+"   === "+ ListUtil.arrayToCons(rowData.toArray()));
			ret.add(ListUtil.arrayToCons(rowData.toArray()));
		}
		return ListUtil.arrayToCons(ret.toArray());
	}
	
	private Object convert(Object data)
	{
		if(data==null) return Null.NULL;
		if(data instanceof Long)
		{
			Long lng = (Long)data;
			if(lng<Integer.MAX_VALUE && lng>Integer.MIN_VALUE) return lng.intValue();
			else return lng;
		}
		
		return data;
	}
	
	private SQLType getSQLType(String sql)
	{
		String preparedSQL = sql.trim().toLowerCase();
		if(preparedSQL.indexOf("insert")==0) return SQLType.INSERT;
		else if(preparedSQL.indexOf("select")==0) return SQLType.SELECT;
		else if(preparedSQL.indexOf("update")==0) return SQLType.UPDATE;
		else if(preparedSQL.indexOf("delete")==0) return SQLType.DELETE;
		else if(preparedSQL.indexOf("create")==0) return SQLType.CREATE;
		else if(preparedSQL.indexOf("drop")==0) return SQLType.DROP;
		else return SQLType.UNKNOWN;			
	}
}
