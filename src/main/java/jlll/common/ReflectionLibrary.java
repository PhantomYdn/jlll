package jlll.common;

import java.lang.reflect.Method;

import jlll.common.annotation.JlllName;


public abstract class ReflectionLibrary implements Library
{
	//protected Logger log = Logger.getLogger(this.getClass());
	
	public void load(Enviroment env) throws JlllException
	{
		loadMethods(this, env);
	}

	
	public static void loadMethods(Object obj, Enviroment env) throws JlllException
	{
		Method[] methods = obj.getClass().getMethods();
		for (int i = 0; i < methods.length; i++)
		{
			Method method = methods[i];
			if(method.isAnnotationPresent(JlllName.class))
			{
				ReflectionPrimitive.createReflectionPrimitive(env, obj, method);
			}
		}
	}
}
