package jlll.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

public class Marshaller
{   
    public static byte[] marshall(Object obj) throws IOException
    {
        List<Object> list = new ArrayList<Object>();
        list.add(obj);
        return marshall(list);
    }
    public static byte[] marshall(List<Object> list) throws IOException
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        marshall(list, baos);
        return baos.toByteArray();
    }
    public static void marshall(Object obj, OutputStream out) throws IOException
    {
        marshall(obj, out, true);
    }
    
    public static void marshall(Object obj, OutputStream out, boolean close) throws IOException
    {
        List<Object> list = new ArrayList<Object>();
        list.add(obj);
        marshall(list, out, close);
    }
    
    public static void marshall(List<Object> list, OutputStream out) throws IOException
    {
        marshall(list, out,true);
    }
    
    public static void marshall(List<Object> list, OutputStream out, boolean close) throws IOException
    {
        ObjectOutputStream oos = new ObjectOutputStream(out);
        for (Object next : list)
        {
            oos.writeObject(next);
            oos.flush();
        }
        if(close)oos.close();
    }
    
    public static List<Object> unmarshall(byte[] bytes) throws IOException
    {
        return unmarshall(new ByteArrayInputStream(bytes));
    }
    
    public static List<Object> unmarshall(InputStream in) throws IOException
    {
        List<Object> output = new ArrayList<Object>();
        try
        {
            ObjectInputStream ois = new ObjectInputStream(in);        
            while(true)
            {
                Object next;
                next = ois.readObject();
                if(next==null) break;
                output.add(next);
            }
        }
        catch(EOFException e)
        {
            //NOP
        }
        catch (ClassNotFoundException e)
        {            
            throw new IOException("ClassNotFoundException: "+e.getMessage());
        }
        return output;
    }   
}
