package ru.ydn.jlll.io;

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

/**
 * Serializes and deserializes JLLL objects to/from byte streams.
 * Uses Java object serialization to persist Cons structures, Symbols, and other JLLL values.
 */
public class Marshaller
{
    /**
     * Serializes a single object to a byte array.
     *
     * @param obj
     *            the object to serialize
     * @return the serialized bytes
     * @throws IOException
     *             if serialization fails
     */
    public static byte[] marshall(Object obj) throws IOException
    {
        List<Object> list = new ArrayList<Object>();
        list.add(obj);
        return marshall(list);
    }

    /**
     * Serializes a list of objects to a byte array.
     *
     * @param list
     *            the objects to serialize
     * @return the serialized bytes
     * @throws IOException
     *             if serialization fails
     */
    public static byte[] marshall(List<Object> list) throws IOException
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        marshall(list, baos);
        return baos.toByteArray();
    }

    /**
     * Serializes a single object to an output stream.
     *
     * @param obj
     *            the object to serialize
     * @param out
     *            the output stream
     * @throws IOException
     *             if serialization fails
     */
    public static void marshall(Object obj, OutputStream out) throws IOException
    {
        marshall(obj, out, true);
    }

    /**
     * Serializes a single object to an output stream with optional close.
     *
     * @param obj
     *            the object to serialize
     * @param out
     *            the output stream
     * @param close
     *            whether to close the stream when done
     * @throws IOException
     *             if serialization fails
     */
    public static void marshall(Object obj, OutputStream out, boolean close) throws IOException
    {
        List<Object> list = new ArrayList<Object>();
        list.add(obj);
        marshall(list, out, close);
    }

    /**
     * Serializes a list of objects to an output stream.
     *
     * @param list
     *            the objects to serialize
     * @param out
     *            the output stream
     * @throws IOException
     *             if serialization fails
     */
    public static void marshall(List<Object> list, OutputStream out) throws IOException
    {
        marshall(list, out, true);
    }

    /**
     * Serializes a list of objects to an output stream with optional close.
     *
     * @param list
     *            the objects to serialize
     * @param out
     *            the output stream
     * @param close
     *            whether to close the stream when done
     * @throws IOException
     *             if serialization fails
     */
    public static void marshall(List<Object> list, OutputStream out, boolean close) throws IOException
    {
        ObjectOutputStream oos = new ObjectOutputStream(out);
        for (Object next : list)
        {
            oos.writeObject(next);
            oos.flush();
        }
        if (close)
            oos.close();
    }

    /**
     * Deserializes objects from a byte array.
     *
     * @param bytes
     *            the serialized data
     * @return list of deserialized objects
     * @throws IOException
     *             if deserialization fails
     */
    public static List<Object> unmarshall(byte[] bytes) throws IOException
    {
        return unmarshall(new ByteArrayInputStream(bytes));
    }

    /**
     * Deserializes objects from an input stream.
     *
     * @param in
     *            the input stream containing serialized data
     * @return list of deserialized objects
     * @throws IOException
     *             if deserialization fails
     */
    public static List<Object> unmarshall(InputStream in) throws IOException
    {
        List<Object> output = new ArrayList<Object>();
        try
        {
            ObjectInputStream ois = new ObjectInputStream(in);
            while (true)
            {
                Object next;
                next = ois.readObject();
                if (next == null)
                    break;
                output.add(next);
            }
        }
        catch (EOFException e)
        {
            //NOP
        }
        catch (ClassNotFoundException e)
        {
            throw new IOException("ClassNotFoundException: " + e.getMessage());
        }
        return output;
    }
}
