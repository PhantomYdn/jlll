package jlll.io;

import java.io.IOException;
import java.net.CookieHandler;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DefaultCookieHandler extends CookieHandler
{
    private Map<URI, List<String>> cache = new HashMap<URI, List<String>>();
    //private static final Map<String, List<String>> NULL_MAP = new HashMap<String, List<String>>();
    @Override
    public Map<String, List<String>> get(URI uri,
            Map<String, List<String>> arg1) throws IOException
    {
//        System.out.println("get URI:"+uri);
        List<String> cookie = cache.get(normilizeURI(uri));
        Map<String, List<String>> ret = new HashMap<String, List<String>>();
        if(cookie!=null && cookie.size()>0) ret.put("Cookie",cookie);
//        System.out.println(ret);
        return ret;
    }

    @Override
    public void put(URI uri, Map<String, List<String>> arg1)
            throws IOException
    {
//        System.out.println("put URI:"+uri);
//        System.out.println(arg1);
        List<String> list = arg1.get("Set-Cookie");
        if(list!=null && list.size()>0)
        {
            String cookie = list.get(0);
            int index = cookie.indexOf(";");
            //TODO: Make something with addons
            //String addons = index>=0?cookie.substring(index+1):null;
            cookie = index>=0?cookie.substring(0,index):cookie;
            cache.put(normilizeURI(uri),Arrays.asList(new String[]{cookie}));
        }
    }
    
    private URI normilizeURI(URI uri) throws IOException
    {        
        try
        {
            String uriStr = uri.toString();
            int indx = uriStr.indexOf("?");
            if(indx<0) return uri;
            else
            {
                return new URI(uriStr.substring(0, indx));
            }
        }
        catch (URISyntaxException e)
        {
            throw new IOException("URISyntaxException:"+e);
        }
    }

}
