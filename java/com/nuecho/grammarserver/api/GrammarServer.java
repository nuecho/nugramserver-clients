package com.nuecho.grammarserver.api;

import java.io.IOException;


/**
 * An object of this class acts as a proxy to NuGram Server. 
 */
public final class GrammarServer
{
    private static final String DEFAULT_GRAMMARSERVER_SERVER = "www.grammarserver.com";
    private static final int DEFAULT_GRAMMARSERVER_PORT = 8082;
    
    private String mServerName;
    private int mPort;

    public GrammarServer()
    {
        this(DEFAULT_GRAMMARSERVER_SERVER, DEFAULT_GRAMMARSERVER_PORT);
    }
    
    public GrammarServer(String serverName, int port)
    {
        mServerName = serverName;
        mPort = port;
    }
    
    public String getName()
    {
        return mServerName;
    }
    
    public int getPort()
    {
        return mPort;
    }
    
    public String getUrl()
    {
        return "http://" + mServerName + ":" + mPort;
    }
    
    public Session createSession(String username, String password) throws IOException
    {
        return new ServerSession(this, username, password);
    }
}
