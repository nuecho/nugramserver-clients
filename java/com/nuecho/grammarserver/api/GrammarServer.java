package com.nuecho.grammarserver.api;

import java.io.IOException;

/**
 * Instances of this class act as proxies to NuGram Hosted Server.
 * 
 * @author Nu Echo
 */
public final class GrammarServer
{
    private static final String DEFAULT_GRAMMARSERVER_SERVER = "www.grammarserver.com";
    private static final int DEFAULT_GRAMMARSERVER_PORT = 443;

    private String mServerName;
    private int mPort;

    /**
     * Creates a new NuGram Hosted Server instance.
     */
    public GrammarServer()
    {
        this(DEFAULT_GRAMMARSERVER_SERVER, DEFAULT_GRAMMARSERVER_PORT);
    }

    /**
     * Creates a new NuGram Hosted Server instance from explicit server name and
     * port).
     * @param serverName
     * @param port
     */
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

    /**
     * Returns the NuGram Hosted Server URL.
     * @return
     */
    public String getUrl()
    {
        if (mPort != DEFAULT_GRAMMARSERVER_PORT) {
            return "https://" + mServerName + ":" + mPort;
        }
        else {
            return "https://" + mServerName;
        }
    }

    /**
     * Creates a new session on NuGram Hosted Server.
     * 
     * @param username the NuGram Hosted Server username
     * @param password the NuGram Hosted Server password
     * @return a new session
     * @throws IOException
     */
    public Session createSession(String username, String password) throws GrammarServerException
    {
        try
        {
            return new ServerSession(this, username, password);
        }
        catch (GrammarServerException exception)
        {
            throw new GrammarServerException("Unable to create session", exception);
        }
    }

    public Session session(String username, String password, String sessionId) throws GrammarServerException
    {
        assert sessionId != null;
        return new ServerSession(this, username, password, sessionId);
    }

    /**
     * Creates a new session on NuGram Hosted Server.
     * 
     * @param authToken
     * @return
     * @throws IOException
     */
    public Session createSession(String authToken) throws GrammarServerException
    {
        return new ServerSession(this, authToken);
    }
}
