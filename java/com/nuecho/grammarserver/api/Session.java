package com.nuecho.grammarserver.api;

import java.io.IOException;

import org.json.simple.JSONObject;

public interface Session
{
    public void upload(String grammarPath, String content) throws IOException;
    
    public InstantiatedGrammar load(String grammarPath) throws IOException;
    
    public InstantiatedGrammar instantiate(String grammarPath, JSONObject data) throws IOException;
    
    public void disconnect();
}
