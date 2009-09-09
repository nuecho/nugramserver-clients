package com.nuecho.grammarserver.api;

import java.io.IOException;

public interface InstantiatedGrammar
{
    public String getUrl(String format);
    
    public String getContent(String format) throws IOException;
    
    public Object interpret(String sentence) throws IOException;
}
