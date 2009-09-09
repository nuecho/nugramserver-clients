package com.nuecho.grammarserver.api;

import java.io.IOException;

public interface InstantiatedGrammar
{
    public static final String ABNF_FORMAT = "abnf";
    public static final String GRXML_FORMAT = "grxml";
    public static final String GSL_FORMAT = "gsl";
    
    public String getUrl();

    public String getUrl(String format);

    public String getContent() throws IOException;

    public String getContent(String format) throws IOException;

    public Object interpret(String sentence) throws IOException;
}
