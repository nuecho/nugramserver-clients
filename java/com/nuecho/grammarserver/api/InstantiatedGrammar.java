package com.nuecho.grammarserver.api;

import java.io.IOException;

/**
 * Instances of this class represent grammars loaded by NuGram Hosted Server.
 * 
 * @author Nu Echo
 */
public interface InstantiatedGrammar
{
    public static final String ABNF_FORMAT = "abnf";
    public static final String GRXML_FORMAT = "grxml";
    public static final String GSL_FORMAT = "gsl";

    /**
     * Returns the grammar's source URL.
     * @return the grammar's source URL
     */
    public String getUrl();

    /**
     * Returns the grammar's source URL for the given grammar format.
     * @param format one of the constants ABNF_FORMAT, GRXML_FORMAT, or
     * GSL_FORMAT
     * @return the grammar's source URL
     */
    public String getUrl(String format);

    /**
     * Returns the source code of the grammar.
     * @return the grammar source code
     * @throws IOException
     */
    public String getContent() throws GrammarServerException;

    /**
     * Returns the source code of the grammar in the given format.
     * @param format one of the constants ABNF_FORMAT, GRXML_FORMAT, or
     * GSL_FORMAT
     * @return the grammar source code
     * @throws IOException
     */
    public String getContent(String format) throws GrammarServerException;

    /**
     * Computes the semantic interpretation of the given sentence (a string)
     * using the instantiated grammar. Returns a JSONArray object or Boolean.FALSE 
     * if there is no associated interpretation.
     * @param sentence
     * @return a JSONArray object or Boolean.FALSE
     * @throws IOException
     */
    public Object interpret(String sentence) throws GrammarServerException;
}
