package com.nuecho.grammarserver.api.test;

import java.io.IOException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import com.nuecho.grammarserver.api.GrammarServer;
import com.nuecho.grammarserver.api.InstantiatedGrammar;
import com.nuecho.grammarserver.api.Session;

public class GrammarServerTest
{
    private final static String GRAMMAR = "#ABNF 1.0 ISO-8859-1;\n\nlanguage en-US;\ntag-format <semantics/1.0>;\n\nroot $digits;\n\npublic $digits  = \n@alt\n    @for (digit : digits)\n        @word digit\n    @end\n@end\n;";
    
    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws IOException
    {
        if (args.length != 2) 
        {
            throw new RuntimeException("Please provide the username and password for your NuGram Hosted Server account.");
        }
        
        String username = args[0];
        String password = args[1];
        
        GrammarServer server = new GrammarServer();
        Session session = server.createSession(username, password);
        System.out.println("NuGram Hosted Server session ID = " + session.getSessionId());
        
        session.upload("digits.abnf", GRAMMAR);
        
        JSONObject context = new JSONObject();
        JSONArray digits = new JSONArray();
        digits.add("one");
        digits.add("two");
        digits.add("three");
        context.put("digits", digits);
        
        InstantiatedGrammar grammar = session.instantiate("digits.abnf", context);
        System.out.println("Instantiated grammar content (ABNF):\n" + grammar.getContent(InstantiatedGrammar.ABNF_FORMAT));
        System.out.println("Interpretation for 'one': " + grammar.interpret("one"));
        
        session.disconnect();
    }
}
