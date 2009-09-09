/*
 * NuGram Hosted Server client API in JavaScript.
 *
 * Copyright (C) 2009 Nu Echo Inc.
 *
 * This code relies quite heavily on Rhino 1.7R2 as support
 * for ECMAScript for XML (E4X) is needed, as well as the
 * interface to Java classes.
 *
 * This implementation also relies on two other objects:
 *   * JSON - this object must provides two functions: parse and stringify,
 *            to convert JSON string to and from JavaScript objects. The
 *            accompanying file 'json.js' provides an implementation of this
 *            object.
 *
 *   * Base64 - Needed for HTTP Basic authorization. The accompanying file
 *            'base64.js' provides an implementation of this object. Not required
 *			  if the authorization token is given explicitly in argument
 *			  to the GrammarServer.createSession function (see documentation below). 
 * 
 * See at the end of this file for a test example. The most important functions
 * are documented below.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 */


/** 
    This function creates an object that acts as a proxy to NuGram Hosted Server.
    @param server the server name (defaults to 'www.grammarserver.com')
    @param port the server port (defaults to 8082)
**/
function GrammarServer(server, port, secure) {
    if (!server) server = 'www.grammarserver.com';
    if (!port) port = 8082;

    var protocol = secure ? "https://" : "http://";
    if (port == undefined) {
	this.url = protocol + server;
    }
    else {
        this.url = protocol + server + ":" + port;
    }
};

GrammarServer.prototype.getUrl = function () {
    return this.url;
};

/** 
    This function initiates a new session with NuGram Hosted Server and returns
    a GrammarServerSession object.
    
    If the password is omitted, the first argument must hold the
    authorization token for NuGram Hosted Server (the Base64 encoding
    of the string "username:password".

    @param username the account name on NuGram Hosted Server
    @param password the password for the account
**/
    
GrammarServer.prototype.createSession = function(username, password) {
	var authToken = username;
	if (password) {
    	var authToken = Base64.encode(username + ":" + password);
   	}
    return new GrammarServerSession(this, authToken);
};

function GrammarServerSession(server, authToken) {
    this.server = server;
    this.authToken = authToken;
    this.state = 'disconnected';
    this.sessionId = false;

    var result = __http_request(server.getUrl() + '/session', 'POST', authToken);
    if (result) {
        this.state = 'connected';
        this.sessionId = (new XML(result)).@id;
    }
};

GrammarServerSession.prototype.isConnected = function() {
    return this.state == 'connected';
};

GrammarServerSession.prototype.getSessionId = function() {
	return this.sessionId;
};

/**
   Uploads a grammar to NuGram Hosted Server.
   @param grammarPath the grammar path that will be used to refer to the grammar
   @param the text of the ABNF grammar

   Note: grammars are usually uploaded using NuGram IDE
   (see http://nugram.nuecho.com:8081/help/topic/com.nuecho.plugin.grammaride.doc/doc/html/abnf_publishing.html)
**/
GrammarServerSession.prototype.upload = function(grammarPath, content) {
    var result = __http_request(this.server.getUrl() + "/grammar/" + grammarPath, 'PUT', this.authToken, undefined, content);
    if (result) {
        return JSON.parse(result);
    }
    return false;
};


/**
   Loads a static grammar on NuGram Hosted Server. This is usually done when the application
   wants to do some semantic interpretation using the grammar. Returns an InstantiatedGrammar 
   object.

   @param grammarPath the path of the grammar to load
**/
GrammarServerSession.prototype.load = function(grammarPath) {
    return this.instantiate(grammarPath, {});
};


/**
   Instantiates a dynamic grammar with the given data. Returns an InstantiatedGrammar
   object upon successful instantiation.
   
   @param grammarPath the path of the grammar to instantiate
   @param the data used to populate the dynamic grammar template (the instantiation context)
**/
GrammarServerSession.prototype.instantiate = function (grammarPath, data) {
    if (this.sessionId) {
        var jsonData = JSON.stringify(data);
        var result = __http_request(this.server.getUrl() + "/grammar/" + this.sessionId + "/"  + grammarPath, 
                                    'POST', this.authToken, {context: jsonData, responseFormat: 'json'});
        if (result) {
            var jsonResult = JSON.parse(result);
            return new InstantiatedGrammar(this, jsonResult.grammar);
        }
        return false;
    }
    else {
        return false;
    }
};

/**
   Terminates the session with NuGram Hosted Server.
**/
GrammarServerSession.prototype.disconnect = function() {
    this.state = 'disconnected';
    __http_request(this.server.getUrl() + '/session/' + this.sessionId, 'DELETE', this.authToken);
};



function InstantiatedGrammar(session, data) {
    this.session = session;
    this.data = data;
};


/**
   Returns the URL of the instantiated grammar. This URL can be passed to an
   ASR engine.
   
   @param format (optional) the format of the source grammar. 
**/
InstantiatedGrammar.prototype.getUrl = function(format) {
    var url = this.data.grammarUrl;
    if (format) {
        url += '.';
        url += format;
    }
    return url;
}

/**
   Retrieves the source representation of the instantiated grammar.
   
   @param format (optional) the format of the source grammar to retrieve (defaults to ABNF)
**/
InstantiatedGrammar.prototype.getContent = function(format) {
    return __http_request(this.getUrl(format), 'GET');
};


/**
   Computes the semantic interpretation of the given sentence (a string) using the
   instantiated grammar. Returns an array of possible interpretations, or 'false' if
   there is none.
**/
InstantiatedGrammar.prototype.interpret = function (text) {
    var result = __http_request(this.data.interpreterUrl, 
                                'POST', 
                                this.session.authToken, 
                                {sentence: text.toString(), responseFormat: 'json'});
    if (result) {
        jsonResult = JSON.parse(result);
        return jsonResult.interpretation;
    }
    return false;
};


// *** A few helper functions

function __read_stream_as_string(inputStream) {
    var buffer = new java.lang.StringBuffer();
    var reader = new java.io.BufferedReader(new java.io.InputStreamReader(inputStream));

    while (reader.ready()) {
        buffer.append(reader.readLine()).append('\n');
    }
    return "" + buffer.toString();
};

function __http_request(url, method, authToken, data, text) {
    var connection = (new java.net.URL(url)).openConnection();
    connection.setRequestMethod(method);
    if (authToken) {
        connection.setRequestProperty("Authorization", "Basic " + authToken);
    }
    if (data || text) {
        var content = "";
        if (text) {
            content = text;
        }
        else {
            for (param in data) {
                content += "&" + param + "=" + data[param];
            }
            content = content.substring(1);
        }
        connection.setDoOutput(true);
        var writer = new java.io.OutputStreamWriter(connection.getOutputStream());
        writer.write(content);
        writer.close();
    }
    connection.setConnectTimeout(5000);
    if (connection.getResponseCode() >= 200 && connection.getResponseCode() < 300) {
        var response = __read_stream_as_string(connection.getInputStream());
        connection.disconnect();
        return response;
    }
    else {
        connection.disconnect();
        throw "com.nuecho.grammarserver.exception:" + connection.getResponseMessage();
    }
};




/**
   EXAMPLE
   
   A simple test function to illustrate the API usage.
   Simply call the test function with your username/password
   for NuGram Hosted Server.
**/

function test(username, password) {
    DIGITS_GRAMMAR = "#ABNF 1.0 ISO-8859-1;\n\
\n\
language en-US;\n\
tag-format <semantics/1.0>;\n\
\n\
root $digits;\n\
\n\
public $digits  = \n\
@alt\n\
    @for (digit : digits)\n\
        @word digit\n\
    @end\n\
@end\n\
;";

    gserver = new GrammarServer();

    print("Creating session....");
    session = gserver.createSession(username, password);
    print("Uploading grammar");
    session.upload("digits.abnf", DIGITS_GRAMMAR);

    print("Instantiating grammar");
    var grammar = session.instantiate("digits.abnf", {digits: ["one", "two", "three", "four"]});

    print("Grammar URL = " + grammar.getUrl());
    print("Grammar content:\n" + grammar.getContent('grxml'));

    var interp = grammar.interpret("two");
    print("Interpretation = " + interp);
    session.disconnect();
    return interp;
};

