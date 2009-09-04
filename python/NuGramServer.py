#!/usr/bin/python
#
# NuGram Hosted Server client API in Python.
#
# Copyright (C) 2009 Nu Echo Inc.
#
# This code relies on the 'simplejson' library for compatibility
# between Python and Jython. It has been tested with Python 2.6 and
# Jython 2.5.0.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:


import sys
import base64
import urllib
import httplib
import simplejson.decoder
import simplejson.encoder

## Some exception classes

class Error(Exception):
    pass

class InternalError(Error):
    pass

class AuthentificationError(Error):
    pass

class InstantiationError(Error):
    pass

class InterpretationError(Error):
    pass

class GetGrammarError(Error):
    pass


#DEFAULT_GSERVER_HOST = "www.grammarserver.com"
#DEFAULT_GSERVER_PORT = 8082
DEFAULT_GSERVER_HOST = "localhost"
DEFAULT_GSERVER_PORT = 8102


## An object of this class acts as a proxy to NuGram Server.

class GrammarServer:
    def __init__(self, host = DEFAULT_GSERVER_HOST, port = DEFAULT_GSERVER_PORT):
        self.host = host
        self.port = port

    def get_host(self):
        return self.host

    def get_port(self):
        return self.port

    def create_session(self, username, password):
        return Session(self, username, password)



## This class represents a session with NuGram Hosted Server.

class Session:

    def __init__(self, server, username, password):
        self.server = server
        self.username = username
        self.password = password
        self.sessionId = None
        self.get_sessionid()

    def get_auth(self):
        return base64.b64encode(self.username + ':' + self.password)

    def request(self, url, mode, data={}, text=''):
        if text:
            data = text
        else:
            data = urllib.urlencode(data)

        connection = httplib.HTTPConnection(self.server.get_host(), self.server.get_port())
        connection.request(mode, url, body = data, headers={'Authorization': 'Basic ' + self.get_auth()})
        response = connection.getresponse()
        return response.status, response.read()

    def get_sessionid(self):
        status, content = self.request('/session', 'POST')

        if not (200 <= status < 300):
            if (400 <= status < 500) :
                raise AuthentificationError(str(status))
            if (500 <= status < 600) :
                raise InternalError(str(status))
            raise Error(str(status))

        self.sessionId = content.split('"')[1] 
        return self.sessionId

    ## This method uploads a source grammar to NuGram Server.
    def upload(self, name, content):
        url = '/grammar/' + name

        return self.request(url, 'PUT', text=content)

    ## This method requests NuGram Server to load a static grammar.
    def load(self, grammarPath):
        return self.instantiate(grammarPath)

    ## This method instantiates a dynamic grammar and loads it.
    def instantiate(self, grammarPath, context = {}):
        url = '/grammar/' + self.sessionId + '/' + grammarPath

        jsonContext = simplejson.encoder.JSONEncoder().encode(context);
        data = {'context': jsonContext, 'responseFormat': 'json'}
        status, content = self.request(url, 'POST', data)
     
        if not (200 <= status < 300):
            raise InstantiationError(str(status))
        
        response = simplejson.decoder.JSONDecoder().decode(content)

        return InstantiatedGrammar(self, response['grammar'])

    def disconnect(self):
        url = '/session/' + self.sessionId
        return self.request(url, 'DELETE')


## Objects of this class act as proxy for instantiated grammars on NuGram Server.

class InstantiatedGrammar:

    # This constructor is called by a Session object. Should not
    # be called directly by client applications.
    def __init__(self, session, grammarInfo):
        self.session = session
        self.grammarInfo = grammarInfo

    ## Retrieves the source representation of the grammar in the 
    ## requested format ('abnf', 'grxml', or 'gsl')
    def get_content(self, extension='abnf'):
        url = '/grammar:' + self.session.username + '/' + self.session.sessionId + '/' + self.grammarInfo['id']
        
        if extension: 
            url += '.' + extension
       
        status, content = self.session.request(url, 'GET')

        if not (200 <= status < 300):
            raise GetGrammarError(str(status))

        return content

    ## Computes the semantic interpretation of the given sentence
    ## (which must a string). Returns a Python object of 'False' if
    ## the sentence cannot be parsed by the grammar.
    def interpret(self, sentence):
        url = '/interpretation:' + self.session.username + '/' + self.session.sessionId + '/' + self.grammarInfo['id']

        data = {'sentence': sentence}
        data['responseFormat'] = 'json'

        status, content = self.session.request(url, 'POST', data)

        if not (200 <= status < 300):
            raise InterpretationError(str(status))

        response = simplejson.decoder.JSONDecoder().decode(content)

        return response['interpretation']
 


## 
## A complete example
##
## Call the following function with your username and password for NuGram Server

def test(username = 'user', password = 'passwd'):
    
    filename = 'testTest.abnf'
    grammar = '''\
#ABNF 1.0 UTF-8;

language en-US;
tag-format <semantics/1.0>;

root $digits;

public $digits  = 
@alt
    @for (digit : digits)
        @word digit
    @end
@end
;
'''

    server = GrammarServer()
    print "Opening a session with NuGram Server"
    session = server.create_session(username, password)
    print "Uploading a grammar.."
    session.upload(filename, grammar)

    print "Instantiating the dynamic grammar.."
    grammar = session.instantiate(filename,  {'digits':  ["one", "two", "three", "four"]} )
    otherGrammar = session.instantiate(filename, {'digits':  ["one", "two", "three", "four"]})

    print "Representation of the instantiated grammar in XML form  .."
    print grammar.get_content(extension='grxml')
    
    print "Interpreting the first grammar.."
    print grammar.interpret('one')

    print "Interpreting the second grammar.."
    print otherGrammar.interpret('two')
    print otherGrammar.interpret('no match')

    print "Terminating the session.."
    session.disconnect()


def usage():
    print "Usage: python NuGramServer.py username password"


if __name__ == '__main__':
    if len(sys.argv) == 3:
        test(*sys.argv[1:])
    else:
        usage();
