#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# NuGram Hosted Server client API in Python.
#
# Copyright (C) 2009 Nu Echo Inc.
#
# This code relies on the 'simplejson' library for compatibility
# between Python and Jython. It has been tested with Python 2.6 and
# Jython 2.5.0.
#
# simplejson can be found here:
#    http://code.google.com/p/simplejson
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The Software shall be used for Good, not Evil.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


import sys
import base64
import urllib
import httplib
from simplejson import JSONEncoder, JSONDecoder

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


DEFAULT_GSERVER_HOST = 'www.grammarserver.com'
DEFAULT_GSERVER_PORT = 8082


## An object of this class acts as a proxy to NuGram Hosted Server.

class GrammarServer:

    def __init__(self, host=DEFAULT_GSERVER_HOST, port=DEFAULT_GSERVER_PORT):
        self.host = host
        self.port = port

    def create_session(self, username, password):
        return Session(self, username, password)

    def session(self, username, password, sessionid):
        return Session(self, username, password, sessionid)

    def get_url():
        return 'http://' + self.host + ':' + self.port



## This class represents a session with NuGram Hosted Server.

class Session:

    def __init__(self, server, username, password, sessionid = None):
        self.server = server
        self.username = username
        self.password = password
        self.sessionId = sessionid
        if sessionid == None:
            self.initialize()


    def get_auth(self):
        return base64.b64encode(self.username + ':' + self.password)


    def request(self, url, mode, data={}, text=''):
        if text:
            data = text
        else:
            data = urllib.urlencode(data)

        connection = httplib.HTTPConnection(self.server.host, self.server.port)
        connection.request(
                mode, url, body=data,
                headers={'Authorization': 'Basic ' + self.get_auth()})
        response = connection.getresponse()
        return response.status, response.read()

    def initialize(self):
        status, content = self.request(
                '/session', 'POST', {'responseFormat': 'json'})

        if not (200 <= status < 300):
            if 400 <= status < 500:
                raise AuthentificationError(str(status))
            if 500 <= status < 600:
                raise InternalError(str(status))
            raise Error(str(status))

        result = JSONDecoder().decode(content)
        
        if result:
            self.sessionId = result['session']['id']
            return self.sessionId


    ## Returns the session ID
    def get_id():
        return self.sessionId


    ## This method uploads a source grammar to NuGram Hosted Server.
    def upload(self, name, content):
        url = '/grammar/' + name

        return self.request(url, 'PUT', text=content)


    ## This method requests NuGram Hosted Server to load a static grammar.
    def load(self, grammarPath):
        return self.instantiate(grammarPath)


    ## This method instantiates a dynamic grammar and loads it.
    ## The 'context' argument is expected to be a dictionary that
    ## maps strings to values. Each value must be convertible to JSON.
    ## Each key in the context must correspond to the name of a variable in the
    ## ABNF template.
    def instantiate(self, grammarPath, context={}):
        url = '/grammar/' + self.sessionId + '/' + grammarPath

        jsonContext = JSONEncoder().encode(context)
        data = {'context': jsonContext, 'responseFormat': 'json'}
        status, content = self.request(url, 'POST', data)
     
        if not (200 <= status < 300):
            raise InstantiationError(str(status))
        
        response = eval(content)

        return InstantiatedGrammar(self, response['grammar'])

    ## Terminates the session with NuGram Hosted Server
    def disconnect(self):
        url = '/session/' + self.sessionId
        return self.request(url, 'DELETE')


## Objects of this class act as proxy for instantiated grammars on NuGram
## Hosted Server.

class InstantiatedGrammar:

    # This constructor is called by a Session object. Should not
    # be called directly by client applications.
    def __init__(self, session, grammarInfo):
        self.session = session
        self.grammarInfo = grammarInfo

    
    ## Returns the URL of the grammar
    def get_url(extension='abnf'):
        url = self.session.server.get_url()
        if extension:
            url += '.' + extension
        return url


    ## Retrieves the source representation of the grammar in the 
    ## requested format ('abnf', 'grxml', or 'gsl')
    def get_content(self, extension='abnf'):
        url = ('/grammar:' + self.session.username + '/'
               + self.session.sessionId + '/' + self.grammarInfo['id'])
        
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
        url = ('/interpretation:' + self.session.username + '/'
               + self.session.sessionId + '/' + self.grammarInfo['id'])

        data = {'sentence': sentence}
        data['responseFormat'] = 'json'

        status, content = self.session.request(url, 'POST', data)

        if not (200 <= status < 300):
            raise InterpretationError(str(status))

        response = JSONDecoder().decode(content)

        return response['interpretation']
 


## A complete example
##
## Call the following function with your username and password for NuGram
## Hosted Server

def test(username='user', password='passwd', write=sys.stdout.write):

    def step(text):
        write('\n' + text + '...\n')
    
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
    step("Opening a session with NuGram Hosted Server")
    session = server.create_session(username, password)
    write("Session ID = " + session.sessionId + '\n')
    step("Uploading a grammar")
    session.upload(filename, grammar)

    step("Instantiating the dynamic grammar")
    grammar = session.instantiate(
            filename,  {'digits':  ["one", "two", "three", "four"]} )
    otherGrammar = session.instantiate(
            filename, {'digits':  ["one", "two", "three", "four"]})

    step("Representation of the instantiated grammar in XML form")
    write(grammar.get_content(extension='grxml') + '\n')
    
    step("Interpreting the first grammar")
    write(str(grammar.interpret('one')) + '\n')

    step("Interpreting the second grammar")
    write(str(otherGrammar.interpret('two')) + '\n')
    write(str(otherGrammar.interpret('no match')) + '\n')

    step("Terminating the session")
    session.disconnect()


def usage():
    sys.stdout.write("Usage: python NuGramServer.py username password\n")


if __name__ == '__main__':
    if len(sys.argv) == 3:
        test(*sys.argv[1:])
    else:
        usage()
