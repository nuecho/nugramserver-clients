%%% File        : nugramserverx.erl
%%% Author      : Nu Echo Inc.
%%% Description : NuGram Hosted Server API implementation
%%
%% Copyright (C) 2009 Nu Echo Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The Software shall be used for Good, not Evil.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

% This module requires Yaws' JSON module.


-module(nugramserver).


%% Public API

-export([create_session/2, create_session/4,
         session/3, session/5,
	 disconnect/1, 
	 upload/3,
	 activate/2,
	 instantiate/3,
	 grammar_url/1, grammar_url/2,
	 interpret/2]).

-export([test/2]).

-include("nugramserver.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(GRAMMARSERVER_HOST, "www.grammarserver.com").
-define(GRAMMARSERVER_PORT, 443).


%%% Initiates a new session with NuGram Hosted Server
%%% Returns a tuple representing a session.
create_session(Username, Password) ->
    create_session(?GRAMMARSERVER_HOST, ?GRAMMARSERVER_PORT, Username, Password).

create_session(Host, Port, Username, Password)
  when is_integer(Port) andalso is_list(Host) ->
    Server = Host ++ ":" ++ integer_to_list(Port),
    Session = #gserver_session{server = Server, username = Username, password = Password},
    Request = server_request(Session, "/api/session", ""),
    {ok, {{_,201,_}, _Headers, Content}} = http:request(post, Request, [{timeout, 2000}], []),
    Id = attribute_value("/session/@id", parse_xml(Content)),
    Session#gserver_session{id = Id}.


%%% Creates a session object
session(Username, Password, SessionId) ->
    session(?GRAMMARSERVER_HOST, ?GRAMMARSERVER_PORT, Username, Password, SessionId).

session(Host, Port, Username, Password, SessionId)
  when is_integer(Port) andalso is_list(Host) ->
    Server = Host ++ ":" ++ integer_to_list(Port),
    #gserver_session{server = Server, username = Username, password = Password, id = SessionId}.
    

%%% Terminates the session with NuGram Hosted Server.
disconnect(Session = #gserver_session{}) ->
    Request = {server_url(Session, "/api/session/" ++ Session#gserver_session.id), 
	       server_request_headers(Session)},
    {ok, {{_,200,_}, _Headers, _Content}} = http:request(delete, Request, [{timeout, 2000}], []),
    ok.


%%% Uploads a source grammar to NuGram Hosted Server
upload(Session = #gserver_session{}, GrammarPath, Content) ->
    Request = server_request(Session,
			     "/api/grammar/" ++ url_encode(GrammarPath),
			     Content,
			     "text/basic"),
    {ok, {{_, 201, _}, _, _}} = http:request(put, Request, [{timeout, 2000}], []),
    ok.

%%% This method requests NuGram Hosted Server to load a static grammar.
activate(Session = #gserver_session{}, GrammarPath) ->
    instantiate(Session, GrammarPath, defaultObject).

%%% This method instantiates a dynamic grammar and loads it.
instantiate(Session = #gserver_session{}, GrammarPath, Context) ->
    JsonObject = json_utils:expand(Context),
    Request = server_request(Session,
			     "/api/grammar/" ++ Session#gserver_session.id ++ "/" ++ GrammarPath,
			     "context=" ++ url_encode(lists:flatten(json:encode(JsonObject)))),
    {ok, {{_,201,_}, _, Content}} = http:request(post, Request,  [{timeout, 2000}], []),
    Xml = parse_xml(Content),
    GrammarUrl = attribute_value("/grammar/@grammarUrl", Xml),
    InterpreterUrl = attribute_value("/grammar/@interpreterUrl", Xml),
    Id = attribute_value("/grammar/@id", Xml),
    #gserver_grammar{session = Session, id = Id, grammarUrl = GrammarUrl, interpreterUrl = InterpreterUrl}.



%%% Returns the URL of the instantiated grammar
grammar_url(Grammar = #gserver_grammar{}) ->
    grammar_url(Grammar, abnf).

%%% Returns the URL of the instantiated grammar in the given format (abnf, grxml, or gsl)
grammar_url(Grammar = #gserver_grammar{}, Type) when is_atom(Type) ->
    true = lists:member(Type, [abnf,grxml,gsl]),
    lists:flatten(io_lib:format("~s.~w", [Grammar#gserver_grammar.grammarUrl, Type])).


%%% Returns the source code of the given instantiated grammar.
grammar_content(Grammar = #gserver_grammar{}, Type) when is_atom(Type) ->
    Url = grammar_url(Grammar, Type),
    Session = Grammar#gserver_grammar.session,
    Request = {Url, server_request_headers(Session)},
    {ok, {{_, 200, _}, _, Content}} = http:request(get, Request,  [{timeout, 2000}], []),
    Content.
    


%%% Interprets a textual sentence and returns the result as a JSON object 
%%% (using an Erlang representation).
interpret(Grammar = #gserver_grammar{}, Sentence) ->
    Session = Grammar#gserver_grammar.session,
    Request = {Grammar#gserver_grammar.interpreterUrl, 
	       server_request_headers(Session),
	       "application/x-www-form-urlencoded", 
	       "sentence=" ++ url_encode(Sentence)},
    {ok, {_, _, Content}} = http:request(post, Request,  [{timeout, 2000}], []),
    {ok, Interpretation} = json:decode_string(text_value("/interpretation/text()", parse_xml(Content))),
    json_utils:simplify(Interpretation).



%% Some utility functions

server_url(Session, Path) ->
    "https://" ++ Session#gserver_session.server ++ Path.

server_request(Session, Path, Data) ->
    server_request(Session, Path, Data, "application/x-www-form-urlencoded").
server_request(Session, Path, Data, ContentType) ->
    {server_url(Session, Path),  server_request_headers(Session), ContentType, Data}.


server_request_headers(Session) ->
    [auth_header(Session), {"Host", Session#gserver_session.server}].


auth_header(Session) ->
    auth_header(Session#gserver_session.username, Session#gserver_session.password).

auth_header(Username, Password) ->
    Auth = "Basic " ++ base64:encode_to_string(Username ++ ":" ++ Password),
    {"Authorization", Auth}.

parse_xml(Content) ->
    {Xml, _} = xmerl_scan:string(Content, [{quiet, true}]),
    Xml.

attribute_value(Path, Xml) ->
    [Node] = xmerl_xpath:string(Path, Xml),
    Node#xmlAttribute.value.


text_value(Path, Xml) ->
    [Node] = xmerl_xpath:string(Path, Xml),
    Node#xmlText.value.


%% Test function

test(Username, Password) ->
    inets:start(),
    Session = create_session(Username, Password),
    io:format("session = ~p~n", [Session]),
    ok = upload(Session, "digits.abnf", "#ABNF 1.0 ISO-8859-1;\n\nlanguage en-US;\ntag-format <semantics/1.0>;\n\nroot $digits;\n\npublic $digits  = \n@alt\n    @for (digit : digits)\n        @word digit\n    @end\n@end\n;"),

    Grammar = instantiate(Session, "digits.abnf", [{digits, {"one", "two", "three"}}]),

    io:format("grammar = ~p~n", [Grammar]),
    io:format("grammar Url = ~p~n", [grammar_url(Grammar)]),
    io:format("grammar content = ~n~p~n", [grammar_content(Grammar, abnf)]),
    io:format("GrXML grammar Url = ~p~n", [grammar_url(Grammar, grxml)]),
    io:format("interpretation: ~p~n", [interpret(Grammar, "one")]),

    disconnect(Session).


%% Code stolen from Yaws (yaws.hyber.org)

url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].


integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.


old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

