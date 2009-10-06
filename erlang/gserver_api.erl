%%% File        : gserver_api.erl
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

% This module requires Yaws' JSON module.


-module(gserver_api).


%% Public API

-export([create_session/2, create_session/4,
	 disconnect/1, 
	 upload_grammar/3,
	 activate_grammar/2,
	 instantiate_grammar/3,
	 grammar_url/1, grammar_url/2,
	 interpret_sentence/2]).

-export([test/2]).

-include("gserver_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(GRAMMARSERVER_HOST, "www.grammarserver.com").
-define(GRAMMARSERVER_PORT, 8082).


%%% Initiates a new session with NuGram Hosted Server
%%% Returns a tuple representing a session.
create_session(Username, Password) ->
    create_session(?GRAMMARSERVER_HOST, ?GRAMMARSERVER_PORT, Username, Password).

create_session(Host, Port, Username, Password)
  when is_integer(Port) andalso is_list(Host) ->
    Server = Host ++ ":" ++ integer_to_list(Port),
    Session = #gserver_session{server = Server, username = Username, password = Password},
    Request = server_request(Session, "/session", ""),
    {ok, {{_,201,_}, _Headers, Content}} = http:request(post, Request, [{timeout, 2000}], []),
    Id = attribute_value("/session/@id", parse_xml(Content)),
    Session#gserver_session{id = Id}.


%%% D
delete_session(Session = #gserver_session{}) ->
    Request = {server_url(Session, "/session/" ++ Session#gserver_session.id), 
	       server_request_headers(Session)},
    {ok, {{_,200,_}, _Headers, _Content}} = http:request(delete, Request, [{timeout, 2000}], []),
    ok.


upload_grammar(Session = #gserver_session{}, GrammarPath, Content) ->
    Request = server_request(Session,
			     "/grammar/" ++ yaws_api:url_encode(GrammarPath),
			     Content,
			     "text/basic"),
    {ok, {{_, 201, _}, _, _}} = http:request(put, Request, [{timeout, 2000}], []),
    ok.

server_url(Session, Path) ->
    "http://" ++ Session#gserver_session.server ++ Path.

server_request(Session, Path, Data) ->
    server_request(Session, Path, Data, "application/x-www-form-urlencoded").
server_request(Session, Path, Data, ContentType) ->
    {server_url(Session, Path),  server_request_headers(Session), ContentType, Data}.


server_request_headers(Session) ->
    [auth_header(Session), {"Host", Session#gserver_session.server}].


activate_grammar(Session = #gserver_session{}, GrammarPath) ->
    instantiate_grammar(Session, GrammarPath, defaultObject).

instantiate_grammar(Session = #gserver_session{}, GrammarPath, Context) ->
    JsonObject = json_utils:expand(Context),
    Request = server_request(Session,
			     "/grammar/" ++ Session#gserver_session.id ++ "/" ++ GrammarPath,
			     "context=" ++ yaws_api:url_encode(lists:flatten(json:encode(JsonObject)))),
    {ok, {{_,201,_}, _, Content}} = http:request(post, Request,  [{timeout, 2000}], []),
    Xml = parse_xml(Content),
    GrammarUrl = attribute_value("/grammar/@grammarUrl", Xml),
    InterpreterUrl = attribute_value("/grammar/@interpreterUrl", Xml),
    Id = attribute_value("/grammar/@id", Xml),
    #gserver_grammar{session = Session, id = Id, grammarUrl = GrammarUrl, interpreterUrl = InterpreterUrl}.


grammar_url(Grammar = #gserver_grammar{}) ->
    grammar_url(Grammar, abnf).

grammar_url(Grammar = #gserver_grammar{}, Type) when is_atom(Type) ->
    lists:flatten(io_lib:format("~s.~w", [Grammar#gserver_grammar.grammarUrl, Type])).


interpret_sentence(Grammar = #gserver_grammar{}, Sentence) ->
    Session = Grammar#gserver_grammar.session,
    Request = {Grammar#gserver_grammar.interpreterUrl, 
	       server_request_headers(Session),
	       "application/x-www-form-urlencoded", 
	       "sentence=" ++ yaws_api:url_encode(Sentence)},
    {ok, {_, _, Content}} = http:request(post, Request,  [{timeout, 2000}], []),
    {ok, Interpretation} = json:decode_string(text_value("/interpretation/text()", parse_xml(Content))),
    json_utils:simplify(Interpretation).



%% Some utility functions

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
    ok = upload_grammar(Session, "digits.abnf", "#ABNF 1.0 ISO-8859-1;\n\nlanguage en-US;\ntag-format <semantics/1.0>;\n\nroot $digits;\n\npublic $digits  = \n@alt\n    @for (digit : digits)\n        @word digit\n    @end\n@end\n;"),

    Grammar = instantiate_grammar(Session, "digits.abnf", [{digits, {"one", "two", "three"}}]),

    io:format("grammar = ~p~n", [Grammar]),
    io:format("grammar Url = ~p~n", [grammar_url(Grammar)]),
    io:format("GrXML grammar Url = ~p~n", [grammar_url(Grammar, grxml)]),
    io:format("interpretation: ~p~n", [interpret_sentence(Grammar, "one")]),

    delete_session(Session).


