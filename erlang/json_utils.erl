%%% File        : json_utils.erl
%%% Author      : Nu Echo Inc.
%%% Description : JSON utilities
%%
%% Copyright (C) 2009 Nu Echo Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

-module(json_utils).

-export([expand/1, simplify/1]).


simplify(true) ->
    true;
simplify(false) ->
    false;
simplify(null) ->
    null;
simplify(undefined) ->
    undefined;
simplify(N) when is_number(N) ->
    N;
simplify(X) when is_list(X) ->
    X;
simplify({array, Elements}) ->
    list_to_tuple(lists:map(fun simplify/1, Elements));
simplify({struct, Vals}) ->
    lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, 
	       lists:map(fun({Attr, Value}) -> {Attr, simplify(Value)} end, 
			 Vals)).


expand(true) ->
    true;
expand(false) ->
    false;
expand(null) ->
    null;
expand(undefined) ->
    undefined;
expand(defaultObject) ->
    {struct, []};
expand(Num) when is_number(Num) ->
    Num;
expand([]) ->
    "";
expand(Tuple) when is_tuple(Tuple) ->
    {array, lists:map(fun expand/1, tuple_to_list(Tuple))};
expand(Args = [{_, _}| _]) ->
    {struct, lists:map(fun({Key, Val}) -> {Key, expand(Val)} end,
		       Args)};
expand(Str) when is_list(Str) ->
    Str.		       

