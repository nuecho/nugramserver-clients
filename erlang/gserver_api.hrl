%%% File        : gserver_api.hrl
%%% Author      : Nu Echo Inc.
%%% Description : NuGram Hosted Server API data structures
%%
%% Copyright (C) 2009 Nu Echo Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% Structure that represents a session with NuGram Hosted Server
-record(gserver_session, {server, username, password, id}).

%% Structure that represents an instantiated grammar within NuGram Hosted Server
-record(gserver_grammar, {session, id, grammarUrl, interpreterUrl}).


