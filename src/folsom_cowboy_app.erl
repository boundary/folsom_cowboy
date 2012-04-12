%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_cowboy_app.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

%% @doc Application module for folsom_cowboy.

-module(folsom_cowboy_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("folsom_cowboy.hrl").

start(_Type, _Args) ->
    application:start(cowboy),

    cowboy:start_listener(folsom_cowboy_listener, 100,
                          cowboy_tcp_transport, [{port, ?PORT}],
                          cowboy_http_protocol, [{dispatch, ?DISPATCH}]
                         ),

    folsom_cowboy_sup:start_link().

stop(_State) ->
    ok.
