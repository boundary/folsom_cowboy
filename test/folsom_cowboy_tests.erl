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
%%% File:      folsom_cowboy_tests.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_cowboy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(APPS, [cowlib, ranch, cowboy, bear, xmerl, folsom, folsom_cowboy]).

start() ->
    [ok = application:start(App) || App <- ?APPS].

stop(_) ->
    [ok = application:stop(App) || App <- lists:reverse(?APPS)].


run_test_() ->
    {setup, fun start/0, fun stop/1,
     [{"create_metrics",
       fun folsom_erlang_checks:create_metrics/0},
      {"populate metrics",
       {timeout, 30, fun folsom_erlang_checks:populate_metrics/0}},
      {"http checks",
       {setup,
        fun ibrowse:start/0,
        fun (_) -> ibrowse:stop() end,
        [{timeout, 60, fun folsom_http_checks:run/0}]}}]}.

run_with_jsonp_test_() ->
    {setup,
     fun() ->
        application:set_env(folsom_cowboy, enable_jsonp, true),
        start()
     end,

     fun(S) ->
        application:unset_env(folsom_cowboy, enable_jsonp),
        stop(S)
     end,

     [{"create_metrics",
       fun folsom_erlang_checks:create_metrics/0},
      {"populate metrics",
       {timeout, 30, fun folsom_erlang_checks:populate_metrics/0}},
      {"jsonp checks",
       {setup,
        fun ibrowse:start/0,
        fun (_) -> ibrowse:stop() end,
        [{timeout, 60, fun folsom_http_checks:run_jsonp/0}]}}]}.

