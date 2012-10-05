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
%%% File:      folsom_webmachine_metrics_handler.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% http metrics end point
%%% @end
%%%------------------------------------------------------------------


-module(folsom_cowboy_metrics_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    {ok, Req2} = get_request(Path, Req1),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

get_request(Path, Req) ->
    Id = lists:last(binary:split(Path, [<<"/">>], [trim,global])),
    case metric_exists(Id) of
        {true, Id1} ->
            cowboy_req:reply(200, [], mochijson2:encode(get_metric_data(Id1)), Req);
        {false, _} ->
            cowboy_req:reply(404, [], mochijson2:encode([{error, nonexistent_metric}]), Req);
        _ ->
            cowboy_req:reply(404, [], mochijson2:encode([{error, nonexistent_metric}]), Req)
    end.

get_metric_data(Id) ->
    case folsom_metrics:get_metric_info(Id) of
        [{_, [{type, histogram}]}] ->
            [{value, folsom_metrics:get_histogram_statistics(Id)}];
        _ ->
            [{value, folsom_metrics:get_metric_value(Id)}]
    end.

% @doc Return true if metric with key 'Id' exists, false otherwise
%
% Searches for a metric with 'Id' stored as a binary first and falls
% back to looking for an existing atom if no matching binary key was
% found.
metric_exists(Id) when is_list(Id) ->
    metric_exists(list_to_binary(Id));
metric_exists(Id) when is_binary(Id) ->
    case folsom_metrics:metric_exists(Id) of
        true  -> {true, Id};
        false ->
            try
                metric_exists(erlang:binary_to_existing_atom(Id, utf8))
            catch
                error:badarg -> {false, Id}
            end
    end;
metric_exists(Id) when is_atom(Id) ->
    {folsom_metrics:metric_exists(Id), Id}.


