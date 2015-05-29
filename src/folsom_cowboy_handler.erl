-module(folsom_cowboy_handler).
-behaviour(cowboy_http_handler).

-export([init/3,
         handle/2,
         terminate/3]).

init({_Any, http}, Req, [Key]) ->
    {ok, Req, Key}.

handle(Req, Key) ->
    {ok, Req1} = get_metric(Key, Req),
    {ok, Req1, Key}.

terminate(_Reason, _Req, _State) ->
    ok.

reply(Req, undefined) ->
    cowboy_req:reply(404, [], [], Req);
reply(Req, Metric) ->
    cowboy_req:reply(200, [], jsx:encode(Metric), Req).

get_metric(health, Req) ->
    {M, F, A} = application:get_env(folsom_cowboy, health, {erlang, node, []}),
    Result = erlang:apply(M, F, A),
    reply(Req, [{<<"health">>, Result}]);
get_metric(memory, Req) ->
    reply(Req, folsom_vm_metrics:get_memory());
get_metric(metrics, Req) ->
    {Info, Req1} = qs_val(<<"info">>, Req),
    reply(Req1, get_metrics(Info));
get_metric(metric, Req) ->
    {Id, Req1} = binding(metric_id, Req),
    {MetricExists, Id1} = metric_exists(Id),
    reply(Req1, get_metric_data(MetricExists, Id1));
get_metric(ping, Req) ->
    reply(Req, <<"pong">>);
get_metric(port, Req) ->
    reply(Req, folsom_vm_metrics:get_port_info());
get_metric(process, Req) ->
    reply(Req, folsom_vm_metrics:get_process_info());
get_metric(statistics, Req) ->
    reply(Req, folsom_vm_metrics:get_statistics());
get_metric(system, Req) ->
    reply(Req, folsom_vm_metrics:get_system_info());
get_metric(ets, Req) ->
    reply(Req, folsom_vm_metrics:get_ets_info());
get_metric(dets, Req) ->
    reply(Req, folsom_vm_metrics:get_dets_info());
get_metric(_, Req) ->
    reply(Req, undefined).

get_metric_data(false, _Id) ->
    undefined;
get_metric_data(_, Id) ->
    case folsom_metrics:get_metric_info(Id) of
        [{_, [{type, histogram} | _]}] ->
            [{value, folsom_metrics:get_histogram_statistics(Id)}];
        _ ->
            [{value, folsom_metrics:get_metric_value(Id)}]
    end.

get_metrics(<<"true">>) ->
    [{M, proplists:delete(tags, List)} ||
        {M, List} <- folsom_metrics:get_metrics_info()];
get_metrics(_) ->
    folsom_metrics:get_metrics().

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

binding(Key, Req) ->
    cowboy_req:binding(Key, Req).

qs_val(Key, Req) ->
    cowboy_req:qs_val(Key, Req).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ranch_proxy_ssl_test_() ->
    {setup,
     fun start_suite/0,
     fun stop_suite/1,
     fun(_) ->
             {foreach, fun setup_test/0,
              fun teardown_test/1,
              [{with, [T]} || T <- [fun ping_test/1,
                                    fun port_test/1,
                                    fun process_test/1,
                                    fun statistics_test/1,
                                    fun system_test/1,
                                    fun ets_test/1,
                                    fun health_test/1,
                                    fun memory_test/1,
                                    fun metrics_info_test/1,
                                    fun metrics_no_info_test/1,
                                    fun metric_info_counter_test/1,
                                    fun metric_info_histogram_test/1
                                   ]]}
     end}.

start_suite() ->
    application:start(meck),
    {ok, Apps} = application:ensure_all_started(folsom),
    #{apps => Apps}.

stop_suite(#{apps := Apps}) ->
    application:stop(meck),
    lists:foreach(fun(App) ->
                          application:stop(App)
                  end, Apps).

setup_test() ->
    catch folsom_metrics:new_counter(test_counter),
    meck:new(cowboy_req, [non_strict]),
    meck:expect(cowboy_req, reply, fun(Code, Headers, Body, _) ->
                                           {ok, {Code, Headers, Body}}
                                   end).

teardown_test(_) ->
    lists:foreach(fun(Metric) ->
                          catch folsom_metrics:delete_metric(Metric)
                  end, folsom_metrics:get_metrics()),
    meck:unload(cowboy_req),
    ok.

metric_info_counter_test(_) ->
    meck:expect(cowboy_req, binding,
                fun(metric_id, Req) ->
                        {test_counter, Req}
                end),
    folsom_metrics:notify({test_counter, {inc, 10}}),
    {ok, {Code, _, Body}, metric} = handle(req, metric),
    ?assertEqual(Code, 200),
    ?assertEqual([{<<"value">>, 10}], jsx:decode(Body)).

metric_info_histogram_test(_) ->
    meck:expect(cowboy_req, binding,
                fun(metric_id, Req) ->
                        {test_histogram, Req}
                end),
    folsom_metrics:new_histogram(test_histogram, uniform, 10),
    lists:foreach(fun(_) ->
                          folsom_metrics:notify({test_histogram, 12})
                  end, lists:seq(0,100)),
    {ok, {Code, _, Body}, metric} = handle(req, metric),
    ?assertEqual(Code, 200),
    ?assertEqual([{<<"value">>, [{<<"arithmetic_mean">>,12.0},
                  {<<"geometric_mean">>,12.000000000000005},
                  {<<"harmonic_mean">>,12.0},
                  {<<"histogram">>,[{<<"12">>,10}]},
                  {<<"kurtosis">>,0.0},
                  {<<"n">>,10},
                  {<<"max">>,12},
                  {<<"median">>,12},
                  {<<"min">>,12},
                  {<<"percentile">>,
                   [{<<"50">>,12},{<<"75">>,12},{<<"95">>,12},{<<"99">>,12},{<<"999">>,12}]},
                  {<<"skewness">>,0.0},
                  {<<"standard_deviation">>,0.0},
                  {<<"variance">>,0.0}]}], jsx:decode(Body)).

metrics_info_test(_) ->
    meck:expect(cowboy_req, qs_val,
                fun(<<"info">>, Req) ->
                        {<<"true">>, Req}
                end),
    {ok, {Code, _, Body}, metrics} = handle(req, metrics),
    ?assertEqual(Code, 200),
    ?assertEqual([{<<"test_counter">>,[{<<"type">>,<<"counter">>}]}],
                 jsx:decode(Body)).

metrics_no_info_test(_) ->
    meck:expect(cowboy_req, qs_val,
                fun(<<"info">>, Req) ->
                        {undefined, Req}
                end),
    {ok, {Code, _, Body}, metrics} = handle(req, metrics),
    ?assertEqual(Code, 200),
    ?assertEqual([<<"test_counter">>], jsx:decode(Body)).

ping_test(_) ->
    ?assertMatch({ok, {200, [], <<"\"pong\"">>}, ping}, handle(req, ping)).

port_test(_) ->
    {ok, {Code, _, Body}, port} = handle(req, port),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from ports
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
                   binary:match(Key, <<"Port">>) =/= nomatch],
    ?assertEqual(length(Body1), length(Keys)).

process_test(_) ->
    {ok, {Code, _, Body}, process} = handle(req, process),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from processes
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
                   is_pid(list_to_pid(binary_to_list(Key))) == true],
    ?assertEqual(length(Body1), length(Keys)).

statistics_test(_) ->
    {ok, {Code, _, Body}, statistics} = handle(req, statistics),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from statistics
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
           lists:member(Key, [<<"context_switches">>,<<"garbage_collection">>,
                              <<"io">>, <<"reductions">>,<<"run_queue">>,
                              <<"runtime">>,<<"wall_clock">>])],
    ?assertEqual(length(Body1), length(Keys)).

system_test(_) ->
    {ok, {Code, _, Body}, system} = handle(req, system),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from system
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
           lists:member(Key, [<<"allocated_areas">>,<<"allocator">>,
                              <<"alloc_util_allocators">>,<<"build_type">>,
                              <<"c_compiler_used">>,<<"check_io">>,
                              <<"compat_rel">>,<<"cpu_topology">>,<<"creation">>,
                              <<"debug_compiled">>,<<"dist">>,<<"dist_ctrl">>,
                              <<"driver_version">>,<<"elib_malloc">>,
                              <<"dist_buf_busy_limit">>,<<"garbage_collection">>,
                              <<"heap_sizes">>,<<"heap_type">>,<<"info">>,
                              <<"kernel_poll">>,<<"loaded">>,
                              <<"logical_processors">>,<<"logical_processors_available">>,
                              <<"logical_processors_online">>,<<"machine">>,<<"modified_timing_level">>,
                              <<"multi_scheduling">>,<<"multi_scheduling_blockers">>,<<"otp_release">>,
                              <<"port_count">>,<<"process_count">>,<<"process_limit">>,
                              <<"scheduler_bind_type">>,<<"scheduler_bindings">>,<<"scheduler_id">>,
                              <<"schedulers">>,<<"schedulers_online">>,<<"smp_support">>,
                              <<"system_version">>,<<"system_architecture">>,<<"threads">>,
                              <<"thread_pool_size">>,<<"trace_control_word">>,<<"update_cpu_info">>,
                              <<"version">>,<<"wordsize">>])],
    ?assertEqual(length(Body1), length(Keys)).

ets_test(_) ->
    {ok, {Code, _, Body}, ets} = handle(req, ets),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from ets
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
                   ets:info(to_table_name(Key)) =/= undefined],
    ?assertEqual(length(Body1), length(Keys)).

health_test(_) ->
    {ok, {Code, _, Body}, health} = handle(req, health),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from statistics
    [{<<"health">>, NodeName}] = jsx:decode(Body),
    ?assertEqual(NodeName, list_to_binary(atom_to_list(erlang:node()))).

memory_test(_) ->
    {ok, {Code, _, Body}, memory} = handle(req, memory),
    ?assertEqual(Code, 200),
    % Check that it looks like something that could come from statistics
    Body1 = jsx:decode(Body),
    Keys = [Key || {Key, _} <- Body1,
                   lists:member(Key, [<<"total">>, <<"processes">>,
                                      <<"processes_used">>, <<"system">>,
                                      <<"atom">>, <<"atom_used">>, <<"binary">>,
                                      <<"code">>, <<"ets">>])],
    ?assertEqual(length(Body1), length(Keys)).

to_table_name(Key) ->
    try list_to_integer(binary_to_list(Key)) of
        Tid ->
            Tid
    catch
        _:_ ->
            list_to_existing_atom(binary_to_list(Key))
    end.

-endif.
