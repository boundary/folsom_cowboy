-define(DISPATCH, [
                   {'_', [
                          {[<<"_health">>], folsom_cowboy_health_handler, []},
                          {[<<"_memory">>], folsom_cowboy_memory_handler, []},
                          {[<<"_metrics">>], folsom_cowboy_metrics_list_handler, []},
                          {[<<"_metrics">>, '_'], folsom_cowboy_metrics_handler, []},
                          {[<<"_ping">>], folsom_cowboy_ping_handler, []},
                          {[<<"_port">>], folsom_cowboy_port_handler, []},
                          {[<<"_process">>], folsom_cowboy_process_handler, []},
                          {[<<"_statistics">>], folsom_cowboy_statistics_handler, []},
                          {[<<"_system">>], folsom_cowboy_system_handler, []},
                          {[<<"_ets">>], folsom_cowboy_ets_handler, []},
                          {[<<"_dets">>], folsom_cowboy_dets_handler, []}
                         ]
                   }
                  ]).

-define(PORT, 5565).
