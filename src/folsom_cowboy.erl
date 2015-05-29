-module(folsom_cowboy).

-export([dispatch/0]).

dispatch() ->
    [{"/_health", folsom_cowboy_handler, [health]},
     {"/_memory", folsom_cowboy_handler, [memory]},
     {"/_metrics", folsom_cowboy_handler, [metrics]},
     {"/_metrics/[:metric_id]", folsom_cowboy_handler, [metric]},
     {"/_ping", folsom_cowboy_handler, [ping]},
     {"/_port", folsom_cowboy_handler, [port]},
     {"/_process", folsom_cowboy_handler, [process]},
     {"/_statistics", folsom_cowboy_handler, [statistics]},
     {"/_system", folsom_cowboy_handler, [system]},
     {"/_ets", folsom_cowboy_handler, [ets]},
     {"/_dets", folsom_cowboy_handler, [dets]}].
