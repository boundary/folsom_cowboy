### folsom_cowboy, a cowboy based wrapper for folsom

This is an application that exposes folsom metrics via http using cowboy. It is a port of [folsom_webmachine] to [cowboy].

#### installation

Just add the folsom_cowboy to your app, in addition to its dependencies: [cowboy], [folsom].

        %% It's not typical to do this manually, but here's how:
        > lists:map(fun application:start/1,
                [cowlib, ranch, cowboy, folsom, folsom_cowboy]).

You can configure the `port`, `transport`, `transport_options`, `num_acceptors`, `dispatch` and `enable_jsonp` in the `folsom_cowboy` application environment. This is typically done in your application's `sys.config`, but can also be specified on the command line:

        $ erl -pa ebin deps/*/ebin -folsom_cowboy port 8888

* `transport` is the cowboy transport module (`cowboy_tcp_transport` or `cowboy_ssl_transport`)
* `transport_options` is the options list to pass to that module (port is handled separately), such as `[{ip, {127, 0, 0, 1}}]`
* `num_acceptors` is the number of acceptor processes to start for the listening socket (default is `100`)
* `dispatch` is the `cowboy_http_module` dispatch tree, which you could use to change the URL scheme
* `enable_jsonp` if true enable a flavour of [JSON with padding][jsonp] response (see [below](#JSONP)), false by default

#### api

Query the list of available metrics:

        $ curl http://localhost:5565/_metrics

Query a specific metric:

        $ curl http://localhost:5565/_metrics/name

Query Erlang VM metrics:

        $ curl http://localhost:5565/_memory

Query Erlang VM stats:

        $ curl http://localhost:5565/_statistics

Query Erlang VM information:

        $ curl http://localhost:5565/_system

#### output

        $ curl http://localhost:5565/_metrics/a
        {"value":{"min":1,"max":1000,"mean":322.2,"median":100,"variance":185259.19999999998,"standard_deviation":430.4174717643325,"skewness":1.2670136514902162,"kurtosis":-1.2908313302242205,"percentile":{"75":500,"95":1000,"99":1000,"999":1000},"histogram":{"10":2,"20":0,"30":0,"50":0,"100":1,"200":0,"300":0,"400":0,"500":1,"1000":1,"99999999999999":0}}}


         $ curl http://localhost:5565/_metrics/test

         {"value":{"1303483997384193":{"event":"asdfasdf"}}}


         $ curl http://localhost:5565/_memory
         {"total":11044608,"processes":3240936,"processes_used":3233888,"system":7803672,"atom":532137,"atom_used":524918,"binary":696984,"code":4358030,"ets":385192}

#### JSONP

If enabled, you can request JSONP responses by including the query-string parameter:

        $ curl http://localhost:5565/_metrics?jsonp=AvailableMetrics
        AvailableMetrics=[]

The returned javascript code is an assignment rather than a function call, so the parameter must be a valid javascript identified, in particular it must be a string of letters, digits or underscores, otherwise a `400` error is returned.

[folsom_webmachine]: https://github.com/boundary/folsom_webmachine
[folsom]: https://github.com/boundary/folsom
[jsonp]: http://en.wikipedia.org/wiki/JSONP
