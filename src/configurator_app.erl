-module(configurator_app).
-export([start/2, stop/1]).
-behaviour(application).

start(_StartType, _StartArgs) ->
    configurator_sup:start_link().

stop(_State) ->
    ok.

