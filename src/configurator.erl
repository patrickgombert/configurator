-module(configurator).
-export([paths/0, add_path/1]).
-export([set/2]).
-export([config/1]).

-define(CONFIGURATOR, configurator_srv).

paths() ->
  gen_server:call(?CONFIGURATOR, paths).

add_path(Path) when is_list(Path) ->
  gen_server:cast(?CONFIGURATOR, {add_path, Path}).

config(FileName) when is_list(FileName) ->
  gen_server:call(?CONFIGURATOR, {config, FileName}).

set(FileName, Config) ->
  gen_server:cast(?CONFIGURATOR, {set, FileName, Config}).

