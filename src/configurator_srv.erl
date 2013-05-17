-module(configurator_srv).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, [[], dict:new()]}.

handle_call(paths, _Input, State) ->
  [Paths, _] = State,
  {reply, Paths, State};

handle_call({config, FileName}, _Input, [Paths, Configs]) ->
  Response = dict:find(FileName, Configs),
  case Response of
    {ok, Config} -> {reply, Config, [Paths, Configs]};
    error ->
      Status = find_config(Paths, FileName),
      case Status of
        not_found -> {reply, not_found, [Paths, Configs]};
        Config ->
          NewConfigs = dict:store(FileName, Config, Configs),
          {reply, Config, [Paths, NewConfigs]}
      end
  end.

handle_cast({add_path, Path}, [Paths, Configs]) ->
  AppendedPaths = Paths ++ [Path],
  {noreply, [AppendedPaths, Configs]};

handle_cast({set, FileName, Config}, [Paths, Configs]) ->
  NewConfigs = dict:store(FileName, Config, Configs),
  {noreply, [Paths, NewConfigs]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

find_config(Paths, FileName) ->
  ValidPaths = lists:filter(fun(Path) -> filelib:is_regular(Path ++ FileName) end, Paths),
  case length(ValidPaths) of
    0 -> not_found;
    _ ->
      Result = file:consult(lists:nth(1, ValidPaths) ++ FileName),
      case Result of
        {ok, Config} -> Config;
        {error, _} -> bad_config
      end
  end.

