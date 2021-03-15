%% @doc Application for the geocoding application.

-module(geocoding_app).
-behaviour(application).

%% application API
-export([start/2, stop/1, prep_stop/1]).

%% @doc Start the application.
-spec start(normal | {takeover, node()} | {failover, node()}, any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _StartArgs) ->
  geocoding_sup:start_link().

%% @doc Actually stop the application.
-spec prep_stop(any()) -> any().
prep_stop(_State) ->
  geocoding:do_stop().

%% @doc Method called when the application process has been stopped.
-spec stop(any()) -> ok.
stop(_State) ->
  ok.
