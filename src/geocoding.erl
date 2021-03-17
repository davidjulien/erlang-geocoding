%% @doc A module to identify location (continent, country, city) from latitude/longitude coordinates.
%% Data from geonames.org
-module(geocoding).
-vsn("1").
-behaviour(gen_server).

-export([
         distance/2,
         reverse/2
        ]).

%% Access from supervisor.
-export([start_link/0]).

%% Access from application.
-export([do_stop/0]).

%% gen_server API.
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% ========================================================================= %%
%% RECORDS AND TYPES
%% ========================================================================= %%

-record(state,
        {
         port :: port() | undefined
        }).

%% 7 continents model (except we replace australia with oceania to include all pacific islands).
-type continent() :: antarctica | africa | asia | europe | north_america | oceania | south_america.

%% Maybe list all ISO-3166 country codes
-type country() :: atom().

%% ========================================================================= %%
%% CONSTANTS
%% ========================================================================= %%

-define(TIMEOUT, 20000).
-define(PORT_TIMEOUT, 5100).

-define(APPLICATION, geocoding).
-define(EXTERNAL_DRIVER, "geocoding_drv").
-define(CITIES_DATA_FILE, "cities.txt").

-define(DEG_TO_RAD, 0.017453292519943295769236907684886).
-define(EARTH_RADIUS_IN_METERS, 6372797.560856).

%% ========================================================================= %%
%% API
%% ========================================================================= %%

%%% @doc Find a city from latitude/longitude coordinates
-spec reverse(float(), float()) -> {ok, {continent(), country(), unicode:unicode_binary(), float()}} | {error, any()}.
reverse(Latitude, Longitude) ->
  gen_server:call(?MODULE, {reverse, Latitude, Longitude}, ?TIMEOUT).

%%% @doc Compute distance between 2 coordinates (result in meters)
-spec distance({float(), float()}, {float(), float()}) -> integer().
distance({FromLatitude, FromLongitude}, {ToLatitude, ToLongitude}) ->
  LatitudeArc = (FromLatitude - ToLatitude) * ?DEG_TO_RAD,
  LongitudeArc = (FromLongitude - ToLongitude) * ?DEG_TO_RAD,
  LatitudeH = math:sin(LatitudeArc / 2.0),
  LatitudeH2 = LatitudeH * LatitudeH,
  LongitudeH = math:sin(LongitudeArc / 2.0),
  LongitudeH2 = LongitudeH * LongitudeH,
  Tmp = math:cos(FromLatitude*?DEG_TO_RAD) * math:cos(ToLatitude * ?DEG_TO_RAD),
  round(?EARTH_RADIUS_IN_METERS * 2.0 * math:asin(math:sqrt(LatitudeH2 + Tmp*LongitudeH2))).


%% ========================================================================= %%
%% supervisor API
%% ========================================================================= %%

%%--------------------------------------------------------------------
%% @doc Send a stop message to the server.
%% This method is called from the supervisor.
%% 
-spec do_stop() -> stopped.
do_stop() ->
  gen_server:call(?MODULE, do_stop, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Start the link grammar parser application server.
%% This method is called from the supervisor.
%%
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ========================================================================= %%
%% gen_server API
%% ========================================================================= %%

%%--------------------------------------------------------------------
%% @doc gen_server's init callback.
%% 
-spec init(list()) -> {ok, #state{}}.
init(_Args) ->
  {ok, #state{port = undefined}}.

%% @doc Handle a synchronous message.
handle_call(do_stop, _From, #state{port = Port} = _State) ->
  case Port of
    undefined -> ok;
    _ -> close_port_to_external_driver(Port)
  end,
  {stop, normal, stopped, #state{port = undefined}};
handle_call({reverse, Latitude, Longitude}, _From, #state{port = Port_0} = State) ->
  Port_1 = case Port_0 of 
             undefined -> open_port_to_external_driver();
             _ -> Port_0
           end,
  {Reply, Port_2} = try
                      Res = do_reverse(Port_1, Latitude, Longitude),
                      {{ok, Res}, Port_1}
                    catch
                      E:V ->
                        error_logger:info_msg("~p : catch ~1000p:~1000p in lookup for ~p", [?MODULE, E,V, {Latitude, Longitude}]),
                        close_port_to_external_driver(Port_1),
                        {{error, V}, undefined}
                    end,
  NewState = State#state{port = Port_2},
  {reply, Reply, NewState}.

%% @doc Handle an asynchronous message.
handle_cast(none, State) ->
  {noreply, State}.

%% @doc handle system messages.
handle_info(_Msg, State) ->
  {noreply, State}.

%% @doc handle code change.
-spec code_change(string() | {down, string()}, any(), any()) -> {ok, #state{}}.
code_change(_Vsn, _State, _Extra) ->
  error_logger:info_msg("[~p] Unknown code change~n", [?MODULE]),
  {ok, _State}.

%% @doc handle termination.
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, #state{port = Port}) ->
  case erlang:port_info(Port) of
    undefined -> ok;
    _ -> port_close(Port)
  end.

%% ========================================================================= %%
%% Private functions
%% ========================================================================= %%

%% @doc Reverse latitude/longitude coordinates to continent/country/city and distance between provided coordinates and official cooordinates of identified location.
-spec do_reverse(port(), float(), float()) -> {continent(), country(), unicode:unicode_binary(), float()}.
do_reverse(Port, Latitude, Longitude) ->
  LatitudeF = if
                is_integer(Latitude) -> Latitude * 1.0;
                true -> Latitude
              end,
  LongitudeF = if
                 is_integer(Longitude) -> Longitude * 1.0;
                 true -> Longitude
               end,
  port_command(Port, [float_to_list(LatitudeF), $\t, float_to_list(LongitudeF), $\n]),
  Line = receive
           {Port, Content} -> 
             case Content of
               {data, Line0} -> Line0;
               eof ->
                 error_logger:error_msg("Unexpected eof !~n", []),
                 throw({error, eof});
               {exit_status, ExitStatus} ->
                 error_logger:error_msg("Unexpected exit status !~n~p~n", [ExitStatus]),
                 throw({error, {exit_status, ExitStatus}});
               Msg ->
                 error_logger:error_msg("Unexpected message from port !~n~p~n", [Msg]),
                 throw({error, unexpected_msg})
             end
         after
           ?PORT_TIMEOUT -> throw(timeout)
         end,
  [DistanceStr, ContinentStr, <<CountryC1, CountryC2>>, NameStr] = re:split(Line, <<"\t">>, [unicode, {return, binary}]),
  {
   binary_to_atom(ContinentStr,'utf8'),
   list_to_atom(string:to_lower([CountryC1, CountryC2])),
   NameStr,
   list_to_float(binary_to_list(DistanceStr))
  }.                



%% ========================================================================= %%
%% Communication with driver
%% ========================================================================= %%

%% @doc Open the port to the external driver.
open_port_to_external_driver() ->
  Path = path_to_external_driver(),
  Cmd = "\"" ++ filename:join(Path, ?EXTERNAL_DRIVER) ++ "\" \"" ++ filename:join(Path, ?CITIES_DATA_FILE) ++ "\"",
  Port = open_port({spawn, Cmd}, [{packet, 2}, exit_status, binary, use_stdio, eof]),
  process_flag(trap_exit, true),
  Port.

%% @doc Close the port.
-spec close_port_to_external_driver(port()) -> ok.
close_port_to_external_driver(Port) ->
  case erlang:port_info(Port) of
    undefined -> ok;
    _ -> port_close(Port)
  end,
  ok.

%% @doc Get the path to the external driver and resources
-spec path_to_external_driver()->string().
path_to_external_driver() ->
  case code:priv_dir(?MODULE) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path ->
      Path
  end.
