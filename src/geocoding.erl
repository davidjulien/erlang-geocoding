%% @doc A module to identify location (continent, country, city) from latitude/longitude coordinates.
%% Data from geonames.org
-module(geocoding).
-vsn("1").
-behaviour(gen_server).

-export([
         distance/2,
         lookup/2,
         reverse/1,
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

%% 7 continents model (except we replace australia with oceania to include all pacific islands).
-type continent() :: antarctica | africa | asia | europe | north_america | oceania | south_america.

% 242 countries
-type country_iso3166() :: 'AD' | 'AE' | 'AF' | 'AG' | 'AI' | 'AL' | 'AM' | 'AO' | 'AQ' | 'AR' | 'AS' | 'AT' | 'AU' | 'AW' | 'AX' | 'AZ' | 'BA' | 'BB' | 'BD' | 'BE' | 'BF' | 'BG' | 'BH' | 'BI' | 'BJ' | 'BL' | 'BM' | 'BN' | 'BO' | 'BQ' | 'BR' | 'BS' | 'BT' | 'BW' | 'BY' | 'BZ' | 'CA' | 'CD' | 'CF' | 'CG' | 'CH' | 'CI' | 'CK' | 'CL' | 'CM' | 'CN' | 'CO' | 'CR' | 'CU' | 'CV' | 'CW' | 'CX' | 'CY' | 'CZ' | 'DE' | 'DJ' | 'DK' | 'DM' | 'DO' | 'DZ' | 'EC' | 'EE' | 'EG' | 'EH' | 'ER' | 'ES' | 'ET' | 'FI' | 'FJ' | 'FK' | 'FM' | 'FO' | 'FR' | 'GA' | 'GB' | 'GD' | 'GE' | 'GF' | 'GG' | 'GH' | 'GI' | 'GL' | 'GM' | 'GN' | 'GP' | 'GQ' | 'GR' | 'GT' | 'GU' | 'GW' | 'GY' | 'HK' | 'HN' | 'HR' | 'HT' | 'HU' | 'ID' | 'IE' | 'IL' | 'IM' | 'IN' | 'IQ' | 'IR' | 'IS' | 'IT' | 'JE' | 'JM' | 'JO' | 'JP' | 'KE' | 'KG' | 'KH' | 'KI' | 'KM' | 'KN' | 'KP' | 'KR' | 'KW' | 'KY' | 'KZ' | 'LA' | 'LB' | 'LC' | 'LI' | 'LK' | 'LR' | 'LS' | 'LT' | 'LU' | 'LV' | 'LY' | 'MA' | 'MC' | 'MD' | 'ME' | 'MF' | 'MG' | 'MH' | 'MK' | 'ML' | 'MM' | 'MN' | 'MO' | 'MP' | 'MQ' | 'MR' | 'MS' | 'MT' | 'MU' | 'MV' | 'MW' | 'MX' | 'MY' | 'MZ' | 'NA' | 'NC' | 'NE' | 'NF' | 'NG' | 'NI' | 'NL' | 'NO' | 'NP' | 'NR' | 'NU' | 'NZ' | 'OM' | 'PA' | 'PE' | 'PF' | 'PG' | 'PH' | 'PK' | 'PL' | 'PM' | 'PR' | 'PS' | 'PT' | 'PW' | 'PY' | 'QA' | 'RE' | 'RO' | 'RS' | 'RU' | 'RW' | 'SA' | 'SB' | 'SC' | 'SD' | 'SE' | 'SG' | 'SH' | 'SI' | 'SJ' | 'SK' | 'SL' | 'SM' | 'SN' | 'SO' | 'SR' | 'SS' | 'ST' | 'SV' | 'SX' | 'SY' | 'SZ' | 'TC' | 'TD' | 'TG' | 'TH' | 'TJ' | 'TK' | 'TL' | 'TM' | 'TN' | 'TO' | 'TR' | 'TT' | 'TV' | 'TW' | 'TZ' | 'UA' | 'UG' | 'US' | 'UY' | 'UZ' | 'VA' | 'VC' | 'VE' | 'VG' | 'VI' | 'VN' | 'VU' | 'WF' | 'WS' | 'XK' | 'YE' | 'YT' | 'ZA' | 'ZM' | 'ZW'.

% Geonameid, to retrieve easily all data related to a location
-type geonameid() :: non_neg_integer().

% City name
-type city_name() :: unicode:unicode_binary().

% Coordinates latitude/longitude
-type coordinates() :: {float(), float()}.

% Full city info in our database
-type city_info() :: {geonameid(), coordinates(), continent(), country_iso3166(), city_name()}.

-record(state,
        {
         countrycity_tree :: gb_trees:tree({country_iso3166(), city_name()}, city_info()),
         port :: port() | undefined
        }).


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
-spec reverse(float(), float()) -> {ok, {continent(), country_iso3166(), unicode:unicode_binary(), float()}} | {error, any()}.
reverse(Latitude, Longitude) ->
  reverse({Latitude, Longitude}).

%%% @doc Find a city from latitude/longitude coordinates
-spec reverse({float(), float()}) -> {ok, {continent(), country_iso3166(), unicode:unicode_binary(), float()}} | {error, any()}.
reverse({Latitude, Longitude}) ->
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

%%% @doc Lookup coordinates from country code and city name
-spec lookup(country_iso3166(), city_name()) -> {ok, {geonameid(), coordinates(), continent(), country_iso3166(), city_name()}}.
lookup(CountryISO3166, CityName) ->
  gen_server:call(?MODULE, {lookup, CountryISO3166, CityName}, ?TIMEOUT).


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
%% @doc Start the application server.
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
  CountryCityTree = init_tree_from_file(),
  {ok, #state{port = undefined, countrycity_tree = CountryCityTree}}.

%% @doc Handle a synchronous message.
handle_call(do_stop, _From, #state{port = Port} = State) ->
  case Port of
    undefined -> ok;
    _ -> close_port_to_external_driver(Port)
  end,
  {stop, normal, stopped, State#state{port = undefined}};
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
  {reply, Reply, NewState};
handle_call({lookup, CountryISO3166, CityName}, _From, #state{countrycity_tree = CountryCityTree} = State) ->
  Result = case gb_trees:lookup({CountryISO3166, string:lowercase(CityName)}, CountryCityTree) of
    none -> none;
    {value, V} -> {ok, V}
  end,
  {reply, Result, State}.

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
-spec do_reverse(port(), integer() | float(), integer() | float()) -> {continent(), country_iso3166(), unicode:unicode_binary(), float()}.
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

-spec init_tree_from_file() -> gb_trees:tree({country_iso3166(), unicode:unicode_binary()}, city_info()).
init_tree_from_file() ->
  Path = path_to_external_driver(),
  DataFilePath = filename:join(Path, ?CITIES_DATA_FILE),
  {ok, Data} = file:read_file(DataFilePath),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  lists:foldl(fun(Line, AccTree) ->
                  if Line =:= <<>> -> % Last line is empty
                       AccTree;
                     true ->
                       [GeonameIdStr, LatitudeStr, LongitudeStr, ContinentStr, CountryCodeStr, CityName] = binary:split(Line, [<<"\t">>], [global]),
                       try
                         GeonameId = binary_to_integer(GeonameIdStr),
                         Latitude = to_float(LatitudeStr),
                         Longitude = to_float(LongitudeStr),
                         Continent = binary_to_atom(ContinentStr, 'utf8'),
                         CountryCode = binary_to_atom(CountryCodeStr, 'utf8'),
                         gb_trees:insert({CountryCode, string:lowercase(CityName)}, {GeonameId, {Latitude, Longitude}, Continent, CountryCode, CityName}, AccTree)
                       catch
                         error:{key_exists, _} ->
                           % In some countries, you may have multiple cities with same name
                           AccTree;
                         E:V ->
                           error_logger:info_msg("Unable to insert ~s:~10000p", [Line, {E,V}]),
                           AccTree
                       end
                  end
              end, gb_trees:empty(), Lines).

to_float(Str) ->
  try
    binary_to_float(Str)
  catch
    error:badarg ->
      binary_to_integer(Str) * 1.0
  end.

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
