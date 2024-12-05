-module(geocoding_test).
-include_lib("eunit/include/eunit.hrl").

-define(PARIS, {48.857929, 2.346707}).
-define(NEW_YORK, {40.7630463, -73.973527}).

start() ->
  {ok, _} = geocoding:start_link().

stop() ->
  stopped = geocoding:do_stop().


start_stop_test_() ->
  [
   ?_assertMatch({ok,_}, geocoding:start_link()),
   ?_assertEqual(stopped, geocoding:do_stop())
  ].

reverse_test_() ->
  {setup,
   fun() ->
       start()
   end,
   fun(_) ->
       stop()
   end,
   fun(_) ->
       [
        % Verify that the 2 APIs of reverse return the same result
        ?_test(begin
                 Result1 = geocoding:reverse(?PARIS),
                 Result2 = geocoding:reverse(element(1, ?PARIS), element(2, ?PARIS)),
                 ?assertEqual(Result1, Result2)
               end),

        % Center of Paris
        ?_assertMatch({ok, {2988507, europe, fr, <<"Paris">>, Distance}} when Distance < 10000, geocoding:reverse(?PARIS)),
        % Parc monceau (Paris) but Levallois-Perret center is nearest than Paris center
        ?_assertMatch({ok, {2998975, europe, fr, <<"Levallois-Perret">>, Distance}} when Distance < 10000, geocoding:reverse(48.878995, 2.307327)),

        % Saint-Denis (France, 93)
        ?_assertMatch({ok, {2980916, europe, fr, <<"Saint-Denis">>, Distance}} when Distance < 10000, geocoding:reverse(48.927533, 2.358144)),

        % Beijing (China)
        ?_assertMatch({ok, {1816670, asia, cn, <<"Beijing">>, Distance}} when Distance < 10000, geocoding:reverse(39.916, 116.344)),

        % Jakarta (Indonesia)
        ?_assertMatch({ok, {1642911, asia, id, <<"Jakarta">>, Distance}} when Distance < 15000, geocoding:reverse(-6.115529, 106.780043)),

        % Moscow (Europe/Russia)
        ?_assertMatch({ok, {524901, europe, ru, <<"Moscow">>, Distance}} when Distance < 15000, geocoding:reverse(55.752391, 37.616879)),
        % Novosibirsk (Asia/Russia)
        ?_assertMatch({ok, {1496747, asia, ru, <<"Novosibirsk">>, Distance}} when Distance < 15000, geocoding:reverse(55.032852, 82.938213)),
        % Glazov (Europe/Russia) - west of Ural Mountains
        ?_assertMatch({ok, {561347, europe, ru,<<"Glazov">>, Distance}} when Distance < 1000, geocoding:reverse(58.138541, 52.663895)),
        % Nizhnyaya Tura (Asia/Russia) - east of Ural Mountains
        ?_assertMatch({ok, {520204, asia,ru,<<"Nizhnyaya Tura">>, Distance}} when Distance < 5000, geocoding:reverse(58.626655, 59.848197)),

        % Liku (Niue island, pacific)
        ?_assertMatch({ok, {4034831, oceania, wf, <<"Liku">>, Distance}} when Distance < 10000, geocoding:reverse(-13.28333, -176.13333))
       ]
   end
  }.

lookup_test_() ->
  {setup,
   fun() ->
       start()
   end,
   fun(_) ->
       stop()
   end,
   fun(_) ->
       [
        ?_assertMatch({ok,{2988507, {48.85341, 2.3488}, europe, 'FR', <<"Paris">>}}, geocoding:lookup('FR', <<"Paris">>)),

        % Lookup is caseless
        ?_assertMatch({ok,{2988507, {48.85341, 2.3488}, europe, 'FR', <<"Paris">>}}, geocoding:lookup('FR', <<"paris">>))
       ]
   end
  }.


distance_test_() ->
  [
   ?_assertMatch(5832947, geocoding:distance(?PARIS, ?NEW_YORK)),

   % 2 local points at Saint-Germain-en-Laye, France
   ?_assertMatch(2188, geocoding:distance({48.900666021262744, 2.066405553816917}, {48.89826633730442, 2.0961015091251927}))
  ].
