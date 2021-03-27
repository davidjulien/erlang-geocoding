#!/usr/bin/env escript
%%! -s -boot start_inets

-mode(native). % increase efficiency 45s -> 7s

main(Parameters) ->
  % inets is mandatory to use httpc
  application:start(inets),

  io:format("[~p] Fetch data...\n", [calendar:universal_time()]),
  {ok, {_, _, Data_str}} = httpc:request(get, {"http://download.geonames.org/export/dump/cities500.zip", []}, [], []),

  io:format("[~p] Unzipping data...\n", [calendar:universal_time()]),
  {ok, [{_, Cities500_str}]} = zip:unzip(list_to_binary(Data_str), [memory]),

  io:format("[~p] Split data...\n", [calendar:universal_time()]),
  Lines = string:tokens(binary_to_list(Cities500_str), "\n"),
  NbrLines = length(Lines),

  io:format("[~p] Analyze ~B entries...\n", [calendar:universal_time(), NbrLines]),
  Entries = [ analyze_line(Line) || Line <- Lines ],
  SortedEntries = lists:keysort(1, Entries),

  io:format("[~p] Write entries in temporary files\n", [calendar:universal_time()]),
  {ok, F} = file:open("priv/cities.txt.new", [write]),
  Total = lists:foldl(
    fun
      ({0}, Count) -> Count;  % Skip discarded entry
      ({_, Entry}, Count) ->
        Str = string:join(Entry, "\t"),
        ok = file:write(F, [Str, "\n"]),
        Count + 1
    end, 0, SortedEntries),
  ok = file:close(F),
  io:format("[~p] Write done (~B entries)\n", [calendar:universal_time(), Total]),

  % Replace cities.txt file ?
  case lists:member("--replace", Parameters) of
    false -> ok;
    true ->
      io:format("[~p] Replace file\n", [calendar:universal_time()]),
      ok = file:rename("priv/cities.txt", "priv/cities.txt.old"),
      ok = file:rename("priv/cities.txt.new", "priv/cities.txt")
  end.

analyze_line(Line) ->
  [GeonameIdStr, NameStr, _AsciiNameStr, _AlternateNamesStr, LatitudeStr, LongitudeStr, _FeatureClassStr, _FeatureCodeStr, CountryCodeStr, _CountryCode2Str,
   _Admin1CodeStr, _Admin2CodeStr, _Admin3CodeStr, _Admin4CodeStr, PopulationStr, _ElevationStr, _DemStr, TimezoneStr, _ModificationDateStr] = string:split(Line, "\t", all),
  GeonameId = list_to_integer(GeonameIdStr),
  Population = list_to_integer(PopulationStr),
  if Population < 500 -> {0};
     true ->
       Continent = to_continent(CountryCodeStr, TimezoneStr),
       {GeonameId, [GeonameIdStr, LatitudeStr, LongitudeStr, Continent, CountryCodeStr, NameStr]}
  end.

to_continent("SJ", _) -> "europe";

to_continent(_, "Atlantic/Azores") -> "europe";
to_continent(_, "Atlantic/Faroe") -> "europe";
to_continent(_, "Atlantic/Reykjavik") -> "europe";

to_continent(_, "Atlantic/Canary") -> "africa";
to_continent(_, "Atlantic/Cape_Verde") -> "africa";
to_continent(_, "Atlantic/Madeira") -> "africa";
to_continent(_, "Atlantic/St_Helena") -> "africa";
to_continent(_, "Indian/Comoro") -> "africa";
to_continent(_, "Indian/Mayotte") -> "africa";
to_continent(_, "Indian/Reunion") -> "africa";
to_continent(_, "Indian/Mauritius") -> "africa";
to_continent(_, "Indian/Antananarivo") -> "africa";
to_continent(_, "Indian/Mahe") -> "africa";

to_continent(_, "Atlantic/Bermuda") -> "north_america";
to_continent("AG", "America" ++ _) -> "north_america";
to_continent("AI", "America" ++ _) -> "north_america";
to_continent("AW", "America" ++ _) -> "north_america";
to_continent("BB", "America" ++ _) -> "north_america";
to_continent("BL", "America" ++ _) -> "north_america";
to_continent("BQ", "America" ++ _) -> "north_america";
to_continent("BS", "America" ++ _) -> "north_america";
to_continent("BZ", "America" ++ _) -> "north_america";
to_continent("CA", "America" ++ _) -> "north_america";
to_continent("CR", "America" ++ _) -> "north_america";
to_continent("CU", "America" ++ _) -> "north_america";
to_continent("CW", "America" ++ _) -> "north_america";
to_continent("DM", "America" ++ _) -> "north_america";
to_continent("DO", "America" ++ _) -> "north_america";
to_continent("GD", "America" ++ _) -> "north_america";
to_continent("GL", "America" ++ _) -> "north_america";
to_continent("GP", "America" ++ _) -> "north_america";
to_continent("GT", "America" ++ _) -> "north_america";
to_continent("HN", "America" ++ _) -> "north_america";
to_continent("HT", "America" ++ _) -> "north_america";
to_continent("JM", "America" ++ _) -> "north_america";
to_continent("KN", "America" ++ _) -> "north_america";
to_continent("KY", "America" ++ _) -> "north_america";
to_continent("LC", "America" ++ _) -> "north_america";
to_continent("MF", "America" ++ _) -> "north_america";
to_continent("MQ", "America" ++ _) -> "north_america";
to_continent("MS", "America" ++ _) -> "north_america";
to_continent("MX", "America" ++ _) -> "north_america";
to_continent("NI", "America" ++ _) -> "north_america";
to_continent("PA", "America" ++ _) -> "north_america";
to_continent("PM", "America" ++ _) -> "north_america";
to_continent("PR", "America" ++ _) -> "north_america";
to_continent("SV", "America" ++ _) -> "north_america";
to_continent("SX", "America" ++ _) -> "north_america";
to_continent("TC", "America" ++ _) -> "north_america";
to_continent("TT", "America" ++ _) -> "north_america";
to_continent("US", "America" ++ _) -> "north_america";
to_continent("VC", "America" ++ _) -> "north_america";
to_continent("VG", "America" ++ _) -> "north_america";
to_continent("VI", "America" ++ _) -> "north_america";

to_continent(_, "Atlantic/Stanley") -> "south_america";
to_continent(_, "Pacific/Galapagos") -> "south_america";
to_continent("AR", "America" ++ _) -> "south_america";
to_continent("BO", "America" ++ _) -> "south_america";
to_continent("BR", "America" ++ _) -> "south_america";
to_continent("CL", "America" ++ _) -> "south_america";
to_continent("CO", "America" ++ _) -> "south_america";
to_continent("EC", "America" ++ _) -> "south_america";
to_continent("GY", "America" ++ _) -> "south_america";
to_continent("GF", "America" ++ _) -> "south_america";
to_continent("PE", "America" ++ _) -> "south_america";
to_continent("PY", "America" ++ _) -> "south_america";
to_continent("SR", "America" ++ _) -> "south_america";
to_continent("UY", "America" ++ _) -> "south_america";
to_continent("VE", "America" ++ _) -> "south_america";

to_continent(_, "Indian/Maldives") -> "asia";

to_continent("AU", "Australia/" ++ _) -> "oceania";
to_continent(_, "Pacific/Pago_Pago") -> "oceania";
to_continent(_, "Pacific/Easter") -> "oceania";
to_continent(_, "Pacific/Fiji") -> "oceania";
to_continent(_, "Pacific/Chuuk") -> "oceania";
to_continent(_, "Pacific/Kosrae") -> "oceania";
to_continent(_, "Pacific/Pohnpei") -> "oceania";
to_continent(_, "Pacific/Guam") -> "oceania";
to_continent(_, "Pacific/Rarotonga") -> "oceania";
to_continent(_, "Pacific/Kiritimati") -> "oceania";
to_continent(_, "Pacific/Tarawa") -> "oceania";
to_continent(_, "Pacific/Kwajalein") -> "oceania";
to_continent(_, "Pacific/Majuro") -> "oceania";
to_continent(_, "Pacific/Noumea") -> "oceania";
to_continent(_, "Pacific/Saipan") -> "oceania";
to_continent(_, "Pacific/Norfolk") -> "oceania";
to_continent(_, "Pacific/Nauru") -> "oceania";
to_continent(_, "Pacific/Niue") -> "oceania";
to_continent(_, "Pacific/Auckland") -> "oceania";
to_continent(_, "Pacific/Gambier") -> "oceania";
to_continent(_, "Pacific/Marquesas") -> "oceania";
to_continent(_, "Pacific/Tahiti") -> "oceania";
to_continent(_, "Pacific/Bougainville") -> "oceania";
to_continent(_, "Pacific/Port_Moresby") -> "oceania";
to_continent(_, "Pacific/Palau") -> "oceania";
to_continent(_, "Pacific/Guadalcanal") -> "oceania";
to_continent(_, "Pacific/Fakaofo") -> "oceania";
to_continent(_, "Pacific/Tongatapu") -> "oceania";
to_continent(_, "Pacific/Funafuti") -> "oceania";
to_continent(_, "Pacific/Honolulu") -> "oceania";
to_continent(_, "Pacific/Efate") -> "oceania";
to_continent(_, "Pacific/Wallis") -> "oceania";
to_continent(_, "Pacific/Apia") -> "oceania";
to_continent(_, "Indian/Christmas") -> "oceania";

to_continent(_, TimezoneStr) ->
  [MainTimezone | _] = string:lexemes(TimezoneStr, "/"),
  string:lowercase(MainTimezone).
