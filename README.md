**Geocoding does not rely on any external API**. It relies on an internal (huge) database of 156707 coordinates corresponding to cities of more than 500 inhabitants (see `Data sources` for more details).

## Installation

Compilation:

```
rebar compile
```

Tests:
```
rebar eunit
```

Usage:
```
> erl -pa _build/default/lib/geocoding/ebin/ 
Erlang/OTP 23 [erts-11.1.8] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.8  (abort with ^G)
1> application:start(geocoding).
ok
2> geocoding:reverse(48.857929, 2.346707).
{ok,{europe,fr,<<"Paris">>,525.451956}}
3> geocoding:reverse({48.857929, 2.346707}).
{ok,{europe,fr,<<"Paris">>,525.451956}}
4> geocoding:distance({48.857929, 2.346707}).
5832947
```

## Technical information

### Algorithm

Reverse geocoding is done thanks to a k-d tree algorithm. We use Martin F. Krafft implementation. Original source code is here: https://github.com/kbranigan/libkdtree/tree/master/kdtree%2B%2B . It is embeded in an erlang driver.

From latitude/longitude coordinates, geocoding finds the nearest point in our locations database. Associated data are returned, as weel as a distance between provided coordinates and real coordinates. Since reverse geocoding relies only on coordinates, strange behaviour may occured when a big city is near a small one: a point inside the large city and near its border may be associated to the small city because the small city coordinates will be nearest than the big city coordinates.

### Data sources

All locations data come from https://www.geonames.org database (http://download.geonames.org/export/dump/cities500.zip - 2021-03-15) :
- Locations without population have been excluded.
- Fields have been reduced to: geonameId, latitude, longitude, country code (ISO-3166), standard name. A 6th fields have been added between longitude and country code: continent. Each field is separated by a tabulation.

Example:
```
2988507 48.85341        2.3488  europe  FR      Paris
```

Each location is associated to one of the following continents:
- africa (africa and islands nearby like Madagascar, Canary, La Réunion...)
- antarctica (only one country)
- asia (including russian cities after Ural Mountains)
- europe (including russian cities before Ural Mountains)
- oceania (including Australia continent and pacific islands)
- north america
- south america

Missing locations may be added on request.
