-module(cache_api).

%% API
-export([init/0]).
-export([insert/3]).
-export([delete/2]).
-export([clean/1]).
-export([lookup/2]).
-export([lookup/3]).
-export([lookup_by_date/3]).
-export([lookup_by_date/4]).
-export([get_ts/0]).
-export([dt_2_ts/1]).

-include("cache_api.hrl").
-import(calendar, [universal_time/0, datetime_to_gregorian_seconds/1]).
-compile(export_all).

get_ts() ->
  datetime_to_gregorian_seconds(universal_time()).

dt_2_ts(Datetime) ->
  datetime_to_gregorian_seconds(Datetime).

init() ->
  ets:new(?MODULE, [ordered_set, {keypos, 2}]).

insert(Table, Key, Value) ->
  insert(Table, Key, Value, get_ts()).

insert(Table, Key, Value, Timestamp) ->
  Entry = #entry{key = Key,
                 value = Value,
                 timestamp = Timestamp},
  ets:insert(Table, Entry),
  Entry.

delete(Table, Key) ->
  ets:delete(Table, Key).

clean(Table) ->
  ets:delete_all_objects(Table).

lookup(Table, Key) ->
  lookup(Table, Key, get_ts()). % simply lookup with extra big ttl

lookup(Table, Key, TTL) ->
  CTS = get_ts(),
  case ets:lookup(Table, Key) of
    [#entry{timestamp = ETS, key = Key}] when CTS > ETS + TTL ->
      io:format("Remove stale~n"),
      delete(Table, Key), % remove stale
      undefined;
    [#entry{value = Value, key = Key} = Entry] ->
      insert(Table, Key, Value, CTS), % prolong TTL
      Entry;
    _ ->
      undefined
  end.


lookup_by_date(Table, FromTimestamp, ToTimestamp) ->
  lookup_by_date(Table, FromTimestamp, ToTimestamp, get_ts()).

lookup_by_date(Table, FromTimestamp, ToTimestamp, TTL) ->
  Key = ets:first(Table),
  lookup_by_date(Table, FromTimestamp, ToTimestamp, Key, [], TTL).

lookup_by_date(_Table, _FromTs, _ToTs, '$end_of_table', Entries, _TTL) ->
  Entries;
lookup_by_date(Table, FromTs, ToTs, Key, Entries, TTL) ->
  NewEntries = case lookup(Table, Key, TTL) of
                 #entry{timestamp = Ts} = Entry when Ts >= FromTs andalso Ts =< ToTs ->
                   [Entry|Entries];
                 _ ->
                   Entries
  end,

  NextKey = ets:next(Table, Key),
  lookup_by_date(Table, FromTs, ToTs, NextKey, NewEntries, TTL).

test() ->
  Table = init(),
  TS1 = datetime_to_gregorian_seconds({{2015, 6, 1}, {0, 0, 0}}),
  TS2 = datetime_to_gregorian_seconds({{2015, 6, 2}, {0, 0, 0}}),
  TS3 = datetime_to_gregorian_seconds({{2015, 6, 3}, {0, 0, 0}}),
  insert(Table, one, "ONE", TS1),
  insert(Table, two, "TWO", TS2),
  insert(Table, three, "THREE", TS3),
  io:format("Data filled~n"),
  io:format("lookup1 ~p~n", [test_lookup1(Table)]),
  io:format("lookup2 ~p~n", [test_lookup2(Table)]),
  io:format("lookup3 ~p~n", [test_lookup3(Table)]),
  io:format("lookup4 ~p~n", [test_lookup4(Table)]),
  ok.

test_lookup1(Table) ->
  TS1 = datetime_to_gregorian_seconds({{2015, 6, 1}, {0, 0, 0}}),
  TS2 = datetime_to_gregorian_seconds({{2015, 6, 2}, {0, 0, 0}}),
  lookup_by_date(Table, TS1, TS2).

test_lookup2(Table) ->
  TS1 = datetime_to_gregorian_seconds({{2015, 6, 2}, {0, 0, 0}}),
  TS2 = datetime_to_gregorian_seconds({{2015, 6, 4}, {0, 0, 0}}),
  lookup_by_date(Table, TS1, TS2).

test_lookup3(Table) ->
  lookup_by_date(Table, get_ts() - 3600, get_ts()).

test_lookup4(Table) ->
  lookup_by_date(Table, 0, get_ts(), -1).
