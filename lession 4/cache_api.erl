-module(cache_api).

%% API
-export([init/0]).
-export([insert/3]).
-export([delete/2]).
-export([clean/1]).
-export([lookup/2]).
-export([lookup/4]).
-export([lookup_by_date/3]).
-export([lookup_by_date/5]).
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
  lookup(Table, Key, get_ts(), true). % simply lookup with extra big ttl

lookup(Table, Key, TTL, Prolong) ->
  CTS = get_ts(),
  case ets:lookup(Table, Key) of
    [#entry{timestamp = ETS, key = Key}] when CTS > ETS + TTL ->
      delete(Table, Key), % remove stale
      io:format("found old~n"),
      undefined;
    [#entry{value = Value, key = Key} = Entry] ->
      if Prolong == true -> insert(Table, Key, Value, CTS);
         true -> void
      end,
      io:format("found valid~n"),
      Entry;
    _ ->
      undefined
  end.


lookup_by_date(Table, FromTimestamp, ToTimestamp) ->
  lookup_by_date(Table, FromTimestamp, ToTimestamp, get_ts(), true).

lookup_by_date(Table, FromTimestamp, ToTimestamp, TTL, Prolong) ->
  io:format("LOOKUP ~p ~p ~p ~p~n", [FromTimestamp, ToTimestamp, TTL, Prolong]),
  Key = ets:first(Table),
  lookup_by_date(Table, FromTimestamp, ToTimestamp, Key, [], TTL, Prolong).

lookup_by_date(_Table, _FromTs, _ToTs, '$end_of_table', Entries, _TTL, _Prolong) ->
  Entries;
lookup_by_date(Table, FromTs, ToTs, Key, Entries, TTL, Prolong) ->
  NewEntries = case lookup(Table, Key, TTL, Prolong) of
                 #entry{timestamp = Ts} = Entry when Ts >= FromTs andalso Ts =< ToTs ->
                   [Entry|Entries];
                 _ ->
                   Entries
  end,

  NextKey = ets:next(Table, Key),
  lookup_by_date(Table, FromTs, ToTs, NextKey, NewEntries, TTL, Prolong).
