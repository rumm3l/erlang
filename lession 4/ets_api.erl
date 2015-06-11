-module(ets_api).
-import(calendar, [universal_time/0, datetime_to_gregorian_seconds/1]).
%-export([init/0, insert/2, lookup/1, lookup_by_date/2]).
-compile(export_all).

-define(TABLE_NAME, ets_cache).

-record(entry, {key, value, timestamp}).

new_entry(Key, Value, Datetime) ->
    #entry{key = Key,
           value = Value,
           timestamp = datetime_to_gregorian_seconds(Datetime)}.

test_data() ->
    init(),
    insert(new_entry(one, "ONE", {{2015, 6, 11}, {0, 0, 0}})),
    insert(new_entry(two, "TWO", {{2015, 6, 12}, {0, 0, 0}})),
    insert(new_entry(three, "THREE", {{2015, 6, 13}, {0, 0, 0}})),
    ok.


now_sec() ->
    datetime_to_gregorian_seconds(universal_time()).

init() ->
    ets:new(?TABLE_NAME, [ordered_set, named_table, {keypos, 2}]),
    ok.

insert(Entry) ->
    ets:insert(?TABLE_NAME, Entry),
    Entry.

lookup(Key) ->
    ets:lookup(?TABLE_NAME, Key).

delete(Key) ->
    ets:delete(?TABLE_NAME, Key).

lookup_by_date(DateFrom, DateTo) ->
    SecFrom = datetime_to_gregorian_seconds(DateFrom),
    SecTo = datetime_to_gregorian_seconds(DateTo),
    Key = ets:first(?TABLE_NAME),
    lookup_by_date(SecFrom, SecTo, Key, []).


lookup_by_date(_SecFrom, _SecTo, '$end_of_table', Entries) ->
    Entries;
lookup_by_date(SecFrom, SecTo, Key, Entries) ->
    [#entry{timestamp = Timestamp, key = Key}] = [Entry] = lookup(Key),
    NewEntries = if Timestamp >= SecFrom andalso Timestamp =< SecTo ->
                    [Entry|Entries];
                 true ->
                         Entries
                 end,
    NextKey = ets:next(?TABLE_NAME, Key),
    lookup_by_date(SecFrom, SecTo, NextKey, NewEntries).
