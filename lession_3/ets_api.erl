-module(ets_api).
-export([init/0, put/3, get/2, get/1, delete/1, fill/0]).

init() ->
    ets:new(persons, [ordered_set, named_table, {keypos, 3}]).

put(FirstName, LastName, DateTime) ->
    ets:insert(persons,
               {FirstName,
                LastName,
                dt2s(DateTime)}),
    {FirstName, LastName, DateTime}.

get(DateTimeFrom, _DateTimeTo) ->
    ets:lookup(persons, DateTimeFrom).

get(DateTimeWhen) ->
    ets:lookup(persons,
               dt2s(DateTimeWhen)).

delete(DateTimeWhen) ->
    ets:delete(persons,
               dt2s(DateTimeWhen)).

fill() ->
    init(),
    put("Vasya", "Pupkin", {{2015,6,1},{19,10,52}}),
    put("Vasya", "Pupkin", {{2013,6,1},{9,10,52}}),
    put("Unter", "Mensch", {{2015,5,26},{7,10,52}}),
    ok.

dt2s(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).
