-module(cache_server_handler).

-export([handle_call/2]).
-export([handle_cast/2]).

-include("cache_server.hrl").
-include("cache_api.hrl").


handle_call({lookup, Key}, State = #server_state{table = Table, ttl = TTL}) ->
  Result = case cache_api:lookup(Table, Key, TTL) of
             #entry{value = Value} ->
               {Key, Value};
             _ ->
               undefined
           end,
  {ok, {ok, Result}, State};
handle_call({lookup_by_date, DateFrom, DateTo}, State = #server_state{table = Table, ttl = TTL}) ->
  TsFrom = cache_api:dt_2_ts(DateFrom),
  TsTo = cache_api:dt_2_ts(DateTo),
  Entries = cache_api:lookup_by_date(Table, TsFrom, TsTo, TTL),
  Result = lists:map(fun({_, K, V, _}) -> {K, V} end, Entries),
  {ok, {ok, Result}, State}.


handle_cast({insert, Key, Value}, #server_state{table = Table} = State) ->
  cache_api:insert(Table, Key, Value),
  {ok, State}.
