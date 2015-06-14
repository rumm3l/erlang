-module(cache_server_handler).

-export([handle_call/2]).
-export([handle_cast/2]).

-include("cache_server.hrl").
-include("cache_api.hrl").


handle_call({lookup, Key}, State = #server_state{table = Table, ttl = TTL}) ->
  CurrTimestamp = cache_api:get_ts(),
  case cache_api:lookup(Table, Key) of
    undefined ->
      {ok, {ok, undefined}, State};
    {_, Key, _, Timestamp} when CurrTimestamp > Timestamp + TTL ->
      cache_api:delete(Table, Key),
      {ok, {ok, undefined}, State};
    {_, Key, Value, _} ->
      cache_api:insert(Table, Key, Value), % prolong TTL
      {ok, {ok, Value}, State}
  end;
handle_call(Msg, State) ->
  io:format("Got call message ~p~n", [Msg]),
  {ok, ok, State}.

handle_cast({insert, Key, Value}, #server_state{table = Table} = State) ->
  cache_api:insert(Table, Key, Value),
  {ok, State}.
