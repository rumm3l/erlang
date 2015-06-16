-module(cache_server).

-export([server_init/1]).
-export([start_link/1]).
-export([stop/0]).
-export([insert/2]).
-export([lookup/1]).
-export([lookup_by_date/2]).
-export([clean_up/0]).

-define(SERVER_NAME, cache_server).
-record(server_state, {ttl, table, tref}).
-include("cache_api.hrl").

start_link([{ttl, TTL}]) ->
  case whereis(?SERVER_NAME) of
    undefined ->
      SrvPid = spawn(?MODULE, server_init, [TTL]),
      register(?SERVER_NAME, SrvPid),
      {ok, SrvPid};
    RegPid ->
      {ok, RegPid}
  end.

stop() ->
  stop(whereis(?SERVER_NAME)).

stop(undefined) ->
  ok;
stop(Pid) ->
  Pid ! stop,
  ok.


insert(Key, Value) ->
  cast(whereis(?SERVER_NAME), {insert, Key, Value}).

clean_up() ->
  cast(whereis(?SERVER_NAME), {clean_up}).

lookup(Key) ->
  call(whereis(?SERVER_NAME), {lookup, Key}).

lookup_by_date(DateFrom, DateTo) ->
  call(whereis(?SERVER_NAME), {lookup_by_date, DateFrom, DateTo}).


cast(undefined, _) ->
  {error, server_not_running};
cast(Pid, Msg) ->
  Pid ! {cast, Msg},
  ok.

call(undefined, _) ->
  {error, server_not_running};
call(Pid, Msg) ->
  Pid ! {call, self(), Msg},
  receive
    {result, Result} ->
      Result
  after
    3000 ->
      {error, timeout_error}
  end.

server_init(TTL) ->
  {ok, TRef} = timer:apply_interval(TTL * 1000,
                                    ?MODULE,
                                    clean_up,
                                    []),
  server_loop(#server_state{ttl = TTL,
                            tref = TRef,
                            table = cache_api:init()}).


server_loop(State) ->
  receive
    stop ->
      timer:cancel(State#server_state.tref),
      ok;
    {cast, Msg} ->
      {ok, NewState} = handle_cast(Msg, State),
      server_loop(NewState);
    {call, From, Msg} ->
      {ok, Result, NewState} = handle_call(Msg, State),
      From ! {result, Result},
      server_loop(NewState);
    Msg ->
      io:format("Got broken message ~p~n", [Msg])
  end,
  ok.


handle_call({lookup, Key}, State = #server_state{table = Table, ttl = TTL}) ->
  Result = case cache_api:lookup(Table, Key, TTL, true) of
             #entry{value = Value} ->
               Value;
             _ ->
               undefined
           end,
  {ok, {ok, Result}, State};
handle_call({lookup_by_date, DateFrom, DateTo}, State = #server_state{table = Table, ttl = TTL}) ->
  TsFrom = cache_api:dt_2_ts(DateFrom),
  TsTo = cache_api:dt_2_ts(DateTo),
  Entries = cache_api:lookup_by_date(Table, TsFrom, TsTo, TTL, true),
  Result = lists:map(fun({_, K, V, _}) -> {K, V} end, Entries),
  {ok, {ok, Result}, State}.


handle_cast({insert, Key, Value}, #server_state{table = Table} = State) ->
  cache_api:insert(Table, Key, Value),
  {ok, State};
handle_cast({clean_up}, #server_state{table = Table, ttl = TTL} = State) ->
  cache_api:lookup_by_date(Table, 0, cache_api:get_ts(), TTL, false),
  {ok, State}.
