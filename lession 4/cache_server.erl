-module(cache_server).

-export([server_init/1]).
-export([start_link/1]).
-export([stop/0]).
-export([insert/2]).
-export([lookup/1]).
-export([lookup_by_date/2]).

-define(SERVER_NAME, cache_server).

-include("cache_server.hrl").

start_link([{ttl, TTL}]) ->
  case whereis(?SERVER_NAME) of
    undefined ->
      io:format("Pid will be registered~n"),
      SrvPid = spawn(?MODULE, server_init, [TTL]),
      register(?SERVER_NAME, SrvPid),
      {ok, SrvPid};
    RegPid ->
      io:format("Pid already registered~n"),
      {ok, RegPid}
  end.

stop() ->
  stop(whereis(?SERVER_NAME)).

stop(undefined) ->
  io:format("Already stopped~n"),
  ok;
stop(Pid) ->
  Pid ! stop,
  ok.


insert(Key, Value) ->
  cast(whereis(?SERVER_NAME), {insert, Key, Value}).

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
  io:format("Initializing server(~p) with TTL ~p~n", [self(), TTL]),
  server_loop(#server_state{ttl = TTL,
                            table = cache_api:init()}).


server_loop(State) ->
  receive
    stop ->
      io:format("Server stopped~n"),
      ok;
    {cast, Msg} ->
      {ok, NewState} = cache_server_handler:handle_cast(Msg, State),
      server_loop(NewState);
    {call, From, Msg} ->
      {ok, Result, NewState} = cache_server_handler:handle_call(Msg, State),
      From ! {result, Result},
      server_loop(NewState);
    Msg ->
      io:format("Got broken message ~p~n", [Msg])
  end,
  ok.
