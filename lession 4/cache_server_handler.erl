-module(cache_server_handler).

-export([handle_call/2]).
-export([handle_cast/2]).

handle_call(Msg, State) ->
  io:format("Got call message ~p~n", [Msg]),
  {ok, ok, State}.

handle_cast(Msg, State) ->
  io:format("Got cast message ~p~n", [Msg]),
  {ok, State}.
