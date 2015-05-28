-module(p01).
-export([last/1]).

last([L]) ->
    L;
last([_|T]) ->
    last(T).
