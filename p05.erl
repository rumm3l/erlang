-module(p05).
-export([reverse/1]).

reverse(L) ->
    reverse(L, []).

reverse([], A) ->
    A;
reverse([H|T], A) ->
    reverse(T, [H] ++ A).
