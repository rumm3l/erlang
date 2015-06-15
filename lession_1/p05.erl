-module(p05).
-export([reverse/1]).

reverse(L) ->
    reverse(L, []).

reverse([H|T], A) ->
    reverse(T, [H|A]);
reverse([], A) ->
    A.
