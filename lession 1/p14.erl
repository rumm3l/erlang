-module(p14).
-export([duplicate/1]).
-import(p05, [reverse/1]).

duplicate(L) ->
    duplicate(L, []).

duplicate([H|T], A) ->
    duplicate(T, [H, H|A]);
duplicate([], A) ->
    reverse(A).
