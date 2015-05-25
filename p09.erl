-module(p09).
-export([pack/1]).
-import(p05, [reverse/1]).

pack([H|T]) ->
    pack(T, [H], []).

pack([H|T], [H|_] = HA, A) ->
    pack(T, [H|HA], A);
pack([H|T], HA, A) ->
    pack(T, [H], [HA|A]);
pack([], HA, A) ->
    reverse([HA|A]).
