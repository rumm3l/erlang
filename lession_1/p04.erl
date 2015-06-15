-module(p04).
-export([len/1]).

len(L) ->
    len(L, 0).

len([], A) ->
    A;
len([_|T], A) ->
    len(T, A + 1).
