-module(p08).
-export([compress/1]).
-import(p05, [reverse/1]).

compress(L) ->
    compress(L, []).

compress([H, H|T], A) ->
    compress([H|T], A);
compress([H|T], A) ->
    compress(T, [H|A]);
compress([], A) ->
    reverse(A).
