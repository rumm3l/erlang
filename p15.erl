-module(p15).
-export([replicate/2]).
-import(p05, [reverse/1]).
-import(p07, [flatten/1]).
-import(p12, [repeat/2]).

replicate(L, N) ->
    replicate(L, N, []).

replicate([H|T], N, A) ->
    replicate(T, N, [repeat(H, N)|A]);
replicate([], _, A) ->
    reverse(flatten(A)).
