-module(p12).
-export([decode_modified/1, repeat/2]).
-import(p05, [reverse/1]).
-import(p07, [flatten/1]).

repeat(V, N) ->
    repeat(V, N, []).

repeat(_, 0, A) ->
    reverse(A);
repeat(V, N, A) ->
    repeat(V, N - 1, [V|A]).


decode_modified(L) ->
    decode_modified(L, []).

decode_modified([{N, V}|T], A) ->
    decode_modified(T, [repeat(V, N)|A]);
decode_modified([V|T], A) ->
    decode_modified(T, [V|A]);
decode_modified([], A) ->
    reverse(flatten(A)).
