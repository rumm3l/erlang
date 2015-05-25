-module(p07).
-export([flatten/1, flatten2/1]).
-import(p05, [reverse/1]).

flatten(L) ->
    flatten(L, []).

flatten([[_|_] = H|T], A) ->
    flatten(H ++ T, A);
flatten([H|T], A) ->
    flatten(T, [H|A]);
flatten([], A) ->
    reverse(A).


flatten2(L) ->
    flatten2(L, []).

flatten2([[HH]|T], A) ->
    flatten2([HH|T], A);
flatten2([[HH|HT]|T], A) ->
    flatten2([HH,HT|T], A);
flatten2([H|T], A) ->
    flatten2(T, [H|A]);
flatten2([], A) ->
    reverse(A).
