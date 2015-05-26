-module(p07).
-export([flatten/1]).
-import(p05, [reverse/1]).


flatten(L) ->
    flatten(L, []).

flatten([[HH]|T], A) ->
    flatten([HH|T], A);
flatten([[HH|HT]|T], A) ->
    flatten([HH,HT|T], A);
flatten([H|T], A) ->
    flatten(T, [H|A]);
flatten([], A) ->
    reverse(A).
