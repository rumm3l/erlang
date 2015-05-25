-module(p07).
-export([flatten/1, do_crap/1]).

%flatten(L) ->
%    L.

flatten([[_|_] = IL|T]) ->
    [flatten(IL)|T].
%flatten() ->


do_crap(L) ->
    do_crap(L, []).

do_crap([H|T], A) ->
    do_crap(T, [H|A]);
do_crap([], A) ->
    A.
