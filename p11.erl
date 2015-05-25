-module(p11).
-export([encode_modified/1]).
-import(p04, [len/1]).
-import(p05, [reverse/1]).
-import(p09, [pack/1]).


encode_modified(L) ->
    count_list(pack(L), []).

count_list([[V|[]]|T], A) ->
    count_list(T, [V|A]);
count_list([[V|_] = H|T], A) ->
    count_list(T, [{len(H), V}|A]);
count_list([], A) ->
    reverse(A).
