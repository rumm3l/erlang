-module(p13).
-export([decode/1]).
-import(p12, [decode_modified/1]).


decode(L) ->
    decode_modified(L).
