-module(bs01).
-export([first_word/1]).

first_word(B) ->
    first_word(B, <<>>).

first_word(<<" ", _/binary>>, A) ->
    A;
first_word(<<>>, A) ->
    A;
first_word(<<C, R/binary>>, A) ->
    first_word(R, <<A/binary,C>>).
