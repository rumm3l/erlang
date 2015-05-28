-module(bs02).
-export([words/1]).

words(BinString) ->
    words(BinString, <<>>, []).

words(<<>>, <<>>, WordsAcc) ->
    WordsAcc;
words(<<>>, WordAcc, WordsAcc) ->
    lists:reverse([WordAcc|WordsAcc]);
words(<<" ", Rest/binary>>, <<>>, WordsAcc) ->
    words(Rest, <<>>, WordsAcc);
words(<<" ", Rest/binary>>, WordAcc, WordsAcc) ->
    words(Rest, <<>>, [WordAcc|WordsAcc]);
words(<<Char, Rest/binary>>, WordAcc, WordsAcc) ->
    words(Rest, <<WordAcc/binary,Char>>, WordsAcc).
