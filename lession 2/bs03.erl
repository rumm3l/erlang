-module(bs03).
-export([split/2]).

split(<<"">>, _) ->
    [];
split(BinString, "") ->
    [BinString];
split(BinString, StrPattern) ->
    split(string:len(StrPattern), list_to_binary(StrPattern), BinString, <<>>, []).


split(_, _, <<>>, <<>>, WordsAcc) ->
    lists:reverse(WordsAcc);
split(_, _, <<>>, WordAcc, WordsAcc) ->
    lists:reverse([WordAcc|WordsAcc]);
split(PatternSize, BinPattern, BinString, WordAcc, WordsAcc) ->
    case BinString of
        <<BinPattern:PatternSize/binary, Rest/binary>> ->
            if
                WordAcc == <<>> ->
                    split(PatternSize, BinPattern, Rest, <<>>, WordsAcc);
                true ->
                    split(PatternSize, BinPattern, Rest, <<>>, [WordAcc|WordsAcc])
            end;
        <<Char, Rest/binary>> ->
            split(PatternSize, BinPattern, Rest, <<WordAcc/binary, Char>>, WordsAcc)
    end.

%% WTF???
%split(PatternSize, BinPattern, <<BinPattern:PatternSize/binary, BinString/binary>>, WordAcc, WordsAcc) ->
%    split(PatternSize, BinPattern, BinString, <<>>, [WordAcc|WordsAcc]);
%split(PatternSize, BinPattern, <<Char, BinString/binary>>, WordAcc, WordsAcc) ->
%    split(PatternSize, BinPattern, BinString, <<WordAcc/binary, Char>>, WordsAcc).
