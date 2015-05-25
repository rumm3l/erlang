-module(p06).
-export([is_palindrome/1]).
-import(p05, [reverse/1]).

is_palindrome(L) ->
    L == reverse(L).
