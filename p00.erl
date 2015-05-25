-module(p01).
-export([last/1, but_last/1, element_at/2, len/1, len2/1]).

last([A])->
	A;
last([_|T])->
	last(T).

but_last([_, _] = C) ->
	C;
but_last([_|T]) ->
	but_last(T).
 
element_at([H|_], 1) ->
	H;
element_at([_|T], N) ->
 	N1 = N - 1,
	element_at(T, N1);
element_at([], _) ->
	undefined.

len([]) ->
	0;
len([_|T]) ->
	len(T, 1).

len([], A) ->
	A;
len([_|T], A) ->
	len(T, A + 1).

len2(L) ->
	len2(L, 0).

len2([], A) ->
	A;
len2([_|T], A) ->
	len2(T, A + 1).
	
