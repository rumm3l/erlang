-module(api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	{ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
	{ok, Body, _} = cowboy_req:body(Req),

	{ok, Req2} = case {Method, ContentType} of
		{<<"POST">>, <<"applicatoin/json">>} ->			
			handler_api_message(jsx:decode(Body), Req);
		_ -> cowboy_req:reply(500, [
				{<<"content-type">>, <<"application/text">>}
			], <<"API error">>, Req)
	end,
	{ok, Req2, State}.

handler_api_message(Msg, Req) ->
	io:format("~p~n", Msg),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>}
	], <<"Very goood">>, Req).

terminate(_Reason, _Req, _State) ->
	ok.
