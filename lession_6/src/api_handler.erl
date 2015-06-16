-module(api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).


init(_Type, Req, []) ->
  cache_server:start_link([{ttl, 5}]),
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  {ContentType, _} = cowboy_req:header(<<"content-type">>, Req),
  {ok, Body, _} = cowboy_req:body(Req),
  {ok, Req2} = case {Method, ContentType, jsx:is_json(Body)} of
                 {<<"POST">>, <<"application/json">>, true} ->
                   handle_api_message(jsx:decode(Body, [{labels, atom}]), Req);
                 _ ->
                   reply_error(Req)
	end,
	{ok, Req2, State}.

reply_error(Req) ->
  cowboy_req:reply(500, [
	     		{<<"content-type">>, <<"application/text">>}
                        ], <<"API error">>, Req).

reply_ok(JsonResp, Req) ->
  cowboy_req:reply(200, [
                         {<<"content-type">>, <<"application/json">>}
                        ], jsx:encode(JsonResp), Req).

handle_api_message(JsonMsg, Req) ->
  JsonResp = handle_action(proplists:get_value(action, JsonMsg), JsonMsg),
  reply_ok(JsonResp, Req).

handle_action(<<"insert">>, JsonMsg) ->
  Key = proplists:get_value(key, JsonMsg),
  Value = proplists:get_value(value, JsonMsg),
  cache_server:insert(Key, Value),
  ok;
handle_action(<<"lookup">>, JsonMsg) ->
  Key = proplists:get_value(key, JsonMsg),
  {ok, Result} = cache_server:lookup(Key),
  Result;
handle_action(<<"lookup_by_date">>, JsonMsg) ->
  DateFrom = dh_date:parse(proplists:get_value(date_from, JsonMsg)),
  DateTo = dh_date:parse(proplists:get_value(date_to, JsonMsg)),
  {ok, Result} = cache_server:lookup_by_date(DateFrom, DateTo),
  Result;
handle_action(_Action, _JsonMsg) ->
  unknown_action.


terminate(_Reason, _Req, _State) ->
	ok.
