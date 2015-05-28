-module(bs04).
-export([decode_xml/1]).

decode_xml(BinStr) ->
    decode_xml(BinStr, [], <<>>).


decode_xml(<<"</", DocStream/binary>>, [CurrElement, PrevElement|Stack], TextBuf) ->
    {_, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [push_to_element(PrevElement, push_to_element(CurrElement,TextBuf))|Stack], <<>>);
decode_xml(<<"</", _/binary>>, [RootElement], _) ->
    RootElement;
decode_xml(<<"<", DocStream/binary>>, [CurrElement|Stack], TextBuf) ->
    {TagName, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [{TagName, [], []}, push_to_element(CurrElement, TextBuf)|Stack], <<>>);
decode_xml(<<"<", DocStream/binary>>, Stack, _) ->
    {TagName, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [{TagName, [], []}|Stack], <<>>);
decode_xml(<<Char, DocStream/binary>>, Stack, TextBuf) ->
    decode_xml(DocStream, Stack, <<TextBuf/binary, Char>>).


push_to_head(Stack, <<>>) ->
    Stack;
push_to_head([Head|Stack], Value)->
    [push_to_element(Head, Value)|Stack];
push_to_head(Stack, _) ->
    Stack.


push_to_element(Element, Value) ->
    {ElementName, _, Values} = Element,
    {ElementName, [], [Value|Values]}.


get_tag_name(DocStream) ->
    get_tag_name(DocStream, <<>>).

get_tag_name(<<">", DocStream/binary>>, TagNameAcc) ->
    {TagNameAcc, DocStream};
get_tag_name(<<Char, DocStream/binary>>, TagNameAcc) ->
    get_tag_name(DocStream, <<TagNameAcc/binary, Char>>).
