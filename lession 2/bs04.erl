-module(bs04).
-export([decode_xml/1]).

decode_xml(BinStr) ->
    decode_xml(BinStr, [], <<>>).



decode_xml(<<"</", _/binary>>, [RootElement], _) ->
    reverse_element_values(RootElement);

decode_xml(<<"</", DocStream/binary>>, [CurrElement|Stack], TextBuf) ->
    {_, NewDocStream} = get_tag_name(DocStream),
    NewStack = push_to_head_element(Stack, reverse_element_values(push_to_element(CurrElement, TextBuf))),
    decode_xml(NewDocStream, NewStack, <<>>);

decode_xml(<<"<", DocStream/binary>>, Stack, TextBuf) ->
    {TagName, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [{TagName, [], []}|push_to_head_element(Stack, TextBuf)], <<>>);

decode_xml(<<Char, DocStream/binary>>, Stack, TextBuf) ->
    decode_xml(DocStream, Stack, <<TextBuf/binary, Char>>).


push_to_head_element([Head|Stack], Value)->
    [push_to_element(Head, Value)|Stack];
push_to_head_element(Stack, _) ->
    Stack.


push_to_element(Element, <<>>) ->
    Element;
push_to_element({ElementName, _, Values}, Value) ->
    {ElementName, [], [Value|Values]}.

reverse_element_values({ElementName, _, Values}) ->
    {ElementName, [], lists:reverse(Values)}.


get_tag_name(DocStream) ->
    get_tag_name(DocStream, <<>>).

get_tag_name(<<">", DocStream/binary>>, TagNameAcc) ->
    {TagNameAcc, DocStream};
get_tag_name(<<Char, DocStream/binary>>, TagNameAcc) ->
    get_tag_name(DocStream, <<TagNameAcc/binary, Char>>).
