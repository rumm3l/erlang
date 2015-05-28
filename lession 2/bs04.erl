-module(bs04).
-export([decode_xml/1, get_tag_name/1, push_element/2, pop_element/1]).

decode_xml(BinStr) ->
    decode_xml(BinStr, [], <<>>).


decode_xml(<<>>, [RootElement], TextBuf) ->
    RootElement;
decode_xml(<<"</", DocStream/binary>>, Stack, TextBuf) ->
    [CurrElement|[PrevElement|NewStack]] = Stack,
    {_, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [push_element(PrevElement, CurrElement)|NewStack], <<>>);
decode_xml(<<"<", DocStream/binary>>, Stack, TextBuf) ->
    {TagName, NewDocStream} = get_tag_name(DocStream),
    decode_xml(NewDocStream, [{TagName, [], []}|Stack], <<>>);
decode_xml(<<Char, DocStream/binary>>, Stack, TextBuf) ->
    decode_xml(DocStream, Stack, <<TextBuf/binary, Char>>).




push_element(Root, Element) ->
    {ElementName, _, Elements} = Root,
    {ElementName, [], [Element|Elements]}.

pop_element([Element|Stack]) ->
    {Element, Stack}.

get_tag_name(DocStream) ->
    get_tag_name(DocStream, <<>>).

get_tag_name(<<">", DocStream/binary>>, TagNameAcc) ->
    {TagNameAcc, DocStream};
get_tag_name(<<Char, DocStream/binary>>, TagNameAcc) ->
    get_tag_name(DocStream, <<TagNameAcc/binary, Char>>).
