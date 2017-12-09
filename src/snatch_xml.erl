-module(snatch_xml).
-compile([warnings_as_errors]).

-include_lib("fast_xml/include/fxml.hrl").

-export([clean_spaces/1,
         get_cdata/1,
         get_attr/2,
         get_attr/3,
         get_attr_atom/2,
         get_attr_atom/3,
         get_attr_int/3]).

clean_spaces(#xmlel{children = []} = XmlEl) ->
    XmlEl;
clean_spaces(#xmlel{children = Children} = XmlEl) ->
    C = lists:filtermap(fun
        ({xmlcdata, Content}) -> trim(Content) =/= <<>>;
        (#xmlel{} = X) -> {true, clean_spaces(X)}
    end, Children),
    XmlEl#xmlel{children = C}.

trim(Text) ->
    re:replace(Text, "^\\s+|\\s+$", "", [{return, binary}, global]).

get_cdata(#xmlel{children = Children}) ->
    get_cdata(Children, <<>>).

get_cdata([{xmlcdata, C}|Children], CData) ->
    get_cdata(Children, <<CData/binary, C/binary>>);
get_cdata([#xmlel{children = []}|Children], CData) ->
    get_cdata(Children, CData);
get_cdata([#xmlel{children = C}|Children], CData) ->
    get_cdata(Children, get_cdata(C, CData));
get_cdata([], CData) ->
    CData.

get_attr(Name, #xmlel{} = XmlEl) when is_binary(Name) ->
    get_attr(Name, XmlEl, undefined).

get_attr(Name, #xmlel{attrs = Attrs}, Default) when is_binary(Name) ->
    case lists:keyfind(Name, 1, Attrs) of
        false -> Default;
        {Name, Value} -> Value
    end.

get_attr_atom(Name, #xmlel{} = XmlEl) ->
    get_attr_atom(Name, XmlEl, undefined).

get_attr_atom(Name, #xmlel{attrs = Attrs}, Default) when is_atom(Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        false -> Default;
        {Name, Value} -> binary_to_atom(Value, utf8)
    end.

get_attr_int(Name, #xmlel{attrs = Attrs}, Default) when is_integer(Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        false -> Default;
        {Name, Value} -> binary_to_integer(Value)
    end.
