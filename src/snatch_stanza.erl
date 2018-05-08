-module(snatch_stanza).
-compile([warnings_as_errors]).

-include_lib("fast_xml/include/fxml.hrl").
-include("snatch.hrl").

-export([iq/5,
         iq_resp/1,
         iq_resp/3,
         iq_resp/4,
         iq_error/2,
         iq_error/5,
         message/5,
         message_error/5,
         error_tag/1]).

iq(From, To, ID, Type, Payload) ->
    stanza(<<"iq">>, From, To, ID, Type, Payload).

message(From, To, ID, Type, Payload) ->
    stanza(<<"message">>, From, To, ID, Type, Payload).

message_error(From, To, ID, Payload, Error) ->
    {Code, Type} = get_error(Error),
    ErrTag = #xmlel{name = Error,
                    attrs = [{<<"xmlns">>, ?XMPP_STANZAS}]},
    PayloadError = [#xmlel{name = <<"error">>,
                           attrs = [{<<"code">>, Code},
                                    {<<"type">>, Type}],
                           children = [ErrTag]}],
    message(From, To, ID, <<"error">>, Payload ++ PayloadError).

stanza(StanzaType, From, To, ID, Type, Payload) ->
    Attrs = case Type of
                undefined -> [];
                _ -> [{<<"type">>, Type}]
            end ++
            [{<<"id">>, ID},
             {<<"from">>, From},
             {<<"to">>, To}],
    Stanza = #xmlel{name = StanzaType,
                    attrs = Attrs,
                    children = Payload},
    fxml:element_to_binary(Stanza).

iq_resp(#xmlel{name = <<"iq">>, attrs = Attrs, children = Payload}) ->
    From = proplists:get_value(<<"to">>, Attrs, undefined),
    To = proplists:get_value(<<"from">>, Attrs, undefined),
    ID = proplists:get_value(<<"id">>, Attrs, undefined),
    iq(From, To, ID, <<"result">>, Payload).

iq_resp(From, To, ID) ->
    iq(From, To, ID, <<"result">>, []).

iq_resp(From, To, ID, Payload) ->
    iq(From, To, ID, <<"result">>, Payload).

iq_error(#xmlel{name = <<"iq">>, attrs = Attrs, children = Payload}, Error) ->
    From = proplists:get_value(<<"to">>, Attrs, undefined),
    To = proplists:get_value(<<"from">>, Attrs, undefined),
    ID = proplists:get_value(<<"id">>, Attrs, undefined),
    iq(From, To, ID, <<"error">>, Payload ++ [error_tag(Error)]).

iq_error(From, To, ID, Payload, Error) ->
    iq(From, To, ID, <<"error">>, Payload ++ [error_tag(Error)]).

error_tag(Error) ->
    {Code, Type} = get_error(Error),
    ErrTag = #xmlel{name = Error,
                    attrs = [{<<"xmlns">>, ?XMPP_STANZAS}]},
    #xmlel{name = <<"error">>,
           attrs = [{<<"code">>, Code},
                    {<<"type">>, Type}],
           children = [ErrTag]}.

%% took from: https://xmpp.org/extensions/xep-0086.html
get_error(<<"bad-request">>) -> {<<"400">>, <<"modify">>};
get_error(<<"forbidden">>) -> {<<"403">>, <<"auth">>};
get_error(<<"item-not-found">>) -> {<<"404">>, <<"cancel">>};
get_error(<<"not-acceptable">>) -> {<<"406">>, <<"modify">>};
get_error(<<"internal-server-error">>) -> {<<"500">>, <<"wait">>};
get_error(<<"service-unavailable">>) -> {<<"503">>, <<"cancel">>};
get_error(<<"feature-not-implemented">>) -> {<<"501">>, <<"cancel">>}.
