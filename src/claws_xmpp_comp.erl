-module(claws_xmpp_comp).
-behaviour(gen_statem).
-behaviour(claws).

-include_lib("fast_xml/include/fxml.hrl").
-include("snatch.hrl").

-record(data, {
    domain :: binary(),
    password :: binary(),
    host :: inet:socket_address(),
    port :: inet:port_number(),
    socket :: gen_tcp:socket(),
    trimmed = false :: boolean(),
    adjust_attrs = false :: boolean(),
    ping = false :: false | pos_integer(),
    stream
}).

-type state_data() :: #data{}.

-export([start_link/1, start_link/2, connect/0, disconnect/0]).
-export([init/1,
         callback_mode/0,
         terminate/3,
         code_change/4,
         handle_event/4]).
-export([disconnected/3,
         retrying/3,
         connected/3,
         stream_init/3,
         authenticate/3,
         ready/3]).
-export([send/2, send/3]).

-define(INIT(D),
        <<"<?xml version='1.0' encoding='UTF-8'?>"
          "<stream:stream to='", D/binary, "' "
                         "xmlns='jabber:component:accept' "
                         "xmlns:stream='http://etherx.jabber.org/streams'>">>).
-define(AUTH(P), <<"<handshake>", P/binary, "</handshake>">>).

-define(SERVER, ?MODULE).

-spec start_link(Name :: atom(), Params :: map()) -> {ok, pid()}.
start_link(Name, Params) ->
    gen_statem:start_link({local, Name}, ?MODULE, Params, []).

-spec start_link(Params :: map()) -> {ok, pid()}.
start_link(Params) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

-type xmpp_conn_state() :: disconnected |
                           retrying |
                           connected |
                           stream_init |
                           authenticate |
                           ready.

-spec init(Params :: map()) -> {ok, xmpp_conn_state(), state_data()}.
init(#{host := Host,
       port := Port,
       domain := Domain,
       password := Password} = Cfg) ->
    Trimmed = maps:get(trimmed, Cfg, false),
    AddFrom = maps:get(adjust_attrs, Cfg, false),
    Ping = maps:get(ping, Cfg, false),
    {ok, disconnected, #data{host = Host,
                             port = Port,
                             domain = Domain,
                             password = Password,
                             trimmed = Trimmed,
                             adjust_attrs = AddFrom,
                             ping = Ping}}.

-spec callback_mode() -> handle_event_function.
%% @private
%% @doc this function is in charge to report to gen_statem the way
%%      the callbacks should work.
%% @end
%% @see gen_statem:callback_mode/0
callback_mode() -> handle_event_function.

%% API

-spec connect() -> ok.
connect() -> 
    ok = gen_statem:cast(?SERVER, connect).

-spec disconnect() -> ok.
disconnect() ->
    ok = gen_statem:stop(?SERVER).

%% States

disconnected(Type, connect, #data{host = Host, port = Port} = Data)
        when Type =:= cast orelse Type =:= state_timeout ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}], 1000) of
        {ok, NewSocket} ->
            {next_state, connected, Data#data{socket = NewSocket},
             [{next_event, cast, init_stream}]};
        Error ->
            error_logger:error_msg("Connecting Error [~p:~p]: ~p~n",
                                   [Host, Port, Error]),
            {next_state, retrying, Data, [{next_event, cast, connect}]}
    end.


retrying(cast, connect, Data) ->
    {next_state, disconnected, Data, [{state_timeout, 3000, connect}]}.


connected(cast, init_stream, #data{} = Data) ->
    Opts = [no_gen_server],
    Stream = fxml_stream:new(whereis(?SERVER), infinity, Opts),
    {next_state, stream_init, Data#data{stream = Stream},
     [{next_event, cast, init}]}.


stream_init(cast, init, #data{domain = Domain, socket = Socket} = Data) ->
    gen_tcp:send(Socket, ?INIT(Domain)),
    {keep_state, Data, []};

stream_init(cast, {received, {xmlstreamstart, _, Attribs}}, Data) ->
    case lists:keyfind(<<"id">>, 1, Attribs) of
        {<<"id">>, StreamID} ->
            {next_state, authenticate, Data,
             [{next_event, cast, {handshake, StreamID}}]};
        false ->
            error_logger:error_msg("stream invalid, no Stream ID", []),
            gen_tcp:close(Data#data.socket),
            {next_state, retrying, Data, [{next_event, cast, connect}]}
    end.


authenticate(cast, {handshake, StreamID},
             #data{socket = Socket, password = Secret} = Data) ->
    Concat = <<StreamID/binary, Secret/binary>>,
    <<Mac:160/integer>> = crypto:hash(sha, Concat),
    Password = iolist_to_binary(io_lib:format("~40.16.0b", [Mac])),
    gen_tcp:send(Socket, ?AUTH(Password)),
    {keep_state, Data, []};

authenticate(cast, {received, #xmlel{name = <<"handshake">>,
                                     children = []}}, Data) ->
    {next_state, ready, Data, timeout_action(Data)}.

remove_attr(Name, #xmlel{attrs = Attrs} = XmlEl) ->
    XmlEl#xmlel{attrs = proplists:delete(Name, Attrs)}.

add_attr(Name, Value, #xmlel{attrs = Attrs} = XmlEl) ->
    XmlEl#xmlel{attrs = [{Name, Value}|Attrs]}.

change_attr(Name, Value, XmlEl) ->
    add_attr(Name, Value, remove_attr(Name, XmlEl)).

change_attrs(Fields, XmlEl) ->
    lists:foldl(fun
        ({_Field, <<"unknown">>}, TempXmlEl) ->
            TempXmlEl;
        ({_Field, Value}, TempXmlEl) when is_atom(Value) ->
            TempXmlEl;
        ({Field, Value}, TempXmlEl) ->
            change_attr(Field, Value, TempXmlEl)
    end, XmlEl, Fields).

ready(cast, {send, Packet, JID, ID}, #data{socket = Socket,
                                           adjust_attrs = true,
                                           domain = Domain} = Data) ->
    XmlEl = fxml_stream:parse_element(Packet),
    Fields = [{<<"to">>, JID},
              {<<"from">>, Domain},
              {<<"id">>, ID}],
    NewXmlEl = change_attrs(Fields, XmlEl),
    NewPacket = fxml:element_to_binary(NewXmlEl),
    gen_tcp:send(Socket, NewPacket),
    {keep_state_and_data, timeout_action(Data)};

ready(cast, {send, Packet, _JID, _ID}, #data{socket = Socket} = Data) ->
    gen_tcp:send(Socket, Packet),
    {keep_state_and_data, timeout_action(Data)};

ready(cast, {received, Packet}, #data{trimmed = true} = Data) ->
    From = snatch_xml:get_attr(<<"from">>, Packet),
    To = snatch_xml:get_attr(<<"to">>, Packet),
    Via = #via{jid = From, exchange = To, claws = ?MODULE},
    TrimmedPacket = snatch_xml:clean_spaces(Packet),
    snatch:received(TrimmedPacket, Via),
    {keep_state_and_data, timeout_action(Data)};

ready(cast, {received, Packet}, #data{trimmed = false} = Data) ->
    From = snatch_xml:get_attr(<<"from">>, Packet),
    To = snatch_xml:get_attr(<<"to">>, Packet),
    Via = #via{jid = From, exchange = To, claws = ?MODULE},
    snatch:received(Packet, Via),
    {keep_state_and_data, timeout_action(Data)}.


handle_event(timeout, ping, _State, #data{socket = Socket} = Data) ->
    gen_tcp:send(Socket, <<"\n">>),
    {keep_state_and_data, timeout_action(Data)};
handle_event(info, {tcp, _Socket, Packet}, _State,
             #data{stream = Stream} = Data) ->
    NewStream = fxml_stream:parse(Stream, Packet),
    {keep_state, Data#data{stream = NewStream}, []};
handle_event(info, {tcp_closed, _Socket}, _State, #data{stream = Stream} = Data) ->
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {tcp_error, _Socket, Reason}, _State, #data{stream = Stream} = Data) ->
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    error_logger:error_msg("tcp closed error: ~p~n", [Reason]),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {xmlstreamstart, _Name, _Attribs} = Packet, _State, Data) ->
    {keep_state, Data, [{next_event, cast, {received, Packet}}]};
handle_event(info, {xmlstreamend, _Name}, _State,
             #data{stream = Stream} = Data) ->
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {xmlstreamerror, _Error}, _State,
             #data{stream = Stream} = Data) ->
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {xmlstreamelement, Packet}, _State, Data) ->
    {keep_state, Data,[{next_event, cast, {received, Packet}}]};
handle_event(Type, Content, State, Data) ->
    ?MODULE:State(Type, Content, Data).

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

send(Data, JID) ->
    send(Data, JID, undefined).

send(Data, JID, ID) ->
    gen_statem:cast(?MODULE, {send, Data, JID, ID}).

close_stream(Stream) ->
    catch fxml_stream:close(Stream).

timeout_action(#data{ping = false}) ->
    [];
timeout_action(#data{ping = Ping}) ->
    [{timeout, Ping, ping}].
