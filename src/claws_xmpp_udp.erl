-module(claws_xmpp_udp).
-behaviour(gen_statem).
-behaviour(claws).

-include_lib("fast_xml/include/fxml.hrl").
-include("snatch.hrl").

-record(data, {
    user :: binary(),
    domain :: binary(),
    password :: binary(),
    resource :: binary(),
    host :: inet:socket_address(),
    port :: inet:port_number(),
    channel_host :: inet:socket_address(),
    channel_port :: inet:port_number(),
    socket :: gen_udp:socket()
}).

-export([start_link/1, connect/0, disconnect/0]).
-export([init/1,
         callback_mode/0,
         handle_event/4,
         code_change/4,
         terminate/3]).
-export([disconnected/3,
         retrying/3,
         connected/3,
         stream_init/3,
         authenticate/3,
         bind/3,
         binding/3,
         binded/3]).
-export([send/2, send/3]).

-define(INIT(D), <<"<?xml version='1.0' encoding='UTF-8'?>"
                   "<stream:stream to='", D/binary, "' xmlns='jabber:client' "
                                  "xmlns:stream='http://etherx.jabber.org/streams' "
                                  "version='1.0'>">>).
-define(AUTH(U, P, R), <<"<iq type='set' id='auth2'>"
                         "<query xmlns='jabber:iq:auth'>"
                         "<username>", U/binary, "</username>"
                         "<password>", P/binary, "</password>"
                         "<resource>", R/binary, "</resource>"
                         "</query></iq>">>).
-define(AUTH_SASL(B64), << "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' "
                                 "mechanism='PLAIN'>", B64/binary, "</auth>">>).
-define(BIND(R), <<"<iq type='set' id='bind3' xmlns='jabber:client'>"
                   "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>"
                   "<resource>", R/binary, "</resource></bind></iq>">>).
-define(SESSION, <<"<iq type='set' id='session4'>"
                   "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>">>).
-define(PRESENCE, <<"<presence/>">>).

-define(SERVER, ?MODULE).

start_link(Params) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(#{host := Host,
       port := Port,
       user := User,
       domain := Domain,
       password := Password,
       resource := Resource} = Cfg) ->
    Events = case maps:get(auto_connect, Cfg, false) of
        true -> [{next_event, cast, connect}];
        false -> []
    end,
    {ok, disconnected, #data{host = Host,
                             port = Port,
                             user = User,
                             domain = Domain,
                             password = Password,
                             resource = Resource}, Events}.

callback_mode() -> handle_event_function.

%% API

connect() ->
    gen_statem:cast(?SERVER, connect).

disconnect() ->
    gen_statem:cast(?SERVER, disconnect).

%% States

disconnected(Type, connect, #data{host = Host, port = Port} = Data)
        when Type =:= cast orelse Type =:= state_timeout ->
    case gen_udp:open(0, [binary, {active, true}]) of
        {ok, NewSocket} ->
            {next_state, connected, Data#data{socket = NewSocket},
             [{next_event, cast, init_stream}]};
        Error ->
            error_logger:error_msg("Connecting Error [~p:~p]: ~p~n",
                                   [Host, Port, Error]),
            {next_state, retrying, Data, [{next_event, cast, connect}]}
    end;

disconnected(cast, disconnect, _Data) ->
    {keep_state_and_data, []}.

retrying(cast, connect, Data) ->
    {next_state, disconnected, Data, [{state_timeout, 3000, connect}]}.

connected(cast, init_stream, #data{} = Data) ->
    {next_state, stream_init, Data, [{next_event, cast, init}]}.

stream_init(cast, init, #data{domain = Domain, socket = Socket, host = Host, port = Port} = Data) ->
    gen_udp:send(Socket, Host, Port, ?INIT(Domain)),
    {keep_state, Data, []};

stream_init(cast, {received, _Packet}, Data) -> 
    {next_state, authenticate, Data, [{next_event, cast, auth_sasl}]}.

authenticate(cast, auth, #data{user = User, password = Password,
                               resource = Resource, socket = Socket} = Data) ->
                                gen_udp:send(Socket, ?AUTH(User, Password, Resource)),
    {keep_state, Data, []};

authenticate(cast, auth_sasl, #data{user = User, password = Password,
                                    host = Host, port = Port, socket = Socket} = Data) ->
    B64 = base64:encode(<<0, User/binary, 0, Password/binary>>),
    gen_udp:send(Socket, Host, Port, ?AUTH_SASL(B64)),
    {keep_state, Data, []};

authenticate(cast, {received, _Packet}, Data) ->
    {next_state, bind, Data, [{next_event, cast, bind}]}.

bind(cast, bind, #data{resource = Resource, socket = Socket, domain = Domain} = Data) ->
    gen_udp:send(Socket, ?INIT(Domain)),
    gen_udp:send(Socket, ?BIND(Resource)),
    {keep_state, Data, []};

bind(cast, {received, _Packet}, #data{socket = Socket, host = Host, port = Port} = Data) ->
    gen_udp:send(Socket, Host, Port, ?SESSION), 
    gen_udp:send(Socket, Host, Port, ?PRESENCE),
    {next_state, binding, Data, []}.

binding(cast, {received, Packet}, Data) ->
    snatch:connected(?MODULE),
    Elem = fxml_stream:parse_element(Packet),
    ChannelHost = snatch_xml:get_attr(<<"ip">>, Elem),
    ChannelPort = snatch_xml:get_attr(<<"port">>, Elem),
    {next_state, binded, Data#data{channel_host = ChannelHost, channel_port = ChannelPort} , []}.

binded(cast, {send, Packet}, #data{socket = Socket, channel_host = ChannelHost, channel_port = ChannelPort}) ->
    gen_udp:send(Socket, ChannelHost, ChannelPort, Packet),
    {keep_state_and_data, []};

binded(cast, {received, #xmlel{} = Packet}, _Data) ->
    From = snatch_xml:get_attr(<<"from">>, Packet),
    To = snatch_xml:get_attr(<<"to">>, Packet),
    Via = #via{jid = From, exchange = To, claws = ?MODULE},
    snatch:received(Packet, Via),
    {keep_state_and_data, []};

binded(cast, {received, Packet}, _Data) ->
    snatch:received(Packet),
    {keep_state_and_data, []};

binded(cast, _Unknown, _Data) ->
    {keep_state_and_data, []}.

handle_event(info, {udp, _Socket, _SrcIp, _SrcPort, Packet}, _State,
             #data{} = Data) ->
    Elem = fxml_stream:parse_element(Packet),
    {keep_state, Data,[{next_event, cast, {received, Elem}}]};
handle_event(info, {udp_error, _Socket, Reason}, _State, #data{} = Data) ->
    snatch:disconnected(?MODULE),
    error_logger:error_msg("udp closed error: ~p~n", [Reason]),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(Type, Content, State, Data) ->
    case erlang:function_exported(?MODULE, State, 3) of
        true ->
            ?MODULE:State(Type, Content, Data);
        _ -> 
            error_logger:error_msg("Unknown Function: ~p~n", [State])
    end.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

send(Data, _JID) ->
    gen_statem:cast(?MODULE, {send, Data}).

send(Data, _JID, _ID) ->
    gen_statem:cast(?MODULE, {send, Data}).
