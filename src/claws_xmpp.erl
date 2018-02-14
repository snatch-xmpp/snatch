-module(claws_xmpp).
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
    socket :: gen_tcp:socket(),
    stream
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
       resource := Resource}) ->
    {ok, disconnected, #data{host = Host,
                             port = Port,
                             user = User,
                             domain = Domain,
                             password = Password,
                             resource = Resource}}.

callback_mode() -> handle_event_function.

%% API

connect() -> 
    gen_statem:cast(?SERVER, connect).

disconnect() ->
    gen_statem:cast(?SERVER, disconnect).

%% States

disconnected(Type, connect, #data{host = Host, port = Port} = Data)
        when Type =:= cast orelse Type =:= state_timeout ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}]) of
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
    Stream = fxml_stream:new(whereis(?SERVER)),
    {next_state, stream_init, Data#data{stream = Stream},
     [{next_event, cast, init}]}.

stream_init(cast, init, #data{domain = Domain, socket = Socket} = Data) ->
    gen_tcp:send(Socket, ?INIT(Domain)),
    {keep_state, Data, []};

stream_init(cast, {received, _Packet}, Data) -> 
    {next_state, authenticate, Data, [{next_event, cast, auth_sasl}]}.

authenticate(cast, auth, #data{user = User, password = Password,
                               resource = Resource, socket = Socket} = Data) ->
    gen_tcp:send(Socket, ?AUTH(User, Password, Resource)),
    {keep_state, Data, []};

authenticate(cast, auth_sasl, #data{user = User, password = Password,
                                    socket = Socket} = Data) ->
    B64 = base64:encode(<<0, User/binary, 0, Password/binary>>),
    gen_tcp:send(Socket, ?AUTH_SASL(B64)),
    {keep_state, Data, []};

authenticate(cast, {received, _Packet}, Data) ->
    {next_state, bind, Data, [{next_event, cast, bind}]}.

bind(cast, bind, #data{resource = Resource, socket = Socket, domain = Domain,
                       stream = Stream} = Data) ->
    close_stream(Stream),
    NewStream = fxml_stream:new(whereis(?SERVER)),
    gen_tcp:send(Socket, ?INIT(Domain)),
    gen_tcp:send(Socket, ?BIND(Resource)),
    {keep_state, Data#data{stream = NewStream}, []};

bind(cast, {received, _Packet}, #data{socket = Socket} = Data) ->
    gen_tcp:send(Socket, ?SESSION), 
    gen_tcp:send(Socket, ?PRESENCE),
    {next_state, binding, Data, []}.

binding(cast, {received, _Packet}, Data) ->
    snatch:connected(?MODULE),
    {next_state, binded, Data, []}.

binded(cast, {send, Packet}, #data{socket = Socket}) ->
    gen_tcp:send(Socket, Packet),
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
handle_event(info, {'$gen_event', {xmlstreamstart, _Name, _Attribs}}, _State, _Data) ->
    {keep_state_and_data, []};
handle_event(info, {'$gen_event', {xmlstreamend, _Name}}, _State, #data{stream = Stream} = Data) ->
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {'$gen_event', {xmlstreamerror, Error}}, _State, #data{stream = Stream} = Data) ->
    error_logger:error_msg("Stream Error: ~p ~n", [Error]),
    snatch:disconnected(?MODULE),
    close_stream(Stream),
    {next_state, retrying, Data, [{next_event, cast, connect}]};
handle_event(info, {'$gen_event', {xmlstreamelement, Packet}}, _State, Data) ->
    {keep_state, Data,[{next_event, cast, {received, Packet}}]};
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

close_stream(<<>>) -> ok;
close_stream(Stream) -> fxml_stream:close(Stream).
