-module(claws_xmpp_comp_tests).
-compile([warnings_as_errors, debug_info]).

-include_lib("eunit/include/eunit.hrl").
-include("snatch_xml.hrl").
-include("snatch.hrl").

-define(AUTH, <<"<stream:stream "
                "xmlns:stream='http://etherx.jabber.org/streams' "
                "xmlns='jabber:component:accept' "
                "from='news.example.com' "
                "id='3BF96D32'>">>).

-define(AUTH_ERR, <<"<stream>">>).

listen() ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active, true}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    {ok, Socket, Port}.

accept(LSocket) ->
    {ok, _Socket} = gen_tcp:accept(LSocket, 1000).

recv() ->
    receive
        {tcp, _Port, Packet} -> {ok, Packet}
    after
        1000 -> {error, etimeout}
    end.

connect() ->
    connect(false, false, false).

connect(Trimmed, AdjustAttrs, Ping) ->
    {ok, LSocket, Port} = listen(),
    Params = #{host => {127,0,0,1},
               port => Port,
               domain => <<"news.example.com">>,
               password => <<"secret">>,
               trimmed => Trimmed,
               adjust_attrs => AdjustAttrs,
               ping => Ping},
    {ok, _PID} = claws_xmpp_comp:start_link(Params),
    ok = claws_xmpp_comp:connect(),
    {ok, Socket} = accept(LSocket),
    ok = timer:sleep(100),
    {ok, <<"<?xml version='1.0' ", _/binary>>} = recv(),
    ok = gen_tcp:send(Socket, ?AUTH),
    {ok, <<"<handshake>b09ea9b3b7f586be8a08d0a3dd7466f110aeb136</handshake>">>} = recv(),
    ok = gen_tcp:send(Socket, <<"<handshake/>">>),
    ok = timer:sleep(100),
    {ok, LSocket, Socket}.

clean() ->
    receive
        _ -> clean()
    after
        0 -> ok
    end.

get_all(Data) ->
    get_all(Data, 250).

get_all(Data, Time) ->
    receive
        Packet -> get_all([Packet|Data], Time)
    after
        Time -> Data
    end.

disconnect(LSocket, Socket) ->
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(LSocket),
    ok = claws_xmpp_comp:disconnect(),
    clean(),
    ok.

error_connect_test() ->
    {ok, LSocket, Port} = listen(),
    Params = #{host => {127,0,0,1},
               port => Port,
               domain => <<"news.example.com">>,
               password => <<"secret">>},
    {ok, _PID} = claws_xmpp_comp:start_link(Params),
    ok = claws_xmpp_comp:connect(),
    {ok, Socket} = accept(LSocket),
    ok = timer:sleep(100),
    {ok, <<"<?xml version='1.0' ", _/binary>>} = recv(),
    ok = gen_tcp:send(Socket, ?AUTH_ERR),
    ?assertMatch([{tcp_closed, _}], get_all([])),
    ok = disconnect(LSocket, Socket),
    ok.

connect_test() ->
    {ok, LSocket, Socket} = connect(),
    ok = disconnect(LSocket, Socket),
    ok.

send_message_test() ->
    {ok, LSocket, Socket} = connect(),
    ok = claws_xmpp_comp:send(<<"<message type='chat' to='user@example.com'><body>Hi</body></message>">>, undefined),
    ?assertMatch([{tcp, _, <<"<message type='chat' ", _/binary>>}], get_all([])),
    ok = disconnect(LSocket, Socket),
    ok.

send_adjusted_attributes_message_test() ->
    {ok, LSocket, Socket} = connect(false, true, false),
    ok = claws_xmpp_comp:send(<<"<message type='chat'><body>Hi</body></message>">>,
                              <<"user@example.com">>,
                              <<"msg1">>),
    ?assertMatch([{tcp, _, <<"<message id='msg1' "
                                      "from='news.example.com' "
                                      "to='user@example.com' "
                                      "type='chat'>", _/binary>>}],
                 get_all([])),
    ok = disconnect(LSocket, Socket),
    ok.

received_ping_test() ->
    {ok, LSocket, Socket} = connect(false, true, 100),
    true = register(snatch, self()),
    timer:sleep(250),
    ?assertMatch([{tcp, _, <<"\n">>}|_], get_all([], 0)),
    ok = disconnect(LSocket, Socket),
    true = unregister(snatch),
    ok.

received_error_message_test() ->
    {ok, LSocket, Socket} = connect(),
    true = register(snatch, self()),
    ok = gen_tcp:send(Socket, <<"</message>">>),
    ?assertMatch([{'$gen_cast',{disconnected,claws_xmpp_comp}}], get_all([])),
    ok = disconnect(LSocket, Socket),
    true = unregister(snatch),
    ok.

received_stream_end_test() ->
    {ok, LSocket, Socket} = connect(),
    true = register(snatch, self()),
    ok = gen_tcp:send(Socket, <<"</stream:stream>">>),
    ?assertMatch([{'$gen_cast',{disconnected,claws_xmpp_comp}}], get_all([])),
    ok = disconnect(LSocket, Socket),
    true = unregister(snatch),
    ok.

received_message_test() ->
    {ok, LSocket, Socket} = connect(),
    true = register(snatch, self()),
    ok = gen_tcp:send(Socket, <<"<message type='chat' to='news.example.com'>
                                     <body>Hi</body>
                                 </message>">>),
    ?assertMatch([{'$gen_cast', {received, #xmlel{children = [_CData1,#xmlel{name = <<"body">>},_CData2]}, #via{}}}],
                 get_all([])),
    ok = disconnect(LSocket, Socket),
    true = unregister(snatch),
    ok.

received_message_trimmed_test() ->
    {ok, LSocket, Socket} = connect(true, false, false),
    true = register(snatch, self()),
    ok = gen_tcp:send(Socket, <<"<message type='chat' to='news.example.com'>
                                     <body>Hi</body>
                                 </message>">>),
    ?assertMatch([{'$gen_cast', {received, #xmlel{children = [#xmlel{name = <<"body">>}]}, #via{}}}],
                 get_all([])),
    ok = disconnect(LSocket, Socket),
    true = unregister(snatch),
    ok.

reconnect_test() ->
    {ok, LSocket, Socket} = connect(),
    true = register(snatch, self()),
    ok = gen_tcp:send(Socket, <<"<message type='chat' to='news.example.com'><body>Hi</body></message>">>),
    ?assertMatch([{'$gen_cast', {received, #xmlel{}, #via{}}}], get_all([])),
    ok = gen_tcp:close(Socket),
    ok = timer:sleep(3000),
    {ok, Socket2} = accept(LSocket),
    {ok, <<"<?xml version='1.0' ", _/binary>>} = recv(),
    ok = gen_tcp:send(Socket2, ?AUTH),
    {ok, <<"<handshake>b09ea9b3b7f586be8a08d0a3dd7466f110aeb136</handshake>">>} = recv(),
    ok = gen_tcp:send(Socket2, <<"<handshake/>">>),
    ok = timer:sleep(100),
    ok = disconnect(LSocket, Socket2),
    true = unregister(snatch),
    ok.

error_connecting_test() ->
    Params = #{host => {127,0,0,2},
               port => 5000,
               domain => <<"news.example.com">>,
               password => <<"secret">>},
    {ok, _PID} = claws_xmpp_comp:start_link(Params),
    ok = claws_xmpp_comp:connect(),
    timer:sleep(1000),
    ?assertMatch({disconnected, _}, sys:get_state(_PID)),
    ok = claws_xmpp_comp:disconnect(),
    ok.
