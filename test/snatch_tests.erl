-module(snatch_tests).
-compile([warnings_as_errors, debug_info]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fast_xml/include/fxml.hrl").
-include("snatch.hrl").

-define(RECEIVE(What), (fun() ->
    receive
        What -> ok;
        Other -> throw(Other)
    after
        1000 -> throw(timeout)
    end end)()).

-export([init/1, handle_info/2, terminate/2]).
-export([send/2, send/3]).

%% snatch behaviour
init([stop]) ->
    {stop, normal};
init([PID]) ->
    {ok, PID}.

handle_info({connected, stop}, PID) ->
    PID ! stop,
    {stop, normal, PID};
handle_info(Info, PID) ->
    PID ! Info,
    {noreply, PID}.

terminate(_Reason, _State) ->
    ok.

%% claws behaviour
send(Data, JID) ->
    ?MODULE ! {send, Data, JID, undefined},
    ok.

send(Data, JID, ID) ->
    ?MODULE ! {send, Data, JID, ID},
    ok.

snatch_send_and_receive_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, ?MODULE, [self()]),
    register(?MODULE, self()),
    %% says to snatch the claw is connected
    ok = snatch:connected(?MODULE),
    ?RECEIVE({connected, ?MODULE}),
    ok = snatch:received(<<"Hello world!">>),
    ?RECEIVE({received, <<"Hello world!">>}),
    ok = snatch:received(<<"Hello again!">>, #via{}),
    ?RECEIVE({received, <<"Hello again!">>, #via{}}),
    ok = snatch:send(<<"How are you?">>),
    ?RECEIVE({send, <<"How are you?">>, <<"unknown">>, undefined}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>),
    ?RECEIVE({send, <<"How are you?">>, <<"alice@example.com">>, undefined}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>, <<"ID1">>),
    ?RECEIVE({send, <<"How are you?">>, <<"alice@example.com">>, <<"ID1">>}),
    ok = snatch:disconnected(?MODULE),
    ?RECEIVE({disconnected, ?MODULE}),
    ok = snatch:stop(),
    true = unregister(?MODULE),
    ok.

snatch_send_and_receive_router_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, self()),
    register(?MODULE, self()),
    %% says to snatch the claw is connected
    ok = snatch:connected(?MODULE),
    ?RECEIVE({connected, ?MODULE}),
    ok = snatch:received(<<"Hello world!">>),
    ?RECEIVE({received, <<"Hello world!">>}),
    ok = snatch:received(<<"Hello again!">>, #via{}),
    ?RECEIVE({received, <<"Hello again!">>, #via{}}),
    ok = snatch:send(<<"How are you?">>),
    ?RECEIVE({send, <<"How are you?">>, <<"unknown">>, undefined}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>),
    ?RECEIVE({send, <<"How are you?">>, <<"alice@example.com">>, undefined}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>, <<"ID1">>),
    ?RECEIVE({send, <<"How are you?">>, <<"alice@example.com">>, <<"ID1">>}),
    ok = snatch:disconnected(?MODULE),
    ?RECEIVE({disconnected, ?MODULE}),
    ok = snatch:stop(),
    true = unregister(?MODULE),
    ok.

snatch_send_and_receive_via_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, ?MODULE, [self()]),
    register(?MODULE, self()),
    %% says to snatch the claw is connected
    ok = snatch:connected(?MODULE),
    ?RECEIVE({connected, ?MODULE}),
    ok = snatch:received(<<"Hello world!">>),
    ?RECEIVE({received, <<"Hello world!">>}),
    DummyVia = #via{jid = <<"alice@example.com">>, claws = claws_dummy},
    ok = snatch:received(<<"Hello again!">>, DummyVia),
    ?RECEIVE({received, <<"Hello again!">>, DummyVia}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>),
    ?RECEIVE({claws_dummy, <<"How are you?">>, <<"alice@example.com">>, undefined}),
    ok = snatch:send(<<"How are you?">>, <<"alice@example.com">>, <<"ID1">>),
    ?RECEIVE({claws_dummy, <<"How are you?">>, <<"alice@example.com">>, <<"ID1">>}),
    ok = snatch:send(<<"How are you?">>, <<"bob@example.com">>, <<"ID1">>),
    ?RECEIVE({send, <<"How are you?">>, <<"bob@example.com">>, <<"ID1">>}),
    ok = snatch:disconnected(?MODULE),
    ?RECEIVE({disconnected, ?MODULE}),
    ok = snatch:stop(),
    true = unregister(?MODULE),
    ok.

snatch_connected_error_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, ?MODULE, [self()]),
    register(?MODULE, self()),
    %% says to snatch the claw is connected
    ok = snatch:connected(stop),
    ?RECEIVE(stop),
    ?assertEqual(undefined, whereis(snatch)),
    ok.

snatch_init_error_test() ->
    {error, normal} = snatch:start_link(?MODULE, ?MODULE, [stop]),
    ok.

snatch_code_change_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, ?MODULE, [self()]),
    ok = sys:suspend(_PID),
    ok = sys:change_code(snatch, snatch, "0", []),
    ok = sys:resume(snatch),
    ok = snatch:stop(),
    ok.

snatch_silly_calls_test() ->
    {ok, _PID} = snatch:start_link(?MODULE, ?MODULE, [self()]),
    snatch ! whatever,
    ignored = gen_server:call(snatch, whatever),
    ok = snatch:stop(),
    ok.
