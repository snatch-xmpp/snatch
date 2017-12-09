-module(snatch).
-compile([warnings_as_errors]).

-behaviour(gen_server).

-include("snatch.hrl").

-type claws() :: claws_rabbitmq | claws_xmpp | claws_xmpp_comp.

-record(state, {claws :: claws(),
                callback :: module(),
                substate :: term()}).

-export([start_link/2, start_link/3, stop/0]).
-export([send/3, send/2, send/1, received/1, received/2, connected/1,
         disconnected/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-callback init(Args :: term()) ->
          {ok, State :: term()} |
          {stop, Reason :: atom()}.
-callback handle_info(Info :: term(), State :: term()) ->
          {noreply, State :: term()} |
          {stop, Reason :: atom(), State :: term()}.
-callback terminate(Reason :: atom(), State :: term()) -> ok.

-type jid() :: binary().

-spec start_link(claws(), pid() | atom()) -> {ok, pid()} | {error, any()}.
%% @doc starts the server using snatch as registered name, using
%%      Claws param to know what kind of handling of connection to use
%%      and PID or Process Name for receive the information comming
%%      from the claws.
start_link(Claws, PIDorName) when is_pid(PIDorName) orelse is_atom(PIDorName) ->
    start_link(Claws, snatch_router, [PIDorName]).


-spec start_link(claws(), module(), [term()]) -> {ok, pid()} | {error, any()}.
%% @doc starts the server using snatch as registered name, using
%%      Claws param to know what kind of handling of connection to use
%%      and Module for the callbacks to send the received information.
%%      Args could be whatever you need to pass to your init/1 callback.
start_link(Claws, Module, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Claws, Module, Args], []).


-spec stop() -> ok.
%% @doc stops the snatch server.
stop() ->
    ok = gen_server:stop(?MODULE).


-spec send(Data :: term(), jid(), ID :: binary()) -> ok.
%% @doc send data to the external connection using a claw based on the JID
%%      passed as a param. The ID is sent directly to the claw with the data.
send(Data, JID, ID) ->
    ok = gen_server:cast(?MODULE, {send, Data, JID, ID}).


-spec send(Data :: term(), jid()) -> ok.
%% @doc send Data to the external connection using a claw based on the JID
%%      passed as a param.
send(Data, JID) ->
    ok = gen_server:cast(?MODULE, {send, Data, JID}).


-spec send(Data :: term()) -> ok.
%% @doc send Data to the external connection using the default claw
%%      configured at start of snatch and `<<"unknown">>' as JID.
send(Data) ->
    ok = gen_server:cast(?MODULE, {send, Data}).


-spec received(Data :: term()) -> ok.
%% @doc received Data, it will be handled directly via the implementation.
received(Data) ->
    ok = gen_server:cast(?MODULE, {received, Data}).


-spec received(Data :: term(), #via{}) -> ok.
%% @doc received Data, it will be handled directly via the implementation
%%      adding the route to know where to send the next data.
received(Data, Via) ->
    ok = gen_server:cast(?MODULE, {received, Data, Via}).


-spec connected(Data :: term()) -> ok.
%% @doc reports to the implementation callback the connection (claw) is up.
connected(Data) ->
    ok = gen_server:cast(?MODULE, {connected, Data}).


-spec disconnected(Data :: term()) -> ok.
%% @doc reports to the implementation callback the connecgtion (claw) is down.
disconnected(Data) ->
    ok = gen_server:cast(?MODULE, {disconnected, Data}).


-spec init([term()]) -> {ok, #state{}} | {stop, Reason :: atom()}.
%% @doc initialize the snatch process. It could be only one per node.
init([Claws, Module, Args]) ->
    via = ets:new(via, [named_table, {keypos, 2}]),
    case Module:init(Args) of
        {ok, State} ->
            {ok, #state{claws = Claws,
                        callback = Module,
                        substate = State}};
        {stop, Reason} ->
            {stop, Reason}
    end.


handle_call(_Call, _From, S) ->
    {reply, ignored, S}.


handle_cast({send, Data}, #state{claws = Claws} = S) ->
    Claws:send(Data, <<"unknown">>),
    {noreply, S};

handle_cast({send, Data, JID}, #state{claws = Claws} = S) ->
    Route = get_route(JID, Claws),
    Route:send(Data, JID),
    {noreply, S};

handle_cast({send, Data, JID, ID}, #state{claws = Claws} = S) ->
    Route = get_route(JID, Claws),
    Route:send(Data, JID, ID),
    {noreply, S};

handle_cast({received, _Data, #via{} = Route} = Msg,
            #state{callback = Module} = State) ->
    add_route(Route),
    forward(Module, Msg, State);

handle_cast({received, _Data} = Msg, #state{callback = Module} = State) ->
    forward(Module, Msg, State);

handle_cast({connected, _} = Msg, #state{callback = Module} = State) ->
    forward(Module, Msg, State);

handle_cast({disconnected, _} = Msg, #state{callback = Module} = State) ->
    forward(Module, Msg, State).


handle_info(_Info, S) ->
    {noreply, S}.


terminate(Reason, #state{callback = Module, substate = SubState}) ->
    Module:terminate(Reason, SubState).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec add_route(#via{} | undefined) -> ok.
%% @doc add route to ets to send back stanzas.
add_route(#via{jid = JID, claws = Claws}) when JID =/= undefined
                                       andalso Claws =/= undefined ->
    true = ets:insert(via, #via{jid = JID, claws = Claws});

add_route(_) ->
    ok.


-spec get_route(JID :: binary(), Default :: claws()) -> claws().
%% @doc gets route stored in ets or the default if it's not found.
get_route(JID, Default) ->
    case ets:lookup(via, JID) of
        [#via{claws = Claws}|_] ->
            Claws;
        [] ->
            Default
    end.


-spec forward(module(), Data :: term(), State :: term()) ->
      {noreply, term()} | {stop, atom(), term()}.
%% @doc Forward information to the handler via callback.
%% @private
forward(Module, Data, #state{substate = SubState} = State) ->
    case Module:handle_info(Data, SubState) of
        {noreply, NewSubState} ->
            {noreply, State#state{substate = NewSubState}};
        {stop, Reason, NewSubState} ->
            {stop, Reason, State#state{substate = NewSubState}}
    end.
