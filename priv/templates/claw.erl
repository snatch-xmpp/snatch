-module({{name}}).

-behaviour(claws).
-behaviour(gen_server).

-export([send/2, send/3]).
-export([init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-include_lib("snatch/include/snatch.hrl").

-record(state, {}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% TODO perform claw connection/configuration here
    {ok, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec send(Data::binary(), JID::binary()) -> any().
send(Data, JID) ->
    send(Data, JID, undefined).

-spec send(Data::binary(), JID::binary(), ID::binary()) -> any().
send(Data, JID, ID) ->
    gen_server:cast(?MODULE, {send, Data, JID, ID}).
