-module({{name}}).

-behaviour(snatch).

-export([init/1, handle_info/2, terminate/2]).

-include_lib("snatch/include/snatch.hrl").

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_info({connected, _Claw}, State) ->
    {noreply, State};

handle_info({disconnected, _Claw}, State) ->
    {noreply, State};

handle_info({received, _Packet, #via{}}, State) ->
    {noreply, State};

handle_info({received, _Packet}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
