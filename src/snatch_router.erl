-module(snatch_router).
-compile([warnings_as_errors]).

-behaviour(snatch).

-export([init/1, handle_info/2, terminate/2]).

-spec init([pid() | atom()]) -> {ok, pid() | atom()}.
init([undefined]) ->
    erlang:error(badarg);
init([Name]) when is_atom(Name) ->
    {ok, Name};
init([PID]) when is_pid(PID) ->
    {ok, PID}.

-spec handle_info(Info :: term(), pid() | atom()) ->
      {noreply, pid() | atom()}.
handle_info(Info, PID) ->
    PID ! Info,
    {noreply, PID}.

-spec terminate(Reason :: any(), pid() | atom()) -> ok.
terminate(_Reason, _PID) ->
    ok.
