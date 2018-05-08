-module(snatch_app).
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    Modules = application:get_env(snatch, modules, []),
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 1},
          children(Modules)}}.

children(Claws) ->
    lists:map(fun({Module, ConfigKey}) ->
        ConfigVal = application:get_env(snatch, ConfigKey, []),
        #{ id => ConfigKey,
           start => {Module, start_link, ConfigVal},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [snatch, Module] }
    end, Claws).
