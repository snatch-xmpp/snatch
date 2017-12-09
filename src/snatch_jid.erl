-module(snatch_jid).
-compile([warnings_as_errors]).

-export([is_full/1, to_bare/1]).


-spec is_full(binary()) -> boolean().
%% @doc returns true if the JID is a full JID, false otherwise.
is_full(JID) when is_binary(JID) -> 
    case parse(JID) of
        {_, _, <<>>} -> false;
        {_, _, _} -> true;
        {error, enojid} -> false
    end.


-spec to_bare(binary()) -> binary().
%% @doc converts JID to a bare JID in binary format.
to_bare(JID) ->
    {Node, Server, _} = parse(JID),
    <<Node/binary, "@", Server/binary>>.


-spec parse(JID :: binary()) ->
      {binary(), binary(), binary()} | {error, enojid}.
%% @doc parse a binary to a {node, server, resource} tuple.
parse(JID) ->
    Opts = [{capture, all, binary}],
    case re:run(JID, "^([^@]+)@([^/]+)(/(.*))?$", Opts) of
        {match, [_, Node, Server]} ->
            {Node, Server, <<>>};
        {match, [_, Node, Server, _, Res]} ->
            {Node, Server, Res};
        nomatch ->
            {error, enojid}
    end.
