-module(claws_lp).

-behaviour(gen_server).
-behaviour(claws).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([send/2, send/3]).

-export([read_chunk/3]).

-include_lib("fast_xml/include/fxml.hrl").

-record(state, {
    url :: string(),
    channel :: httpc:request_id(),
    params,
    pid :: pid(),
    buffer = <<>> :: binary(),
    size = -1 :: integer()
}).

-define(EOL, <<"\r\n">>).

start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(#{url := URL}) ->
    gen_server:cast(?MODULE, connect),
    {ok, #state{url = URL}}.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(connect, #state{url = URL} = State) ->
    Opts = [{sync, false},
            {stream, self},
            {full_result, false},
            {socket_opts, [{nodelay, true}]}],
    case httpc:request(get, {URL, []}, [], Opts) of
        {ok, Channel} ->
            {noreply, State#state{channel = Channel, params = undefined}};
        _ -> 
            {noreply, State#state{channel = undefined, params = undefined}}
    end;

handle_cast(_Msg, State) -> 
    {noreply, State}.


handle_info({http, {_Pid, stream_start, Params}}, State) ->
    snatch:connected(?MODULE),
    {noreply, State#state{params = Params}};

handle_info({http, {_Pid, stream_start, Params, Pid}}, State) ->
    snatch:connected(?MODULE),
    httpc:stream_next(Pid),
    {noreply, State#state{params = Params, pid = Pid}};

handle_info({http, {_Pid, stream_end, Params}}, State) ->
    snatch:disconnected(?MODULE),
    {noreply, State#state{params = Params, channel = undefined}};

handle_info({http, {_Pid, {error, _Reason}}}, State) ->
    snatch:disconnected(?MODULE),
    gen_server:cast(?MODULE, connect),
    {noreply, State#state{channel = undefined}};    

handle_info({http, {_Pid, stream, Data}},
            #state{buffer = Buffer, size = Size, pid = Pid} = State) ->
    NState = case read_chunk(Size, Buffer, Data) of
        {wait, NSize, NBuffer} ->
            State#state{buffer = NBuffer, size = NSize};
        {chunk, _S, Packet, Rem} ->
            snatch:received(Packet),
            State#state{buffer = Rem, size = -1}
        end,
    httpc:stream_next(Pid),
    {noreply, NState};

handle_info(refresh, #state{pid = Pid} = State) ->
    httpc:stream_next(Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, #state{channel = Channel}) when Channel /= undefined ->
    httpc:cancel_request(Channel),
    ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send(Data, JID) ->
    gen_server:cast(?MODULE, {send, Data, JID}).


send(Data, JID, _ID) ->
    gen_server:cast(?MODULE, {send, Data, JID}).


read_chunk(-1, Buffer, Data) ->
    Bin = <<Buffer/binary, Data/binary>>,
    {I, J} = binary:match(Bin, ?EOL),
    SizeBin = binary:part(Bin, {0, I + J - byte_size(?EOL)}),
    Size = erlang:binary_to_integer(SizeBin, 16),
    Offset = I + J,
    Chunk = binary:part(Bin, {Offset, byte_size(Bin) - Offset}),
    read_chunk(Size, Chunk, <<>>);

read_chunk(Size, Buffer, Data)
        when byte_size(Buffer) + byte_size(Data) >= Size ->
    Bin = <<Buffer/binary, Data/binary>>,
    SizeBin = binary:part(Bin, {0, Size}),
    Chunk = binary:part(Bin, {Size, byte_size(Bin) - Size}),
    {chunk, Size, SizeBin, Chunk};

read_chunk(Size, Buffer, Data) ->
    {wait, Size, <<Buffer/binary, Data/binary>>}.
