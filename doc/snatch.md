

# Module snatch #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-claws">claws()</a> ###


<pre><code>
claws() = claws_rabbitmq | claws_xmpp | claws_xmpp_comp
</code></pre>




### <a name="type-jid">jid()</a> ###


<pre><code>
jid() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#connected-1">connected/1</a></td><td>reports to the implementation callback the connection (claw) is up.</td></tr><tr><td valign="top"><a href="#disconnected-1">disconnected/1</a></td><td>reports to the implementation callback the connecgtion (claw) is down.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>initialize the snatch process.</td></tr><tr><td valign="top"><a href="#received-1">received/1</a></td><td>received Data, it will be handled directly via the implementation.</td></tr><tr><td valign="top"><a href="#received-2">received/2</a></td><td>received Data, it will be handled directly via the implementation
adding the route to know where to send the next data.</td></tr><tr><td valign="top"><a href="#send-1">send/1</a></td><td>send Data to the external connection using the default claw
configured at start of snatch and <code><<"unknown">></code> as JID.</td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td>send Data to the external connection using a claw based on the JID
passed as a param.</td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td>send data to the external connection using a claw based on the JID
passed as a param.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>starts the server using snatch as registered name, using
Claws param to know what kind of handling of connection to use
and PID or Process Name for receive the information comming
from the claws.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>starts the server using snatch as registered name, using
Claws param to know what kind of handling of connection to use
and Module for the callbacks to send the received information.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>stops the snatch server.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="connected-1"></a>

### connected/1 ###

<pre><code>
connected(Data::term()) -&gt; ok
</code></pre>
<br />

reports to the implementation callback the connection (claw) is up.

<a name="disconnected-1"></a>

### disconnected/1 ###

<pre><code>
disconnected(Data::term()) -&gt; ok
</code></pre>
<br />

reports to the implementation callback the connecgtion (claw) is down.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Call, From, S) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, S) -> any()`

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[term()]) -&gt; {ok, #state{claws = <a href="#type-claws">claws()</a>, callback = module(), substate = term()}} | {stop, Reason::atom()}
</code></pre>
<br />

initialize the snatch process. It could be only one per node.

<a name="received-1"></a>

### received/1 ###

<pre><code>
received(Data::term()) -&gt; ok
</code></pre>
<br />

received Data, it will be handled directly via the implementation.

<a name="received-2"></a>

### received/2 ###

<pre><code>
received(Data::term(), Via::#via{}) -&gt; ok
</code></pre>
<br />

received Data, it will be handled directly via the implementation
adding the route to know where to send the next data.

<a name="send-1"></a>

### send/1 ###

<pre><code>
send(Data::term()) -&gt; ok
</code></pre>
<br />

send Data to the external connection using the default claw
configured at start of snatch and `<<"unknown">>` as JID.

<a name="send-2"></a>

### send/2 ###

<pre><code>
send(Data::term(), JID::<a href="#type-jid">jid()</a>) -&gt; ok
</code></pre>
<br />

send Data to the external connection using a claw based on the JID
passed as a param.

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(Data::term(), JID::<a href="#type-jid">jid()</a>, ID::binary()) -&gt; ok
</code></pre>
<br />

send data to the external connection using a claw based on the JID
passed as a param. The ID is sent directly to the claw with the data.

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Claws::<a href="#type-claws">claws()</a>, PIDorName::pid() | atom()) -&gt; {ok, pid()} | {error, any()}
</code></pre>
<br />

starts the server using snatch as registered name, using
Claws param to know what kind of handling of connection to use
and PID or Process Name for receive the information comming
from the claws.

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Claws::<a href="#type-claws">claws()</a>, Module::module(), Args::[term()]) -&gt; {ok, pid()} | {error, any()}
</code></pre>
<br />

starts the server using snatch as registered name, using
Claws param to know what kind of handling of connection to use
and Module for the callbacks to send the received information.
Args could be whatever you need to pass to your init/1 callback.

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

stops the snatch server.

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

