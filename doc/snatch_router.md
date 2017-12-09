

# Module snatch_router #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`snatch`](snatch.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle_info-2"></a>

### handle_info/2 ###

<pre><code>
handle_info(Info::term(), PID::pid() | atom()) -&gt; {noreply, pid() | atom()}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[pid() | atom()]) -&gt; {ok, pid() | atom()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::any(), PID::pid() | atom()) -&gt; ok
</code></pre>
<br />

