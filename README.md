

# snatch #

Copyright (c) 2017 Thiago Camargo

__Authors:__ "Thiago Camargo" ([`barata7@gmail.com`](mailto:barata7@gmail.com)).

[![Build Status](https://img.shields.io/travis/snatch-xmpp/snatch/master.svg)](https://travis-ci.org/snatch-xmpp/snatch)
[![Codecov](https://img.shields.io/codecov/c/github/snatch-xmpp/snatch.svg)](https://codecov.io/gh/snatch-xmpp/snatch)
[![License: Apache 2.0](https://img.shields.io/github/license/snatch-xmpp/snatch.svg)](https://raw.githubusercontent.com/snatch-xmpp/snatch/master/LICENSE)

Lightweight XMPP Client Library for Erlang. This library is intended to handle client connections in an agnostic way. The system is launched using the snatch process and configuring as many claws as you need.

The built-in claws are:

- [XMPP Client](http://github.com/manuel-rubio/snatch/blob/master/doc/how-to/claws_xmpp.md)
- [XMPP Component](http://github.com/manuel-rubio/snatch/blob/master/doc/how-to/claws_xmpp_comp.md)
- [XMPP over HTTP Long-polling](http://github.com/manuel-rubio/snatch/blob/master/doc/how-to/claws_lp.md)
- [XMPP over AMQP](https://github.com/snatch-xmpp/claws_rabbitmq)
- [XMPP over Kafka](https://github.com/snatch-xmpp/claws_kafka)

Installation
------------

The system requires OTP 19+ because we use [gen_statem](http://erlang.org/doc/design_principles/statem) and remove the `code_change/3` and `terminate/2`. For OTP 19+ those callbacks are optional.

In the same way we prefer to use [rebar3](http://www.rebar3.org) instead of older versions. To install snatch only needs:

```erlang
{deps, [
    {snatch, {git, "https://github.com/manuel-rubio/snatch.git", {branch, master}}}
]}
```

Or if you are using [erlang.mk](https://erlang.mk) instead, you can use:

```Makefile
DEPS += snatch
dep_snatch = git https://github.com/snatch-xmpp/snatch.git master
```

You'll need a C/C++ compiler installed in your system for [fast_xml](https://github.com/processone/fast_xml) and [stringprep](https://github.com/processone/stringprep).

Configuring
-----------

There are no strict configuration for the application. You have to send the specific information when you start the specific process:

```erlang
{ok, PID} = snatch:start_link(claws_xmpp, my_module, []).
```

This format helps to start snatch using `my_module` as our implementation of the `snatch` behaviour to use the callbacks to implement the behaviour we want to achieve.

The third element (`[]`) are the arguments passed to the `init/1` callback in `my_module`. You can send all of the information you need for your callback.

The specific claws should be started spearately. See the specific information about each one in their specific documentation pages (see above).

Callbacks
---------

You can use this template to develop your own modules:

```erlang
-module(my_own_module).
-behaviour(snatch).

-include_lib("snatch/include/snatch.hrl").

-export([init/1, handle_info/2, terminate/2]).

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
```

Note that if you're using [rebar3](https://rebar3.org) you can use this command instead to create your template:

```
./rebar3 new snatch name=my_own_module
```

The call to the `init/1` function is performed each time the snatch system is started and `terminate/2` when the system is finished properly. Note that in case of internal errors this code will not be called.

The callback `handle_info/2` will be called when a message arrives to snatch.

The events you can receive are:

- `connected`: says the claw (passed as second element in the tuple) is connected properly.
- `disconnected`: says the claw (passed as second element in the tuple) was disconnected.
- `received`: this event has two ways. Only receive a packet (as second element in the tuple) or receive the packet and the `#via{}` information as well. This information gives us information about the way the packet was sent.

The `#via{}` record has the following information:

- `jid` as the JID (Jabber Identification) of the via.
- `exchange` *(only for AMQP)* says the exchange in use for the element.
- `claws` is the module used to receive the message.
- `id` the identification of the packet.

Router / Listener
-----------------

Although the previous (*Callbacks*) section let you to create a router or listener for all of the events received from the claws, maybe you want to receive in your gen_server or gen_statem those events instead of create a module only for that.

In this case you can use the `snatch_router` and instead of use `snatch:start_link/3` you can use `snatch:start_link/2`. This way the second parameter to use for snatch will be a registered name or a PID.

All of the previous events will be send to that process using this way.

Sending
-------

To send information thru snatch to the claws you can use the following functions:

- `send/1` to send a packet directly to the default claw. The JID will be configured as *unknown* for the claw.
- `send/2` to send a packet using a specific JID (as second param). The JID will be in use to retrieve a route from the internal snatch information.
- `send/3` to send a packet using a specific JID (as second param) and pointing the ID of the packet to be handled properly by the claw.

The packet should be a valid XML construction. Your application is in charge to create it in the way you want.

For example:

```erlang
snatch:send(<<"<presence/>">>).
```

Testing
-------

There are a framework to test XMPP easily. For further information: [testing framework](https://github.com/snatch-xmpp/snatch_test_framework).

Troubleshooting
---------------

Feel free to create an issue in github to point a bug, flaw or improvement and even send a pull request with a specific change. Read the [LICENSE](http://github.com/manuel-rubio/snatch/blob/master/doc/LICENSE) if you have doubts about what you can do with the code.

Enjoy!


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/claws.md" class="module">claws</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/claws_lp.md" class="module">claws_lp</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/claws_xmpp.md" class="module">claws_xmpp</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/claws_xmpp_comp.md" class="module">claws_xmpp_comp</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/snatch.md" class="module">snatch</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/snatch_jid.md" class="module">snatch_jid</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/snatch_router.md" class="module">snatch_router</a></td></tr>
<tr><td><a href="http://github.com/manuel-rubio/snatch/blob/master/doc/snatch_xml.md" class="module">snatch_xml</a></td></tr></table>

