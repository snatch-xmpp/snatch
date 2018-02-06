Claws XMPP Component
====================

This claw is designed to connect to a XMPP Server using the [Jabber Component Protocol (XEP-0114)](https://xmpp.org/extensions/xep-0114.html) to receive and send stanzas from/to the Server from/to the implementation. To see more information about the implementation check [Callbacks section in README.md](../README.md#Callbacks).

This claw should be started as follow:

```erlang
Params = #{host => "example.com",
           port => 5222,
           password => <<"guest">>,
           domain => <<"example.com">>,
           trimmed => false,
           adjust_attrs => false,
           ping => false},
{ok, PID} = claws_xmpp_comp:start_link(Params).
```

To connect to the system you have to call to the function:

```erlang
claws_xmpp_comp:connect().
```

The params passed inside of the map for the `start_link/1` function are:

- `host` the Host to connect to the system. It could be a string with a name or a 4-element tuple for the IP (i.e. `{127,0,0,1}`).
- `port` an integer for the number of port to be connected.
- `password` the password to perform the authentication.
- `domain` the XMPP domain where the component registered. This will be used for the JID composition.
- `trimmed` is a special (an optional) option that let you to do more processing in the snatch part. If you configure as `true` the system will remove all of the empty *cdata* entries (only with spaces and/or line feeds).
- `adjust_attrs` is a special (an optional) option that let us to change the `from`, `to` and `id` attributes before to send the packet to the XMPP Server, based on the information received from `send/3` function. Note that the `from` is retrieved from the domain stored in the claw.
- `ping` is a function to send periodically a ping if there are no traffic for the configured seconds. The default value is `false` and you can use a positive integer or `false`.

The claw is following different states to perform the connection. The claw is automatically performing this activity in a simple and fast way:

- *Disconnected* is the init state. When the event `connect` is received then it moves to the *Connected* state if the connection is successful or to the *Retrying* state otherwise.
- *Retrying* is the state where the connection stays for 3 seconds until it tries to connect again.
- *Connected* is in charge of create the *fast XML* flow and moves to *Stream Init* state to continue.
- *Stream Init* sends the stream initiation to the server. When receives information from the server it moves to *Authenticate* state. The stream is checked to get the ID to perform the authetication. If the ID isn't there an error happens and the connection is restarted (closed and opened again in *Retrying* state).
- *Authenticate* sends the *handshake* information to the server. It moves to *Ready* state if the returns from the server was correct, crash otherwise.
- *Ready* is kept to handle the incoming and outgoing information. The information arrived from the connection is sent to snatch to be redirected using `snatch:received/2` function. The information sent by the specific implementation is sent directly to the socket (Note that this information MUST be in XML format).

If an event about TCP error or closed is received the state is moved directly to *Retrying*. This way the claw can try to connect again to the server.
