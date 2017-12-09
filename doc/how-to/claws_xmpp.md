Claws XMPP Client
=================

The mission of this claw is create a connection to a XMPP Server as a client to receive and send stanzas from/to the Server from/to the implementation. To see more information about the implementation check [Callbacks section in README.md](../README.md#Callbacks).

This claw should be started as follow:

```erlang
Params = #{host => "example.com",
           port => 5222,
           user => <<"guest">>,
           password => <<"guest">>,
           resource => <<"guest">>,
           domain => <<"example.com">>},
{ok, PID} = claws_xmpp:start_link(Params).
```

To connect to the system you have to call to the function:

```erlang
claws_xmpp_comp:connect().
```

The params passed inside of the map for the `start_link/1` function are:

- `host` the Host to connect to the system. It could be a string with a name or a 4-element tuple for the IP (i.e. `{127,0,0,1}`).
- `port` an integer for the number of port to be connected.
- `user` the username to perform the authentication and it will be the node part in the JID composition.
- `password` the password to perform the authentication.
- `resource` the resource to bind the user when the session is created. It will be part of the full JID information.
- `domain` the XMPP domain where the user is logged in. This will be used for the JID composition.

The claw is following different states to perform the connection. The claw is automatically performing this activity in a simple and fast way:

- *Disconnected* is the init state. When the event `connect` is received then it moves to the *Connected* state if the connection is successful or to the *Retrying* state otherwise.
- *Retrying* is the state where the connection stays for 3 seconds until it tries to connect again.
- *Connected* is in charge of create the *fast XML* flow and moves to *Stream Init* state to continue.
- *Stream Init* sends the stream initiation to the server. When receives information from the server it moves to *Authenticate* state. Note that it doesn't check what kind of information is received from the server but usually when there are an error the XMPP Server drops the connection.
- *Authenticate* sends the auth information to the server and then the SASL information for the authentication. It moves to *Bind*.
- *Bind* sends the binding information to bind the session/connection to the specific resource. Is in charge to init the session, send the binding information and the presence. After that it moves to *Binding*.
- *Binding* only ensures the information is sent to `snatch` at this point using the `snatch:connected/1` function. After that is moves to the final state *Binded*.
- *Binded* is kept to handle the incoming and outgoing information. The information arrived from the connection is sent to snatch to be redirected using `snatch:received/2` function. The information sent by the specific implementation is sent directly to the socket (Note that this information MUST be in XML format).

If an event about TCP error or closed is received the state is moved directly to *Retrying*. This way the claw can try to connect again to the server.
