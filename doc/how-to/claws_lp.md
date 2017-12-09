Claws XMPP over HTTP Long Polling
=================================

This claw let to use HTTP as a XMPP transport. Using long polling the socket remains open sending chunks from the XMPP server and requests from the client.

The way to start the claw is:

```erlang
Params = #{url => "http://example.com/xmpp"},
{ok, PID} = claws_lp:start_link(Params).
```

The system works creating the connection to the HTTP server but there are no more activities done in the server part. If the remote server has a BOSH configuration, you are in charge to perform the stream init and the whole authentication to operate with the XMPP Server.

The reason because the HTTP connection is so basic is to let you to create your own HTTP communication with the server even if you want to do it in a very easy way.

The way to send and receive information could be read in the [README.md](../README.md) file.
