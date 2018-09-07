-module(snatch_jid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(JID1, <<"hello@user.domain.com/resource1">>).
-define(JID2, <<"hello@user.domain.com">>).
-define(JID3, <<"user.domain.com/resource2">>).
-define(JID4, <<"user.domain.com">>).

-define(BARE_JID1, <<"hello@user.domain.com">>).
-define(BARE_JID2, <<"hello@user.domain.com">>).
-define(BARE_JID3, <<"user.domain.com">>).
-define(BARE_JID4, <<"user.domain.com">>).

bare_jid_test() ->
    ?assertEqual(?BARE_JID1, snatch_jid:to_bare(?JID1)),
    ?assertEqual(?BARE_JID2, snatch_jid:to_bare(?JID2)),
    ?assertEqual(?BARE_JID3, snatch_jid:to_bare(?JID3)),
    ?assertEqual(?BARE_JID4, snatch_jid:to_bare(?JID4)),
    ok.
