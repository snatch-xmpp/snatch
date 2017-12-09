-module(claws).

-callback send(Data::binary(), JID::binary()) -> any().
-callback send(Data::binary(), JID::binary(), ID::binary()) -> any().
