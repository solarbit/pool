% solarbit.hrl

-define(TTY(Term), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, Term])).
-define(LOG(Term), sbt_log_srv:write(Term)).

-define(UDP_PORT, 21314). % <<"SB">>

-define(SBT_MAGIC, 1397574912).
-define(SBT_VERSION, <<0, 1, 0, $A>>).

-record(message, {magic = ?SBT_MAGIC, version = ?SBT_VERSION, nonce = 1, command, payload = <<>>}).

-record(miner, {ip, port, info, time}).

-define(TEST_BLOCK, <<1,0,0,0,234,141,253,92,14,35,8,200,179,166,115,97,93,250,242,143,169,3,156,
	88,73,250,88,75,5,15,0,0,0,0,0,0,34,89,134,254,85,127,253,145,191,7,250,55,
	82,184,161,249,5,88,122,137,30,126,220,11,228,65,119,50,21,45,140,229,12,234,
	249,77,133,33,19,26,112,140,77,210>>).
