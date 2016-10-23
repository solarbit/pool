% solarbit.hrl
% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-include_lib("base/include/base.hrl").

-define(LOG(Term), sbt_log_srv:write(?MODULE, Term)).

-define(UDP_PORT, 21314). % <<"SB">>

-define(SBT_MAGIC, 1397574912). % <<"SMM", 0>>
-define(SBT_VERSION, <<0, 4, 0, $A>>). % semver: 0.4.0-A

-define(NULL_XXTEA_KEY, <<0:128>>).

-define(COINBASE_ID, <<"//SolarBit/SMM/A/">>).
-define(SBT_BTC_CLIENT, <<"/SolarBit:0.4.0-A/">>).

-define(SBT_MINER_FLAGS, [solar, hardware, reserved, tethered, ready, compact, valid, paused]).

-record(sbt_message, {host = localhost, magic = ?SBT_MAGIC, version = ?SBT_VERSION, nonce = 0, type, payload = #{}}).

-record(sbt_miner, {host, port, flags, address, key, time}).


-define(DB_TABLES, [
	{sbt_config, [key, value]},
	{sbt_miner, record_info(fields, sbt_miner)},
	{btc_height, [height, block_key]},
	{btc_block, [key, value]},
	{btc_tx, [key, value]}
]).


-define(TEST_BLOCK, <<1,0,0,0,234,141,253,92,14,35,8,200,179,166,115,97,93,250,242,143,169,3,156,
	88,73,250,88,75,5,15,0,0,0,0,0,0,34,89,134,254,85,127,253,145,191,7,250,55,
	82,184,161,249,5,88,122,137,30,126,220,11,228,65,119,50,21,45,140,229,12,234,
	249,77,133,33,19,26,112,140,77,210>>).
