% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(solarbit).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").

-compile(export_all).

-export([start/0, stop/0]).
-export([info/0, key/1, state/0, send/2, coinbase/0, connect/1]).


start() ->
	application:start(solarbit).


stop() ->
	application:stop(solarbit).


genesis() ->
	Request = #btc_getblocks{block_locator_hashes = [?GENESIS_BLOCK]},
	sbt_btc_srv:send(Request).


miners() ->
	sbt_pool_srv:miners().


key(Key) when is_binary(Key) ->
	sbt_pool_srv:set_key(Key).


info() -> [
	sbt_pool_srv:miners(),
	sbt_btc_srv:info()
].


state() -> [
	sys:get_state(sbt_pool_srv, 2000),
	sys:get_state(sbt_btc_srv, 2000)
].


connect(local) ->
	sbt_btc_srv:connect(local);
connect(pool) ->
	sbt_btc_srv:connect(pool);
connect(remote) ->
	sbt_btc_srv:connect(remote).


coinbase() ->
	{ok, Miners} = miners(),
	[{Host, sbt_pool_srv:coinbase(Miner)} || Miner = #sbt_miner{ip = Host} <- Miners].


ping() ->
	sbt_btc_srv:ping().

send(Miner, ping) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"PING">>});
send(Miner, stat) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"STAT">>});
send(Miner, wait) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"WAIT">>});
send(Miner, test) ->
	Hash = btc_crypto:hash256(<<"solarbit.cc">>),
	Payload = <<431498:32/little, (?TEST_BLOCK)/binary, 1, Hash/binary>>,
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"MINE">>, payload = Payload}).


log(M) ->
	?LOG(M).


test() ->
	Key = <<"SolarBitSolarBit">>,
	{ok, Socket} = gen_udp:open(?UDP_PORT, [binary]),
	Message = #sbt_message{type = <<"HELO">>, nonce = dttm:now()},
	?TTY({request, Message}),
	Bin = sbt_codec:encode(Key, Message),
	ok = gen_udp:send(Socket, "solarbit.cc", ?UDP_PORT, Bin),
	receive
	{udp, Socket, Host, Port, Packet} ->
		?TTY({response, Host, Port, sbt_codec:decode(Key, Packet)});
	Other ->
		?TTY({other, Other})
	after 5000 ->
		timeout
	end,
	gen_udp:close(Socket).
