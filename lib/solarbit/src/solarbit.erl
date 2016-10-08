% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(solarbit).

-include("solarbit.hrl").

-compile(export_all).

-export([start/0, stop/0]).
-export([info/0, key/1, state/0, send/2, coinbase/0, connect/1]).


start() ->
	application:start(solarbit).


stop() ->
	application:stop(solarbit).


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
connect(remote) ->
	sbt_btc_srv:connect(remote).


coinbase() ->
	{ok, Miners} = miners(),
	[{Host, sbt_pool_srv:coinbase(Miner)} || Miner = #sbt_miner{ip = Host} <- Miners].


send(Message) ->
	sbt_btc_srv:send(Message).


send(Miner, ping) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"PING">>});
send(Miner, stat) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"STAT">>});
send(Miner, mine) ->
	Hash = btc_crypto:hash256(<<"solarbit.cc">>),
	Payload = <<431498:32/little, (?TEST_BLOCK)/binary, 1, Hash/binary>>,
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"MINE">>, payload = Payload});
send(Miner, test) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = <<"TEST">>, payload = ?TEST_BLOCK}).
