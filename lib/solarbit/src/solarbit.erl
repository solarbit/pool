% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(solarbit).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").

-compile(export_all).

-export([start/0, stop/0]).
-export([info/0, state/0, connect/1]).
-compile(export_all).


start() ->
	application:start(solarbit).


stop() ->
	application:stop(solarbit).


set_bitcoind(Host, Port) when is_tuple(Host), tuple_size(Host) == 4, is_integer(Port) ->
	sbt_btc_srv:set_bitcoind(Host, Port).


start_cpu_miner() ->
	case whereis(sbt_sup) of
	Pid when is_pid(Pid) ->
		sbt_sup:start_service(sbt_miner_srv);
	undefined ->
		{error, {not_running, ?MODULE}}
	end.


genesis() ->
	Request = #btc_getblocks{block_locator_hashes = [?GENESIS_BLOCK]},
	sbt_btc_srv:send(Request).


info() -> [
	db:info(),
	sbt_btc_srv:info()
].


state() ->
	State = [{pool, sys:get_state(sbt_pool_srv, 2000)}, {btcd, sys:get_state(sbt_btc_srv, 2000)}],
	case whereis(sbt_miner_srv) of
	Pid when is_pid(Pid) ->
		State ++ [{miner, sys:get_state(sbt_miner_srv, 2000)}];
	undefined ->
		State
	end.


connect(local) ->
	sbt_btc_srv:connect(local);
connect(pool) ->
	sbt_btc_srv:connect(pool);
connect(remote) ->
	sbt_btc_srv:connect(remote).


ping() ->
	sbt_btc_srv:ping().


send(Miner, ping) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = 'PING', nonce = dttm:now()});
send(Miner, stat) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = 'STAT', nonce = dttm:now()});
send(Miner, wait) ->
	sbt_pool_srv:send(Miner, #sbt_message{type = 'WAIT'});
send(Miner, test) ->
	Hash = btc_crypto:hash256(<<"solarbit.cc">>),
	Payload = <<431498:32/little, (?TEST_BLOCK)/binary, 1, Hash/binary>>,
	sbt_pool_srv:send(Miner, #sbt_message{type = 'MINE', payload = Payload}).


log(M) ->
	?LOG(M).
