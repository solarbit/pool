-module(solarbit).

-include("solarbit.hrl").
-export([start/0, stop/0, miners/0, info/0, send/2]).


start() ->
	application:start(solarbit).


stop() ->
	application:stop(solarbit).


miners() ->
	sbt_pool_srv:miners().


info() ->
	sbt_pool_srv:state().


send(Miner, helo) ->
	sbt_pool_srv:send(Miner, #message{command = <<"HELO">>});
send(Miner, stat) ->
	sbt_pool_srv:send(Miner, #message{command = <<"STAT">>});
send(Miner, test) ->
	sbt_pool_srv:send(Miner, #message{command = <<"TEST">>, payload = ?TEST_BLOCK}).
