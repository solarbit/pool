-module(solarbit).

-include("solarbit.hrl").

% TEMP
-include_lib("bitcoin/include/bitcoin.hrl").
-compile(export_all).

-export([start/0, stop/0, miners/0, info/0, send/2, coinbase/0, connect/1, remote/1, remote/2]).


start() ->
	application:start(solarbit).


stop() ->
	application:stop(solarbit).


miners() ->
	sbt_pool_srv:miners().


info() ->
	sbt_pool_srv:state().


coinbase() ->
	{ok, Miners} = miners(),
	[{Host, sbt_pool_srv:coinbase(Miner)} || Miner = #miner{ip = Host} <- Miners].


send(Miner, ping) ->
	sbt_pool_srv:send(Miner, #message{type = <<"PING">>});
send(Miner, stat) ->
	sbt_pool_srv:send(Miner, #message{type = <<"STAT">>});
send(Miner, mine) ->
	Hash = crypto:hash(sha256, <<"solarbit.cc">>),
	Payload = <<431498:32/little, (?TEST_BLOCK)/binary, 1, Hash/binary>>,
	sbt_pool_srv:send(Miner, #message{type = <<"MINE">>, payload = Payload});
send(Miner, test) ->
	sbt_pool_srv:send(Miner, #message{type = <<"TEST">>, payload = ?TEST_BLOCK}).


connect(Node) ->
	pong == net_adm:ping(Node).


remote(Command) ->
	case nodes() of
	[Node] ->
		rpc:call(Node, solarbit, Command, [], 3000);
	[] ->
		error
	end.

remote(Miner, Command) ->
	case nodes() of
	[Node] ->
		rpc:cast(Node, solarbit, send, [Miner, Command]);
	[] ->
		error
	end.


%% {ok, B} = blockchain_analyze:extract(<<"blk00000.dat">>).
test(#btc_block{id = ID, merkle_root = Root, txns = TX}) ->
	?TTY(hex:encode(<<ID:256>>)),
	Hashes = [Coinbase|_] = [<<X:256>> || #btc_tx{id = X} <- TX],
	?TTY([hex:encode(X) || X <- Hashes]),
	{<<Root0:256/little>>, Path} = merkle_root(Hashes),
	<<PathCheck:256/little>> = root(Coinbase, Path),
	{length(Hashes), Root == Root0, hex:encode(Root), hex:encode(<<Root0:256>>), [hex:encode(X) || X <- Path], hex:encode(PathCheck)}.


root(Hash, [H|T]) ->
	Hash0 = hash256(<<Hash/binary, H/binary>>),
	root(Hash0, T);
root(Root, []) ->
	Root.


merkle_root(Hashes = [_, X|_]) ->
	{_Root, _CoinbasePath} = merkle_root(Hashes, [], [X]);
merkle_root([Hash]) ->
	{Hash, []};
merkle_root([]) ->
	{<<0:256>>, []}.

merkle_root([], [Root], Path) ->
	{Root, lists:reverse(Path)};
merkle_root([Hash0, Hash1|T], Acc, Path) ->
	Hash2 = hash256(<<Hash0/binary, Hash1/binary>>),
	merkle_root(T, [Hash2|Acc], Path);
merkle_root([Hash0], Acc, Path) ->
	merkle_root([Hash0, Hash0], Acc, Path);
merkle_root([], Acc, Path) ->
	Acc0 = [_, Next|_] = lists:reverse(Acc),
	merkle_root(Acc0, [], [Next | Path]).


hash256(Bin) when is_binary(Bin) ->
	Bin0 = crypto:hash(sha256, Bin),
	crypto:hash(sha256, Bin0).
