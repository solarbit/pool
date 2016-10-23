% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(bitcoin).

-include("bitcoin.hrl").

-compile(export_all).


address() ->
	btc_crypto:generate_address().

address(Wif) ->
	btc_crypto:generate_address(Wif).


encode(Record) ->
	btc_codec:encode(Record).


decode(block, Bin) ->
	btc_codec:decode_block(Bin);
decode(tx, Bin) ->
	btc_codec:decode_tx(Bin);
decode(coinbase, Bin) ->
	btc_codec:decode_coinbase(Bin).


rules(BlockHeight) when is_integer(BlockHeight), BlockHeight >= 0 ->
	BlockVersion = 4,
	Reward = btc_codec:get_block_reward(BlockHeight),
	DifficultyBits = btc_codec:get_bits(?BASE_DIFFICULTY), % TODO: !!!
	{ok, BlockVersion, Reward, DifficultyBits}.


get_merkle_root(Coinbase, TxnList) ->
	TxHashes = [<<X:256/little>> || X <- TxnList],
	CoinbaseHash = btc_crypto:hash256(btc_codec:encode(Coinbase)),
	{Root, _Path} = btc_crypto:merkle_root([CoinbaseHash|TxHashes]),
	{ok, Root}.


%% NOTE: local machine
-define(BLOCK_DIR, os:getenv("HOME") ++ "/bitcoin/").

extract(File) ->
	extract(File, 1).

extract(File, Limit) ->
	Path = ?BLOCK_DIR ++ File,
	case filelib:is_regular(Path) of
	true ->
		{ok, Fd} = file:open(Path, [binary, read]),
		{ok, Blocks} = extract_blocks(Fd, Limit, []),
		ok = file:close(Fd),
		{ok, Blocks};
	false ->
		{invalid_path, Path}
	end.

extract_blocks(Fd, Count, Acc) when Count > 0 ->
	case file:read(Fd, 8) of
	{ok, <<?BITCOIN_MAGIC:32, BlockSize:32/little>>} ->
		{ok, Bin} = file:read(Fd, BlockSize),
		Block = btc_codec:decode_block(Bin),
		extract_blocks(Fd, Count - 1, [Block|Acc]);
	{ok, <<0:64>>} ->
		{ok, lists:reverse(Acc)};
	{ok, Other} ->
		?TTY({unexpected, Other}),
		{ok, lists:reverse(Acc)};
	eof ->
		{ok, lists:reverse(Acc)};
	Error ->
		Error
	end;
extract_blocks(_, _, Acc) ->
	{ok, lists:reverse(Acc)}.


utxo(N) ->
	Path = ?BLOCK_DIR ++ "blk00400.dat",
	case filelib:is_regular(Path) of
	true ->
		{ok, Fd} = file:open(Path, [binary, read]),
		UTXO = extract_utxo(Fd, 0, N, #{}),
		ok = file:close(Fd),
		UTXO;
	false ->
		{invalid_path, Path}
	end.


extract_utxo(Fd, Count, Max, Map) when Count =< Max ->
	case file:read(Fd, 8) of
	{ok, <<?BITCOIN_MAGIC:32, BlockSize:32/little>>} ->
		{ok, Bin} = file:read(Fd, BlockSize),
		Block = #btc_block{height = Height, timestamp = TS} = btc_codec:decode_block(Bin),
		Map0 = parse_utxo(Block, Map),
		?TTY({block, Count, Height, {utxo, maps:size(Map0)}, TS}),
		extract_utxo(Fd, Count + 1, Max, Map0);
	{ok, <<0:64>>} ->
		{ok, Map};
	{ok, Other} ->
		?TTY({unexpected, Other}),
		{ok, Map};
	eof ->
		{ok, Map};
	Error ->
		Error
	end;
extract_utxo(_Fd, _, _, Map) ->
	Map.


parse_utxo(#btc_block{txns = [Coinbase|TX]}, Map) ->
	Map0 = parse_utxo_out([Coinbase|TX], Map),
	Map1 = parse_utxo_in(TX, Map0),
	Map1.


parse_utxo_out([#btc_tx{id = TxId, tx_out = Out}|T], Map) ->
	Map0 = parse_utxo_out(TxId, Out, 0, Map),
	parse_utxo_out(T, Map0);
parse_utxo_out([], Map) ->
	Map.

parse_utxo_out(TxId, [#btc_tx_out{value = V}|T], N, Map) ->
	parse_utxo_out(TxId, T, N + 1, maps:put({TxId, N}, V, Map));
parse_utxo_out(_, [], _N, Map) ->
	Map.


parse_utxo_in([#btc_tx{tx_in = In}|T], Map) ->
	Map0 = parse_utxo_in0(In, Map),
	parse_utxo_in(T, Map0);
parse_utxo_in([], Map) ->
	Map.


parse_utxo_in0([#btc_tx_in{outpoint_ref = TxId, outpoint_index = N}|T], Map) ->
	parse_utxo_in0(T, maps:remove({TxId, N}, Map));
parse_utxo_in0([], Map) ->
	Map.
