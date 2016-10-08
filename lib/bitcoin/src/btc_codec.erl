% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(btc_codec).

-include("bitcoin.hrl").

-compile(export_all).

% base difficulty hex: 0x00000000FFFF0000000000000000000000000000000000000000000000000000
-define(BASE_DIFFICULTY, 26959535291011309493156476344723991336010898738574164086137773096960).


encode(B = #btc_block{}) ->
	#btc_block{version = Version, prev_block = PrevHash, merkle_root = RootHash,
		timestamp = Timestamp, bits = Bits, nonce = Nonce} = B,
	TS = dttm:to_seconds(Timestamp),
	<<Version:32/little, PrevHash:256/little, RootHash:256/little, TS:32/little, Bits:32/little, Nonce:32/little>>;

encode(Tx = #btc_tx{}) ->
	encode([Tx]);

encode(Records) when is_list(Records) ->
	encode(Records, <<>>).


encode([#btc_tx{id = _Id, version = Version, tx_in = TxIn, tx_out = TxOut, lock_time = LockTime}|T], Acc) ->
	TxInSize = encode_integer(length(TxIn)),
	TxInBin = encode_tx_inputs(TxIn, <<>>),
	TxOutSize = encode_integer(length(TxOut)),
	TxOutBin = encode_tx_outputs(TxOut, <<>>),
	Tx = <<Version:32/little, TxInSize/binary, TxInBin/binary, TxOutSize/binary, TxOutBin/binary, LockTime:32/little>>,
	encode(T, <<Acc/binary, Tx/binary>>);
encode([], Acc) ->
	Acc.


encode_tx_inputs([#btc_tx_in{outpoint_ref = OutHash, outpoint_index = Index, sig_script = Script, sequence = Sequence}|T], Acc) ->
	Script0 = btc_script:encode(Script),
	ScriptLength = encode_integer(byte_size(Script0)),
	TxIn = <<OutHash:256, Index:32/little, ScriptLength/binary, Script0/binary, Sequence:32/little>>,
	encode_tx_inputs(T, <<Acc/binary, TxIn/binary>>);
encode_tx_inputs([], Acc) ->
	Acc.


encode_tx_outputs([#btc_tx_out{value = Value, pk_script = Script}|T], Acc) ->
	Script0 = btc_script:encode(Script),
	ScriptLength = encode_integer(byte_size(Script0)),
	TxOut = <<Value:64/little, ScriptLength/binary, Script0/binary>>,
	encode_tx_outputs(T, <<Acc/binary, TxOut/binary>>);
encode_tx_outputs([], Acc) ->
	Acc.


encode_bits(X) ->
	<<X:32/little>>.


encode_integer(X) when X < 253 ->
	<<X>>;
encode_integer(X) when X < 16#FFFF ->
	<<253, X:16/little>>;
encode_integer(X) when X < 16#FFFFFFFF ->
	<<254, X:32/little>>;
encode_integer(X) ->
	<<255, X:64/little>>.


decode_block(<<Header:80/binary, Bin/binary>>) ->
	<<Id:256/little>> = btc_crypto:hash256(Header),
	<<Version:32/little, PrevHash:256/little, RootHash:256/little,
		Time:32/little, Bits:32/little, Nonce:32/little>> = Header,
	{NumTx, Bin0} = decode_integer(Bin),
	case NumTx > 0 of
	true ->
		{Coinbase, BlockHeight, Bin1} = decode_coinbase(Bin0),
		Temp = decode_transactions(Bin1, NumTx - 1, []), % TODO: Temp keep coinbase in TX list
		Txns = [Coinbase|Temp];
	false ->
		BlockHeight = unknown,
		Coinbase = undefined,
		Txns = []
	end,
	#btc_block{
		id = Id,
		height = BlockHeight,
		version = Version,
		prev_block = PrevHash,
		merkle_root = RootHash,
		timestamp = dttm:datetime(unix, Time),
		bits = Bits,
		nonce = Nonce,
		coinbase = Coinbase,
		txns = Txns
	}.


decode_coinbase(Bin = <<Version:32/little, Bin0/binary>>) ->
	{1, Bin1} = decode_integer(Bin0),
	{CoinbaseInput, BlockHeight, Bin2} = decode_coinbase_input(Bin1),
	{TxOut, Bin3} = decode_tx_outputs(Bin2),
	<<LockTime:32/little, Bin4/binary>> = Bin3,
	TxBinSize = byte_size(Bin) - byte_size(Bin4),
	<<TxBin:TxBinSize/binary, _/binary>> = Bin,
	<<Id:256>> = btc_crypto:hash256(TxBin),
	Tx = #btc_tx{id = Id, version = Version, tx_in = [CoinbaseInput], tx_out = TxOut, lock_time = LockTime},
	{Tx, BlockHeight, Bin4}.


decode_coinbase_input(<<Hash:256, Index:32/little, Bin/binary>>) ->
	{ScriptLength, Bin0} = decode_integer(Bin),
	<<Script:ScriptLength/binary, Sequence:32/little, Bin1/binary>> = Bin0,
	case Script of
	<<3, BlockHeight:24/little, _Bin2/binary>> ->
		ok;
	_ ->
		BlockHeight = unknown
	end,
	TxIn = #btc_tx_in{outpoint_ref = Hash, outpoint_index = Index, sig_script = Script, sequence = Sequence},
	{TxIn, BlockHeight, Bin1}.


decode_transaction(Bin) ->
	[Tx] = decode_transactions(Bin, 1, []),
	Tx.


decode_transactions(Bin = <<Version:32/little, Bin0/binary>>, Count, Acc) when Count > 0 ->
	{TxIn, Bin1} = decode_tx_inputs(Bin0),
	{TxOut, Bin2} = decode_tx_outputs(Bin1),
	<<LockTime:32/little, Bin3/binary>> = Bin2,
	TxBinSize = byte_size(Bin) - byte_size(Bin3),
	<<TxBin:TxBinSize/binary, _/binary>> = Bin,
	<<Id:256>> = btc_crypto:hash256(TxBin),
	Tx = #btc_tx{id = Id, version = Version, tx_in = TxIn, tx_out = TxOut, lock_time = LockTime},
	decode_transactions(Bin3, Count - 1, [Tx|Acc]);
decode_transactions(<<>>, 0, Acc) ->
	lists:reverse(Acc).


decode_tx_inputs(Bin) ->
	{NumIn, Bin0} = decode_integer(Bin),
	decode_tx_inputs(Bin0, NumIn, []).

decode_tx_inputs(<<Hash:256, Index:32/little, Bin/binary>>, Count, Acc) when Count > 0 ->
	{ScriptLength, Bin0} = decode_integer(Bin),
	<<Script:ScriptLength/binary, Sequence:32/little, Bin1/binary>> = Bin0,
	Script0 = btc_script:decode(Script),
	TxIn = #btc_tx_in{outpoint_ref = Hash, outpoint_index = Index, sig_script = Script0, sequence = Sequence},
	decode_tx_inputs(Bin1, Count - 1, [TxIn|Acc]);
decode_tx_inputs(Bin, 0, Acc) ->
	{lists:reverse(Acc), Bin}.


decode_tx_outputs(Bin) ->
	{NumOut, Bin0} = decode_integer(Bin),
	decode_tx_outputs(Bin0, NumOut, []).

decode_tx_outputs(<<Value:64/little, Bin/binary>>, Count, Acc) when Count > 0 ->
	{ScriptLength, Bin0} = decode_integer(Bin),
	<<Script:ScriptLength/binary, Bin1/binary>> = Bin0,
	Script0 = btc_script:decode(Script),
	TxOut = #btc_tx_out{value = Value, pk_script = Script0},
	decode_tx_outputs(Bin1, Count - 1, [TxOut|Acc]);
decode_tx_outputs(Bin, 0, Acc) ->
	{lists:reverse(Acc), Bin}.


decode_integer(<<X, Bin/binary>>) when X < 253 ->
	{X, Bin};
decode_integer(<<253, X:16/little, Bin/binary>>) ->
	{X, Bin};
decode_integer(<<254, X:32/little, Bin/binary>>) ->
	{X, Bin};
decode_integer(<<255, X:64/little, Bin/binary>>) ->
	{X, Bin}.



target(#btc_block{bits = Bits}) ->
	target(Bits);
target(Bits) when is_integer(Bits) ->
	<<0:3, Exponent:5, Mantissa:24>> = <<Bits:32>>,
	true = Exponent > 3,
	ShiftCount = (Exponent - 3) bsl 3,
	Mantissa bsl ShiftCount.


difficulty(Block) ->
	?BASE_DIFFICULTY / target(Block).


get_block_height(#btc_block{txns = []}) ->
	unknown;
get_block_height(#btc_block{txns = [Coinbase|_]}) ->
	#btc_tx{tx_in = [#btc_tx_in{sig_script = Script}]} = Coinbase,
	case Script of
	[H|_] when is_binary(H), byte_size(H) == 3 ->
		<<BlockHeight:24/little>> = H;
	_ when is_binary(Script) ->
		<<3, BlockHeight:24/little, _/binary>> = Script;
	_ ->
		BlockHeight = unknown
	end,
	BlockHeight.


-ifdef(TEST).

test(#btc_block{id = ID, merkle_root = Root, bits = Bits, txns = TX}) ->
	Hashes = [CoinbaseHash|_] = [<<X:256>> || #btc_tx{id = X} <- TX],
	Coinbase = #btc_tx{tx_in = [#btc_tx_in{sig_script = SigScript} ]} = hd(TX),
	case SigScript of
	[H|_] when is_binary(H), byte_size(H) == 3 ->
		<<BlockHeight:24/little>> = H;
	[H|_] ->
		BlockHeight = {unknown, H};
	Bin ->
		<<3, BlockHeight:24/little, _/binary>> = Bin
	end,
	<<CoinbaseCheckHash:256/little>> = btc_crypto:hash256(btc_codec:encode(Coinbase)),
	<<CoinbaseCheck:256/little>> = CoinbaseHash,
	{<<Root0:256/little>>, Path} = btc_crypto:hash_root(Hashes),
	<<PathCheck:256/little>> = btc_crypto:merkle_root_from_path(CoinbaseHash, Path),
	{ok, [
		{block, hex:encode(<<ID:256>>)},
		{height, BlockHeight},
		{difficulty, btc_codec:difficulty(Bits)},
		{txns, length(Hashes)},
		{coinbase_hash, hex:encode(<<CoinbaseCheck:256>>)},
		{coinbase_check, hex:encode(CoinbaseCheckHash)},
		{expected_root, hex:encode(Root)},
		{calculated_root, hex:encode(<<Root0:256>>)},
		{path_root, hex:encode(<<PathCheck:256>>)},
		{valid, Root == Root0 andalso Root == PathCheck},
		{path, length(Path), [hex:encode(X) || X <- Path]}
	]}.

-endif.
