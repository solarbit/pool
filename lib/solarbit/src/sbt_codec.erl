% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(sbt_codec).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").
-include_lib("bitcoin/include/bitcoin_script.hrl").

-export([encode/2, decode/2, coinbase/1]).
-compile(export_all).

decode(Key, <<?SBT_MAGIC:32, Version:4/binary, Number:32/little, Type:4/binary, Size:32/little, Bin/binary>>) ->
	Type0 = decode_type(Type),
	Payload = decrypt_payload(Key, Size, Bin),
	Payload0 = decode_payload(Type0, Payload),
	#sbt_message{magic = ?SBT_MAGIC, version = Version, nonce = Number, type = Type0, payload = Payload0}.


decode_type(<<"PING">>) -> 'PING';
decode_type(<<"HELO">>) -> 'HELO';
decode_type(<<"SYNC">>) -> 'SYNC';
decode_type(<<"NODE">>) -> 'NODE';
decode_type(<<"POOL">>) -> 'POOL';
decode_type(<<"OKAY">>) -> 'OKAY';
decode_type(<<"MINE">>) -> 'MINE';
decode_type(<<"LAST">>) -> 'LAST';
decode_type(<<"DONE">>) -> 'DONE';
decode_type(<<"WAIT">>) -> 'WAIT';
decode_type(<<"STAT">>) -> 'STAT';
decode_type(<<"INFO">>) -> 'INFO';
decode_type(<<"NACK">>) -> 'NACK';
decode_type(Type) -> Type.


decrypt_payload(_, 0, <<>>) ->
	<<>>;
decrypt_payload(Key, Size, Bin) when byte_size(Bin) == Size ->
	Value = xxtea:decode(Key, Bin),
	Pad = binary:last(Value),
	PayloadSize = byte_size(Value) - Pad,
	<<Payload:PayloadSize/binary, _:Pad/binary>> = Value,
	Payload.


decode_payload(_, <<>>) ->
	#{};
decode_payload('NODE', <<Flags:1/binary, Address/binary>>) ->
	#{flags => decode_flags(Flags), address => Address};
decode_payload('POOL', Coinbase) ->
	#{template => Coinbase};
decode_payload(Type, <<Height:32/little, Bits:32/little, Nonce:32/little, Nonce2:32/little>>)
		when Type == 'LAST'; Type == 'DONE' ->
	#{height => Height, bits => Bits, nonce => Nonce, nonce2 => Nonce2};
decode_payload('INFO', <<Flags, Status, Height:32/little, Nonce:32/little, Nonce2:32/little,
		Hash:256, HashTime:32/little, HashRate:64/float>>) ->
	Flags0 = decode_flags(Flags),
	#{flags => Flags0, status => Status, height => Height, nonce => Nonce, nonce2 => Nonce2,
		hash => Hash, hashtime => HashTime, hashrate => HashRate};
decode_payload('NACK', <<Type:4/binary, Sync:32, Code:8/binary>>) ->
	#{type => decode_type(Type), sync => Sync, code => Code};
decode_payload(_, Bin) ->
	#{payload => Bin}.


decode_flags(Flags) ->
	decode_flags(Flags, ?SBT_MINER_FLAGS, []).

decode_flags(<<1:1, F/bits>>, [H|T], Acc) ->
	decode_flags(F, T, [H|Acc]);
decode_flags(<<0:1, F/bits>>, [_|T], Acc) ->
	decode_flags(F, T, Acc);
decode_flags(<<>>, [], Acc) ->
	lists:reverse(Acc).


encode(Key, #sbt_message{version = Version, nonce = Number, type = Type, payload = Payload}) ->
	Type0 = atom_to_binary(Type, utf8),
	Payload0 = encode_payload(Type, Payload),
	Encrypted = encrypt_payload(Key, Payload0),
	Size = byte_size(Encrypted),
	<<?SBT_MAGIC:32, Version/binary, Number:32/little, Type0:4/binary, Size:32/little, Encrypted/binary>>.


encode_payload('NODE', #{flags := Flags, address := Address}) ->
	Byte = encode_flags(Flags),
	<<Byte, Address/binary>>;
encode_payload('POOL', Map = #{template := Coinbase}) ->
	Suffix = maps:get(suffix, Map, <<>>),
	<<Coinbase/binary, Suffix/binary>>;
encode_payload('MINE', Map = #{height := Height, header := Header}) ->
	Header0 = btc_codec:encode(Header),
	Bin = <<Height:32/little, Header0:80/binary>>,
	case maps:get(merkle_path, Map, []) of
	[] ->
		Bin;
	Path ->
		Path = [<<X:256/little>> || X <- Path],
		Length = length(Path),
		Path0 = list_to_binary([<<X:256/little>> || X <- Path]),
		<<Bin/binary, Length, Path0/binary>>
	end;
encode_payload('NACK', #{type := Type, sync := Sync, code := Code}) ->
	Type0 = atom_to_binary(Type, utf8),
	<<Type0:4/binary, Sync:32, Code/binary>>;
encode_payload(_, _) ->
	<<>>.


encode_flags(List) ->
	encode_flags(List, ?SBT_MINER_FLAGS, 0).

encode_flags([H|T], [H|T0], Acc) ->
	encode_flags(T, T0, Acc bsl 1 bor 1);
encode_flags(F, [_|T], Acc) ->
	encode_flags(F, T, Acc bsl 1);
encode_flags([], [], Acc) ->
	Acc.


encrypt_payload(_Key, <<>>) ->
	<<>>;
encrypt_payload(Key, Bin) ->
	PayloadSize = byte_size(Bin),
	N = case byte_size(Bin) of
		X when X < 4 -> 8;
		_ -> 4
		end,
	Pad = N - (PayloadSize rem 4),
	Padding = binary:copy(<<Pad>>, Pad),
	xxtea:encode(Key, <<Bin/binary, Padding/binary>>).


coinbase(Address) ->
	coinbase(0, 1250000000, Address, <<>>).

coinbase(Height, Value, Address) ->
	coinbase(Height, Value, Address, <<>>).

coinbase(Height, Value, Address, Suffix) ->
	SigScript = coinbase_script(Height, 0, Address, Suffix),
	Input = #btc_tx_in{outpoint_ref = 0, outpoint_index = 16#FFFFFFFF, sig_script = SigScript, sequence = 0},
	PkScript = btc_script:pk_script(Address),
 	Output = #btc_tx_out{value = Value, pk_script = PkScript},
	#btc_tx{version = 1, tx_in = [Input], tx_out = [Output], lock_time = 0}.


coinbase_script(Height, Nonce2, Address, Suffix) ->
	Hash = btc_crypto:hash160(Address),
	Tag = <<?COINBASE_ID/binary, Suffix/binary>>,
	[<<Height:24/little>>, <<Nonce2:24/little>>, op_2drop, Hash, op_drop, op_return, Tag].
