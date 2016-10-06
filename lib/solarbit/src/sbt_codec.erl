% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(sbt_codec).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin_script.hrl").

-export([encode/2, decode/2, coinbase/1]).


decode(Key, <<?SBT_MAGIC:32, Version:4/binary, Number:32/little, Type:4/binary, Size:32/little, Bin/binary>>) ->
	Payload = decode_payload(Key, Size, Bin),
	#sbt_message{magic = ?SBT_MAGIC, version = Version, nonce = Number, type = Type, payload = Payload}.


decode_payload(_, 0, <<>>) ->
	<<>>;
decode_payload(Key, Size, Bin) when byte_size(Bin) == Size ->
	Value = xxtea:decode(Key, Bin),
	Pad = binary:last(Value),
	PayloadSize = byte_size(Value) - Pad,
	<<Payload:PayloadSize/binary, _:Pad/binary>> = Value,
	Payload.


encode(Key, #sbt_message{version = Version, nonce = Number, type = Type, payload = Payload}) ->
	Encrypted = encode_payload(Key, Payload),
	Size = byte_size(Encrypted),
	<<?SBT_MAGIC:32, Version/binary, Number:32/little, Type/binary, Size:32/little, Encrypted/binary>>.


encode_payload(_Key, <<>>) ->
	<<>>;
encode_payload(Key, Bin) when size(Bin) >= 4 ->
	PayloadSize = byte_size(Bin),
	Pad = 4 - (PayloadSize rem 4),
	<<Padding:Pad/binary, _/binary>> = <<Pad, Pad, Pad, Pad>>,
	xxtea:encode(Key, <<Bin/binary, Padding/binary>>);
encode_payload(Key, Bin) ->
	PayloadSize = 4 + byte_size(Bin),
	Pad = 8 - (PayloadSize rem 4),
	<<Padding:Pad/binary, _/binary>> = <<Pad, Pad, Pad, Pad, Pad, Pad, Pad>>,
	xxtea:encode(Key, <<Bin/binary, Padding/binary>>).


coinbase(Address) when is_binary(Address) ->
	Hash = crypto:hash(sha, Address),
	<<3, 0:24, ?OP_DROP, (byte_size(?COINBASE_ID)), ?COINBASE_ID/binary, ?OP_DROP,
		(byte_size(Hash)), Hash/binary, ?OP_DROP, ?OP_RETURN, 0:64>>.
