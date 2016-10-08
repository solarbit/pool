% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(base58).

-export([encode/1, encode/2, decode/1, decode/2, charset/1]).

-define(BITCOIN_BASE58_CHARSET, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).
-define(FLICKR_BASE58_CHARSET, <<"123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ">>).


encode(Bin) ->
	encode(Bin, bitcoin).

encode(Bin, Charset) ->
	Charset0 = charset(Charset),
	Prefix = encode_prefix(Bin, <<>>),
	Size = bit_size(Bin),
	<<N:Size>> = Bin,
	Base58 = encode_integer(N, Charset0, <<>>),
	<<Prefix/binary, Base58/binary>>.


encode_prefix(<<0, Bin/binary>>, Acc) ->
	encode_prefix(Bin, <<Acc/binary, $1>>);
encode_prefix(_, Acc) ->
	Acc.


encode_integer(N, Charset, Acc) when N > 0 ->
	Byte = binary:at(Charset, N rem 58),
	encode_integer(N div 58, Charset, <<Byte, Acc/binary>>);
encode_integer(0, _, Acc) ->
	Acc.


decode(Base58) ->
	decode(Base58, bitcoin).

decode(Base58, Charset) ->
	Charset0 = charset(Charset),
	Prefix = decode_prefix(Base58, <<>>),
	Bin = decode_chars(Base58, Charset0, 0),
	<<Prefix/binary, Bin/binary>>.


decode_prefix(<<$1, Bin/binary>>, Acc) ->
	decode_prefix(Bin, <<Acc/binary, 0>>);
decode_prefix(_, Acc) ->
	Acc.


decode_chars(<<X, Bin/binary>>, Charset, N) ->
	M = decode_char(X, Charset),
	decode_chars(Bin, Charset, N * 58 + M);
decode_chars(<<>>, _, N) ->
	binary:encode_unsigned(N).


decode_char(X, Charset) ->
	decode_char(X, 0, Charset).

decode_char(X, N, <<X, _/binary>>) ->
	N;
decode_char(X, N, <<_, Bin/binary>>) ->
	decode_char(X, N + 1, Bin).


charset(bitcoin) ->
	?BITCOIN_BASE58_CHARSET;
charset(flickr) ->
	?FLICKR_BASE58_CHARSET.
