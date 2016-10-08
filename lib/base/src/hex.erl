% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(hex).

-export([encode/1, decode/1, value/1, dump/1]).


encode(0) ->
	<<$0, $0>>;
encode(Num) when is_integer(Num) ->
	encode(encode_number(Num, []));
encode(Bin) when is_binary(Bin) ->
	encode(Bin, <<>>).
encode(<<A:4, B:4, Rest/binary>>, Acc) ->
	U = encode_digit(A),
	L = encode_digit(B),
	encode(Rest, <<Acc/binary, U, L>>);
% TODO: Consider...
%encode(<<>>, <<$0, Acc/binary>>) ->
%	Acc;
encode(<<>>, Acc) ->
	Acc.


encode_number(Number, Acc) when Number > 0 ->
	Value = Number band 255,
	encode_number(Number bsr 8, [Value|Acc]);
encode_number(0, Acc) ->
	list_to_binary(Acc).


encode_digit(D) when D >= 0, D =< 9 ->
	$0 + D;
encode_digit(D) when D >= 10, D =< 15 ->
	$a + D - 10.


decode(Bin) when is_binary(Bin) ->
	case byte_size(Bin) rem 2 of
	0 ->
		decode(Bin, <<>>);
	1 ->
		decode(<<$0, Bin/binary>>, <<>>)
	end.

decode(<<A, B, Rest/binary>>, Acc) ->
	I = decode_digit(A) bsl 4 bor decode_digit(B),
	decode(Rest, <<Acc/binary, I>>);
decode(<<>>, Acc) ->
	Acc.


decode_digit(H) when H >= $0, H =< $9 ->
	H - $0;
decode_digit(H) when H >= $a, H =< $f ->
	H - $a + 10;
decode_digit(H) when H >= $A, H =< $F ->
	H - $A + 10.


value(Bin) when is_binary(Bin) ->
	erlang:binary_to_integer(Bin, 16).


dump(Bin) ->
	dump(Bin, 0, <<>>).
dump(<<X, Bin/binary>>, N, Acc) when N =:= 7 ->
	Hex = integer_to_binary(X, 16),
	dump(Bin, N + 1, <<Acc/binary, Hex/binary, "   ">>);
dump(<<X, Bin/binary>>, N, Acc) when N < 16 ->
	Hex = integer_to_binary(X, 16),
	dump(Bin, N + 1, <<Acc/binary, Hex/binary, " ">>);
dump(<<X, Bin/binary>>, _, Acc) ->
	Hex = integer_to_binary(X, 16),
	dump(Bin, 0, <<Acc/binary, Hex/binary, $\n>>);
dump(<<>>, _, Acc) ->
	Acc.
