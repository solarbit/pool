% Copyright 2016 SolarBit <steve@solarbit.cc>
% See LICENSE
-module(xxtea).

-export([encode/2, encode/3, decode/2, decode/3]).
-export([test/0, test/1, test/3]).

-define(DELTA, 16#9E3779B9).
-define(INT32_MASK, 16#FFFFFFFF).

-define(TEST_KEY, <<"SolarBitSolarBit">>).

-compile(export_all).

% Ref: http://en.wikipedia.org/wiki/XXTEA

% Ref: http://read.pudn.com/downloads187/sourcecode/windows/other/877059/xxtea.cpp__.htm
% "Zero padding is adequate for text messages (which cannot contain the character NUL = 0x00), but in a
% binary context it is not a reversible padding scheme. If the data can include the byte 0x00, it is
% recommended to use the "Pad80" scheme in which you always add a single '1' bit to your data (even if the
% data is already a multiple of the block length) then pad with '0' bits to a multiple of the block length.
% This padding can always be reversed without confusion".


encode(Key, Plaintext) ->
	encode(Key, Plaintext, little).

encode(Key, Plaintext, Endian) when byte_size(Plaintext) rem 4 == 0 ->
	{V, K, Rounds} = get_args(Key, Endian, Plaintext),
	V0 = encode0(V, K, Rounds, lists:last(V), ?DELTA),
	int32list_to_binary(Endian, V0);
encode(Key, Plaintext, Endian) ->
	% TODO: Zero-padding may not be the best scheme...
	Pad = (4 - byte_size(Plaintext) rem 4) bsl 3,
	encode(Key, <<Plaintext/binary, 0:Pad>>, Endian).


encode0(V, K, Rounds, Z, Sum) when Rounds > 0 ->
	E = (Sum bsr 2) band 3,
	V0 = [Z0|_] = encode1(V, K, Z, Sum, E, 0, []),
	encode0(lists:reverse(V0), K, Rounds - 1, Z0, Sum + ?DELTA);
encode0(V, _K, 0, _Z, _Sum) ->
	V.


encode1([H|T = [Y|_]], K, Z, Sum, E, P, Acc) ->
	Z0 = int32(H + mx(K, Z, Y, Sum, E, P)),
	encode1(T, K, Z0, Sum, E, P + 1, [Z0|Acc]);
encode1([H], K, Z, Sum, E, P, Acc)  ->
	[int32(H + mx(K, Z, lists:last(Acc), Sum, E, P)) | Acc].


decode(Key, Ciphertext) ->
	decode(Key, Ciphertext, little).

decode(_Key, Ciphertext, _Endian) when byte_size(Ciphertext) < 8 ->
	Ciphertext;
decode(Key, Ciphertext, Endian) ->
	{V, K, Rounds} = get_args(Key, Endian, Ciphertext),
	V0 = decode0(V, K, Rounds, hd(V), Rounds * ?DELTA),
	int32list_to_binary(Endian, V0).


decode0(V, K, Rounds, Y, Sum) when Rounds > 0 ->
	E = (Sum bsr 2) band 3,
	V0 = [Y0|_] = decode1(lists:reverse(V), K, Y, Sum, E, length(V) - 1, []),
	decode0(V0, K, Rounds - 1, Y0, Sum - ?DELTA);
decode0(V, _K, 0, _Y, 0) ->
	V.


decode1([H|T = [Z|_]], K, Y, Sum, E, P, Acc) ->
	Y0 = int32(H - mx(K, Z, Y, Sum, E, P)),
	decode1(T, K, Y0, Sum, E, P - 1, [Y0|Acc]);
decode1([H], K, Y, Sum, E, P = 0, Acc)  ->
	[int32(H - mx(K, lists:last(Acc), Y, Sum, E, P))|Acc].


mx(K, Z, Y, Sum, E, P) ->
	X1 = (Z bsr 5) bxor (Y bsl 2),
	X2 = (Y bsr 3) bxor (Z bsl 4),
	X3 = element((P band 3 bxor E) + 1, K),
	(X1 + X2) bxor ((Sum bxor Y) + (X3 bxor Z)).


get_args(Key, Endian, Bin) ->
	K = list_to_tuple(int32list_from_binary(Endian, Key)),
	V = int32list_from_binary(Endian, Bin),
	Rounds = 6 + 52 div length(V),
	{V, K, Rounds}.


int32list_from_binary(Endian, Bin) ->
	int32list_from_binary(Endian, Bin, []).

int32list_from_binary(little, <<X:32/little, Bin/binary>>, Acc) ->
	int32list_from_binary(little, Bin, [X|Acc]);
int32list_from_binary(big, <<X:32, Bin/binary>>, Acc) ->
 	int32list_from_binary(big, Bin, [X|Acc]);
int32list_from_binary(_, <<>>, Acc) ->
	lists:reverse(Acc).


int32list_to_binary(little, List) ->
	<< <<X:32/little>> || X <- List>>;
int32list_to_binary(big, List) ->
	<< <<X:32>> || X <- List>>.


int32(X) ->
	X band ?INT32_MASK.


test(Key, Text) ->
	hex:encode(encode(Key, Text)).

test() ->
	Vectors = [
		{<<"00000000000000000000000000000000">>,<<"0000000000000000">>, <<"053704ab575d8c80">>, <<"ab043705808c5d57">>},
		{<<"0102040810204080fffefcf8f0e0c080">>,<<"0000000000000000">>, <<"8b86e32648a0669d">>, <<"d1e78be2c746728a">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>,<<"ffffffffffffffff">>, <<"6be3702a2d1f9499">>, <<"67ed0ea8e8973fc5">>},
		{<<"0102040810204080fffefcf8f0e0c080">>,<<"fffefcf8f0e0c080">>, <<"875a9ee9a1840eb3">>, <<"8c3707c01c7fccc4">>},
		{<<"ffffffffffffffffffffffffffffffff">>,<<"157c13a850ba5e57306d7791">>, <<"944b9541f00ef7025c5990da">>, <<"b2601cefb078b772abccba6a">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>,<<"157c13a850ba5e57306d7791">>, <<"5a4090f8e793680fd1981120">>, <<"579016d143ed6247ac6710dd">>},
		{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<"0102040810204080fffefcf8f0e0c080">>, <<"e3c9b52bab3438b1f9fc706aabb9b7e5">>, <<"c0a19f06ebb0d63925aa27f74cc6b2d0">>},
	 	{<<"9e3779b99b9773e9b979379e6b695156">>, <<"0102040810204080fffefcf8f0e0c080">>, <<"b4c1460127de2198ff3232a1bc05a0ad">>, <<"01b815fd2e4894d13555da434c9d868a">>},
	 	{<<"0102040810204080fffefcf8f0e0c080">>, <<"157c13a850ba5e57306d77916fa2c37be1949616">>, <<"c6574667e9950de0c3f44520666fc725caaab1fe">>, <<"51f0ffeb46012a245e0c6c4fa097db27caec698d">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>, <<"690342f45054a708c475c91db77761bc01b815fd2e4894d1">>, <<"eda850f7ac4b1dbb00c349ed020eacb7fee5f4b7dce0c418">>, <<"759e5b212ee58be734d610248e1daa1c9d0647d428b4f95a">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>, <<"3555da434c9d868a1431e73e73372fc0688e09ce11d00b6fd936a764">>, <<"681faae8568134e64fed86e154198a556ae6737d9ddf5ef778ba2ec4">>, <<"8e63ae7d8a119566990eb756f16abf94ff87359803ca12fbaa03fdfb">>},
		{<<"0102040810204080fffefcf8f0e0c080">>, <<"db9af3c96e36a30c643c6e97f4d75b7a4b51a40e9d8759e581e3c40b341b4436">>, <<"2c34cc18fb1b22ddecdd57accf20173eb824217b74637d8eead7604a2e11b718">>, <<"5ef1b6e010a2227ba337374b59beffc5263503054745fb513000641e2c7dd107">>},
		% messages zero-padded to 64-bit
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4100000000000000">>, <<"e3e0d664e7f07753">>, <<"014e7a34874eeb29">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142000000000000">>, <<"4277e271b8f6e4b3">>, <<"e9d39f636e9ed090">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142430000000000">>, <<"89550b483d47e902">>, <<"d20ec51c06feaf0e">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142434400000000">>, <<"77543ad3c5399858">>, <<"b1551d6ffcd4b61b">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142434445000000">>, <<"ce9516ea9a08815c">>, <<"0ff91e518b9837e3">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142434445460000">>, <<"6f465615ce2ef27b">>, <<"7003fc98b6788a77">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142434445464700">>, <<"e56e193c212ec827">>, <<"93951ad360650022">>},
		{<<"6a6f686e636b656e64616c6c6a6f686e">>,<<"4142434445464748">>, <<"2f920605c75725aa">>, <<"cdeb72b9c903ce52">>}
	],
	[test(hex:decode(Key), hex:decode(Text), hex:decode(Cipher)) || {Key, Text, _NBOCipher, Cipher} <- Vectors],
	ok.


test(Text) ->
	Cipher = encode(?TEST_KEY, Text),
	Size = byte_size(Text),
	<<Text:Size/binary, _/binary>> = decode(?TEST_KEY, Cipher),
	{ok, hex:encode(Cipher)}.


test(Key, Text, Cipher) ->
	Cipher = encode(Key, Text),
	Text = decode(Key, Cipher).
