% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(hkdf).

% HMAC-based Extract-and-Expand Key Derivation Function (HKDF)
%@ref https://tools.ietf.org/html/rfc5869

-compile(export_all).

-define(HASHLENGTHS, #{
	md5 => 16,
	sha => 20,
	sha224 => 28,
	sha256 => 32,
	sha384 => 28,
	sha512 => 64
}).


derive_key(IKM) ->
	derive_key(IKM, 32).

derive_key(IKM, Size) ->
	derive_key(sha256, Size, IKM, <<>>, <<>>).

derive_key(Algorithm, L, IKM, <<>>, Info) ->
	HashSize = hash_bitsize(Algorithm),
	Salt = <<0:HashSize>>,
	derive_key(Algorithm, L, IKM, Salt, Info);
derive_key(Algorithm, L, IKM, Salt, Info) ->
	HashLength = hash_length(Algorithm),
	true = L =< 255 * HashLength,
	PRK = extract_key(Algorithm, IKM, Salt),
	N = ceiling(L / HashLength),
	<<OKM:L/binary, _/binary>> = expand_key(Algorithm, N, PRK, Info),
	OKM.


extract_key(Algorithm, IKM, Salt) ->
	crypto:hmac(Algorithm, Salt, IKM).


expand_key(Algorithm, N, PRK, Info) ->
	expand_key(Algorithm, 1, N, PRK, Info, [<<>>]).

expand_key(Algorithm, Count, Max, PRK, Info, Acc = [H|_]) when Count =< Max ->
	H0 = crypto:hmac(Algorithm, PRK, [H, Info, Count rem 256]),
	expand_key(Algorithm, Count + 1, Max, PRK, Info, [H0|Acc]);
expand_key(_, _, _, _, _, Acc) ->
	list_to_binary(lists:reverse(Acc)).


ceiling(X) ->
    ceiling(X, trunc(X)).

ceiling(X, XT) when X > XT ->
	XT + 1;
ceiling(_, XT) ->
	XT.


hash_length(K) ->
	maps:get(K, ?HASHLENGTHS).


hash_bitsize(K) ->
	hash_length(K) bsl 3.


-ifdef(TEST).

test() ->
	IKM = <<"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b">>,
	Salt = <<"000102030405060708090a0b0c">>,
	Info = <<"f0f1f2f3f4f5f6f7f8f9">>,
	OKM = <<"3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865">>,
	Key = derive_key(sha256, 42, hex:decode(IKM), hex:decode(Salt), hex:decode(Info)),
	OKM = hex:encode(Key).

-endif.
