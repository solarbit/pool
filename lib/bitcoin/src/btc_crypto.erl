% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(btc_crypto).

-include("bitcoin.hrl").

-compile(export_all).

-define(ELLIPTIC_CURVE, secp256k1).


hash256(Bin) when is_binary(Bin) ->
	Bin0 = crypto:hash(sha256, Bin),
	crypto:hash(sha256, Bin0).


hash160(PublicKey) ->
	Hash = crypto:hash(sha256, PublicKey),
	crypto:hash(ripemd160, Hash).


checksum(Bin) ->
	<<Checksum:32, _/binary>> = hash256(Bin),
	Checksum.


nonce() ->
	<<N:64>> = crypto:strong_rand_bytes(8),
	N.


% TODO: Support for Compressed Public Keys/WIF format
compress_public_key(<<4, PK:32/binary, _:31/binary, SuffixByte>>) ->
	PrefixByte = compressed_public_key_prefix_byte(SuffixByte),
	<<PrefixByte, PK:32/binary>>.


compressed_public_key_prefix_byte(Byte) when Byte rem 2 =:= 0 ->
	2;
compressed_public_key_prefix_byte(_) ->
	3.


generate_address() ->
	{Public, Private} = generate_keypair(),
	Address = encode_address(Public),
	Wif = encode_wif(Private),
	#btc_address{id = Address, public_key = Public, wif = Wif, private_key = Private}.

generate_address(Wif) ->
	Private = decode_wif(Wif),
	{Public, Private} = generate_keypair(Private),
	Address = encode_address(Public),
	#btc_address{id = Address, public_key = Public, wif = Wif, private_key = Private}.


encode_address(PublicKey) when byte_size(PublicKey) =:= 65 ->
	Hash160 = hash160(PublicKey),
	KeyHash = <<?VERSION_PUBKEY_HASH, Hash160/binary>>,
	Checksum = checksum(KeyHash),
	base58:encode(<<KeyHash/binary, Checksum:32>>).


decode_address(Address) when is_binary(Address), byte_size(Address) =< 34 ->
	<<KeyHash:21/binary, Checksum:32>> = base58:decode(Address),
	Checksum = checksum(KeyHash),
	<<?VERSION_PUBKEY_HASH, Hash160/binary>> = KeyHash,
	Hash160.


verify_address(Address) when is_binary(Address), byte_size(Address) =< 34 ->
	<<KeyHash:21/binary, Checksum:32>> = base58:decode(Address),
	Checksum =:= checksum(KeyHash);
verify_address(_) ->
	false.

verify_address(Address, PublicKey) when is_binary(Address), is_binary(PublicKey) ->
	Address =:= encode_address(PublicKey).


encode_wif(PrivateKey) when byte_size(PrivateKey) =:= 32 ->
	Bin = <<?VERSION_PRIVATE_KEY, PrivateKey/binary>>,
	Checksum = checksum(Bin),
	base58:encode(<<Bin/binary, Checksum:32>>).


decode_wif(Wif) ->
	<<Verify:33/binary, Checksum:32>> = base58:decode(Wif),
	Checksum = checksum(Verify),
	<<?VERSION_PRIVATE_KEY, PrivateKey:32/binary>> = Verify,
	PrivateKey.


generate_keypair() ->
	crypto:generate_key(ecdh, ?ELLIPTIC_CURVE).

generate_keypair(PrivateKey) ->
	crypto:generate_key(ecdh, ?ELLIPTIC_CURVE, PrivateKey).


encrypt(ClearText, YourPublicKey, MyPrivateKey) ->
	<<IV:16/binary, SecretKey:16/binary>> = crypto:compute_key(ecdh, YourPublicKey, MyPrivateKey, ?ELLIPTIC_CURVE),
	crypto:block_encrypt(aes_cfb128, SecretKey, IV, ClearText).


decrypt(ClearText, YourPublicKey, MyPrivateKey) ->
	<<IV:16/binary, SecretKey:16/binary>> = crypto:compute_key(ecdh, YourPublicKey, MyPrivateKey, ?ELLIPTIC_CURVE),
	crypto:block_decrypt(aes_cfb128, SecretKey, IV, ClearText).


sign(ClearText, PrivateKey) ->
	crypto:sign(ecdsa, sha256, ClearText, [PrivateKey, ?ELLIPTIC_CURVE]).


verify(ClearText, Signature, PublicKey) ->
	crypto:verify(ecdsa, sha256, ClearText, Signature, [PublicKey, ?ELLIPTIC_CURVE]).


merkle_root(Values) when is_list(Values) ->
	Hashes = [hash256(Value) || Value <- Values],
	hash_root(Hashes, [], []).

merkle_root(CoinbaseValue, MerklePath) ->
	StartingHash = hash256(CoinbaseValue),
	merkle_root_from_path(StartingHash, MerklePath).

merkle_root_from_path(Hash, [H|T]) ->
	NextHash = hash256(<<Hash/binary, H/binary>>),
	merkle_root_from_path(NextHash, T);
merkle_root_from_path(MerkleRoot, []) ->
	MerkleRoot.


hash_root(Hashes = [_Coinbase, H|_]) ->
	hash_root(Hashes, [], [H]);
hash_root(Hashes) when is_list(Hashes) ->
	hash_root(Hashes, [], []).

hash_root([Hash0], [], []) ->
	{Hash0, []};
hash_root([Hash0], Acc, Path) ->
	hash_root([Hash0, Hash0], Acc, Path);
hash_root([Hash0, Hash1|T], Acc, Path) ->
	Hash2 = hash256(<<Hash0/binary, Hash1/binary>>),
	hash_root(T, [Hash2|Acc], Path);
hash_root([], Acc, Path) when length(Acc) > 1 ->
	Acc0 = [_, PathElement|_] = lists:reverse(Acc),
	% ?TTY([hex:encode(X) || X <- Acc0]),
	hash_root(Acc0, [], [PathElement|Path]);
hash_root([], [MerkleRoot], CoinbasePath) ->
	{MerkleRoot, lists:reverse(CoinbasePath)}.


-ifdef(TEST).

test() ->
	M = <<"test">>,
	{PK, SK} = generate_keypair(),
	SIG = sign(M, SK),
	true = verify(M, SIG, PK),
	<<48, PKL, 2, RL, Bin/binary>> = SIG,
	<<R:RL/binary, 2, SL, SB/binary>> = Bin,
	BitSize = SL bsl 3,
	<<S:BitSize>> = SB,
	Max = 16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0,
	N = 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
	?TTY([Max, N]),
	case S > Max of
	true ->
		S0 = N - S;
	false ->
		S0 = N + S
	end,
	BitSize0 = BitSize + 8,
	case <<S0:BitSize0>> of
	<<0, SB1/binary>> ->
		SIG0 = <<48, PKL, 2, RL, R/binary, 2, SL, SB1/binary>>;
	SB1 ->
		SIG0 = <<48, PKL, 2, RL, R/binary, 2, SL, SB1/binary>>
	end,
	?TTY([SIG, SIG0]),
	verify(M, SIG0, PK).

-endif.
