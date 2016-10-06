% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(btc_script).

-include("bitcoin.hrl").
-include("bitcoin_script.hrl").

-compile(export_all).


encode(Script) when is_binary(Script) ->
	Script;
encode(Script) ->
	encode(Script, <<>>).

encode([Opcode|T], Acc) when is_atom(Opcode) ->
	{N, Opcode} = lists:keyfind(Opcode, 2, ?OPCODES),
	encode(T, <<Acc/binary, N>>);
encode([Bin|T], Acc) when is_binary(Bin) ->
	Size = byte_size(Bin),
	encode(T, <<Acc/binary, Size, Bin/binary>>);
encode([], Acc) ->
	Acc.


decode(Script) ->
	case decode(Script, []) of
	{ok, L} ->
		L;
	Error = {error, _, _} ->
		?TTY(Error),
		Script
	end.

decode(<<X, Bin/binary>>, Acc) when X > 0 andalso X < 76 ->
	case Bin of
	<<Data:X/binary, Bin0/binary>> ->
 		decode(Bin0, [Data|Acc]);
	_ ->
		{error, lists:reverse(Acc), Bin}
	end;
decode(<<?OP_RETURN, Bin/binary>>, Acc) ->
	{ok, lists:reverse([Bin, op_return|Acc])};
decode(<<X, Bin/binary>>, Acc) ->
	X0 = proplists:get_value(X, ?OPCODES, {op, X}),
	decode(Bin, [X0|Acc]);
decode(<<>>, Acc) ->
	{ok, lists:reverse(Acc)}.


exec(Hash, Script) ->
	exec(Hash, Script, []).

exec(Hash, [H|T], Acc) when is_binary(H) ->
	exec(Hash, T, [H|Acc]);
exec(Hash, [op_drop|T], [_|Acc]) ->
	exec(Hash, T, Acc);
exec(Hash, [op_dup|T], [H|Acc]) ->
	exec(Hash, T, [H, H|Acc]);
exec(Hash, [op_hash160|T], [H|Acc]) ->
	H0 = bitcoin_crypto:hash160(H),
	exec(Hash, T, [H0|Acc]);
exec(Hash, [op_equalverify|T], [H, H|Acc]) ->
	exec(Hash, T, Acc);
exec(_, [op_equalverify|_], _) ->
	{error, op_equalverify};
exec(Hash, [op_checksig|T], [Pub, Sig|Acc]) ->
	Size = byte_size(Sig) - 1,
	<<Sig0:Size/binary, ?SIG_HASH_ALL>> = Sig,
	?TTY({hash, Hash, sig, Sig0, pk, Pub}),
	case bitcoin_crypto:verify({digest, Hash}, Sig0, Pub) of
	true ->
		exec(Hash, T, Acc);
	false ->
		{error, op_checksig}
	end;
exec(_, [], []) ->
	ok;
exec(_, Script, Acc) ->
	?TTY({Script, Acc}),
	{error, Script}.


sig_script(Signature, PublicKey) ->
	encode([<<Signature/binary, ?SIG_HASH_ALL>>, PublicKey]).


pk_script(Address) ->
	Hash = bitcoin_crypto:decode_address(Address),
	encode([op_dup, op_hash160, Hash, op_equalverify, op_checksig]).


%%% Tests

-define(SCRIPT_IN_1, <<4,255,255,0,29,1,4,69,84,104,101,32,84,105,109,101,115,32,48,
                51,47,74,97,110,47,50,48,48,57,32,67,104,97,110,99,101,108,108,
                111,114,32,111,110,32,98,114,105,110,107,32,111,102,32,115,101,
                99,111,110,100,32,98,97,105,108,111,117,116,32,102,111,114,32,
                98,97,110,107,115>>).
-define(SCRIPT_OUT_1, <<65,4,103,138,253,176,254,85,72,39,25,103,241,166,113,48,183,16,
                92,214,168,40,224,57,9,166,121,98,224,234,31,97,222,182,73,246,
                188,63,76,239,56,196,243,85,4,229,30,193,18,222,92,56,77,247,
                186,11,141,87,138,76,112,43,107,241,29,95,172>>).

-define(SCRIPT_IN_2, <<4,255,255,0,29,1,4>>).
-define(SCRIPT_OUT_2, <<65,4,150,181,56,232,83,81,156,114,106,44,145,230,30,193,22,0,
                174,19,144,129,58,98,124,102,251,139,231,148,123,230,60,82,218,
                117,137,55,149,21,212,224,166,4,248,20,23,129,230,34,148,114,
                17,102,191,98,30,115,168,44,191,35,66,200,88,238,172>>).
-define(SIG_SCRIPT,
<<72,48,69,2,33,0,243,88,30,25,114,174,138,199,199,54,122,122,37,59,193,19,82,
  35,173,185,164,104,187,58,89,35,63,69,188,87,131,128,2,32,89,175,1,202,23,
  208,14,65,131,122,29,88,233,122,163,27,174,88,78,222,194,141,53,189,150,146,
  54,144,145,59,174,154,1,65,4,156,2,191,201,126,242,54,206,109,143,229,217,64,
  19,199,33,233,21,152,42,205,43,18,182,93,155,125,89,226,10,132,32,5,248,252,
  78,2,83,46,135,61,55,185,111,9,214,212,81,26,218,143,20,4,47,70,97,74,76,112,
  192,241,75,239,245>>).

test() ->
	{decode(?SCRIPT_OUT_1), decode(?SCRIPT_IN_2)}.

test1() ->
	decode(?SIG_SCRIPT).
test2() ->
	decode(hex:decode(<<"76a914097072524438d003d23a2f23edb65aae1bb3e46988ac">>)).

dtest() ->
	decode(<<112,115,106,4,92,152,4,26,4,214,143,17,19,82,44,250,
		190,109,109,23,91,89,108,29,223,69,160,187,77,217,
		118,101,73,213,242,148,16,250,64,60,103,96,193,226,
		109,25,80,197,8,108,135,1,0,0,0,0,0,0,0,66,84,67,32,
		71,117,105,108,100,32,51,172,30,238,237,136>>).

dtest2() -> decode(<<7,69,108,105,103,105,117,115,2,82,25,76,80,0,
                               73,32,98,101,108,105,101,118,101,32,116,104,
                               101,115,101,32,97,110,100,32,97,108,108,32,116,
                               104,101,32,116,114,117,116,104,115,32,119,104,
                               105,99,104,32,116,104,101,32,72,111,108,121,32,
                               67,97,116,104,111,108,105,99,32,67,104,117,114,
                               99,104,32,116,101,97,99,104,101,115,44,32,46,
                               46,46,0>>).

dtest3() -> decode(<<7,69,108,105,103,105,117,115,3,223,165,0,76,84,
                               0,79,32,72,101,97,114,116,32,111,102,32,74,101,
                               115,117,115,44,32,98,117,114,110,105,110,103,
                               32,119,105,116,104,32,108,111,118,101,32,102,
                               111,114,32,117,115,44,32,105,110,102,108,97,
                               109,101,32,111,117,114,32,104,101,97,114,116,
                               115,32,119,105,116,104,32,108,111,118,101,32,
                               102,111,114,32,84,104,101,101,46,1>>).


test5() ->
	A = bitcoin_crypto:decode_address(<<"16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM">>),
	Script = [op_dup, op_hash160, A, op_equalverify,op_checksig],
  	PK = <<4,80,134,58,214,74,135,174,138,47,232,60,26,241,168,64,60,181,63,83,228,134,
			216,81,29,173,138,4,136,126,91,35,82,44,212,112,36,52,83,162,153,250,158,119,
			35,119,22,16,58,188,17,161,223,56,133,94,214,242,238,24,126,156,88,43,166>>,
  	exec(<<"hash">>, Script, [PK, <<"sig">>]).

test4() ->
	Script = [op_dup, op_hash160,
	<<1,9,102,119,96,6,149,61,85,103,67,158,94,57,248,106,13,39,59,238>>,
  	op_equalverify,op_checksig],
  	encode(Script).
