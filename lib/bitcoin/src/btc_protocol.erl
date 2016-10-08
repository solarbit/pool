% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(btc_protocol).

-include("bitcoin.hrl").

-compile(export_all).

init() ->
	{ok, fun decode/1}.


decode(Bin) ->
	decode(Bin, <<>>).

decode(Bin, Acc) ->
	decode0(<<Acc/binary, Bin/binary>>, []).

decode0(All = <<?BITCOIN_MAGIC:32, Command:12/binary, PayloadLength:32/little,
		Checksum:32, Bin/binary>>, Acc) ->
	case Bin of
	<<Payload:PayloadLength/binary, Bin0/binary>> ->
		Checksum = btc_crypto:checksum(Payload),
		Record = decode_payload(Command, Payload),
		decode0(Bin0, [Record|Acc]);
	_ ->
		{ok, lists:reverse(Acc), fun(X) -> decode(X, All) end}
	end;
decode0(Bin, Acc) ->
	{ok, lists:reverse(Acc), fun(X) -> decode(X, Bin) end}.


decode_payload(<<"version", 0:40>>, <<Version:32/little, Services:64/little, Timestamp:64/little,
		ToAddress:26/binary, FromAddress:26/binary, Nonce:64/little, Bin/binary>>) ->
	AddrRecv = decode_netaddr(ToAddress),
	AddrFrom = decode_netaddr(FromAddress),
	{UserAgent, Bin0} = decode_string(Bin),
	case Bin0 of
	<<StartHeight:32/little, Relay>> ->
		ok;
	<<StartHeight:32/little>> ->
		Relay = 0
	end,
	#btc_version{
		id = Version,
		services = Services,
		timestamp = Timestamp,
		addr_recv = AddrRecv,
		addr_from = AddrFrom,
		nonce = Nonce,
		user_agent = UserAgent,
		start_height = StartHeight,
		relay = (Relay =/= 0)
	};
decode_payload(<<"verack", 0:48>>, <<>>) ->
	#btc_verack{};
decode_payload(<<"addr", 0:64>>, Payload) ->
	{Count, Bin} = decode_integer(Payload),
	Size = byte_size(Bin) div Count,
	NetAddrs = decode_netaddrs(Size, Bin, []),
	#btc_addr{addr_list = NetAddrs};
decode_payload(<<"inv", 0:72>>, Payload) ->
	{Count, Bin} = decode_integer(Payload),
	Vectors = decode_inventory_vectors(Count, Bin),
	#btc_inv{vectors = Vectors};
decode_payload(<<"getdata", 0:40>>, Payload) ->
	{Count, Bin} = decode_integer(Payload),
	Vectors = decode_inventory_vectors(Count, Bin),
	#btc_getdata{vectors = Vectors};
decode_payload(<<"notfound", 0:32>>, Payload) ->
	{Count, Bin} = decode_integer(Payload),
	Vectors = decode_inventory_vectors(Count, Bin),
	#btc_notfound{vectors = Vectors};
decode_payload(<<"getblocks", 0:24>>, Payload) ->
	<<Version:32/little, Bin/binary>> = Payload,
	{Count, Bin0} = decode_integer(Bin),
	{BlockIds, <<HashStop:256>>} = decode_hashlist(Count, Bin0),
	#btc_getblocks{version = Version, block_locator_hashes = BlockIds, hash_stop = HashStop};
decode_payload(<<"getheaders", 0:16>>, Payload) ->
	<<Version:32/little, Bin/binary>> = Payload,
	{Count, Bin0} = decode_integer(Bin),
	{BlockIds, <<HashStop:256>>} = decode_hashlist(Count, Bin0),
	#btc_getheaders{version = Version, block_locator_hashes = BlockIds, hash_stop = HashStop};
decode_payload(<<"tx", 0:80>>, Payload) ->
	btc_codec:decode_transaction(Payload);
decode_payload(<<"block", 0:56>>, Payload) ->
	btc_codec:decode_block(Payload);
decode_payload(<<"headers", 0:40>>, Payload) ->
	{Count, Bin} = decode_integer(Payload),
	Headers = decode_headers(Count, Bin),
	#btc_headers{headers = Headers};
decode_payload(<<"getaddr", 0:40>>, <<>>) ->
	#btc_getaddr{};
% BIP-35, BIP-37
decode_payload(<<"mempool", 0:40>>, <<>>) ->
	#btc_mempool{};
% deprecated
decode_payload(<<"checkorder", 0:16>>, _Payload) ->
	{deprecated, checkorder};
% deprecated
decode_payload(<<"submitorder", 0:8>>, _Payload) ->
	{deprecated, submitorder};
% deprecated
decode_payload(<<"reply", 0:56>>, _Payload) ->
	{deprecated, reply};
decode_payload(<<"ping", 0:64>>, <<Nonce:64/little>>) ->
	#btc_ping{nonce = Nonce};
decode_payload(<<"pong", 0:64>>, <<Nonce:64/little>>) ->
	#btc_pong{nonce = Nonce};
decode_payload(<<"alert", 0:56>>, _Payload) ->
	alert;
decode_payload(<<"reject", 0:48>>, Payload) ->
	{Message, <<CCode, Bin/binary>>} = decode_string(Payload),
	{Reason, Data} = decode_string(Bin),
	#btc_reject{
		message = Message,
		ccode = decode_ccode(CCode),
		reason = Reason,
		data = Data
	};
% BIP-37
decode_payload(<<"filterload", 0:16>>, _Payload) ->
	filterload;
% BIP-37
decode_payload(<<"filteradd", 0:24>>, _Payload) ->
	filteradd;
% BIP-37
decode_payload(<<"filterclear", 0:8>>, _Payload) ->
	filterclear;
% BIP-37
decode_payload(<<"merkleblock", 0:8>>, _Payload) ->
	merkleblock;
decode_payload(Command, _Payload) ->
	CommandName = << <<X>> || <<X>> <= Command, X =/= 0>>,
	{error, CommandName}.


decode_ccode(?REJECT_MALFORMED) -> malformed;
decode_ccode(?REJECT_INVALID) -> invalid;
decode_ccode(?REJECT_OBSOLETE) -> obsolete;
decode_ccode(?REJECT_DUPLICATE) -> duplicate;
decode_ccode(?REJECT_NONSTANDARD) -> non_standard;
decode_ccode(?REJECT_DUST) -> dust;
decode_ccode(?REJECT_INSUFFICIENTFEE) -> insufficient_fee;
decode_ccode(?REJECT_CHECKPOINT) -> checkpoint.


decode_string(Bin) ->
	{Size, Bin0} = decode_integer(Bin),
	<<S:Size/binary, Bin1/binary>> = Bin0,
	{S, Bin1}.


decode_netaddrs(Size, Bin, Acc) when byte_size(Bin) > 0 ->
	<<NetAddr:Size/binary, Bin0/binary>> = Bin,
	NetAddr0 = decode_netaddr(NetAddr),
	decode_netaddrs(Size, Bin0, [NetAddr0|Acc]);
decode_netaddrs(_, <<>>, Acc) ->
	lists:reverse(Acc).


decode_netaddr(<<Services:64/little, ?IPV4_FLAG:96, IPv4:4/binary, Port:16>>) ->
	IP = decode_ip_address(IPv4),
	#btc_netaddr{services = Services, ip = IP, port = Port};
decode_netaddr(<<Services:64/little, IPv6:16/binary, Port:16>>) ->
	#btc_netaddr{services = Services, ip = IPv6, port = Port};
decode_netaddr(<<Time:32, Services:64/little, ?IPV4_FLAG:96, IPv4:4/binary, Port:16>>) ->
	IP = decode_ip_address(IPv4),
	#btc_netaddr{time = Time, services = Services, ip = IP, port = Port};
decode_netaddr(<<Time:32, Services:64/little, IPv6:16/binary, Port:16>>) ->
	#btc_netaddr{time = Time, services = Services, ip = IPv6, port = Port}.


decode_ip_address(<<X0, X1, X2, X3>>) ->
	{X0, X1, X2, X3}.


decode_inventory_vectors(Count, Bin) ->
	decode_inventory_vectors(Count, Bin, []).

decode_inventory_vectors(Count, <<Type:32/little, Id:256, Bin/binary>>, Acc) when Count > 0 ->
	Vector = {decode_vector_type(Type), Id},
	decode_inventory_vectors(Count - 1, Bin, [Vector|Acc]);
decode_inventory_vectors(0, <<>>, Acc) ->
	lists:reverse(Acc).


decode_vector_type(?INV_TX) -> tx;
decode_vector_type(?INV_BLOCK) -> block;
decode_vector_type(?INV_FILTERED_BLOCK) -> filtered_block;
decode_vector_type(?INV_ERROR) -> error.


decode_hashlist(Count, Bin) ->
	decode_hashlist(Count, Bin, []).

decode_hashlist(Count, <<Id:256, Bin/binary>>, Acc) when Count > 0 ->
	decode_hashlist(Count - 1, Bin, [Id|Acc]);
decode_hashlist(0, Bin, Acc) ->
	{lists:reverse(Acc), Bin}.


decode_headers(Count, Bin) ->
	decode_headers(Count, Bin, []).

decode_headers(Count, <<Header:80/binary, 0, Bin/binary>>, Acc) when Count > 0 ->
	Header0 = btc_codec:decode_block(<<Header:80/binary, 0>>),
	decode_headers(Count - 1, Bin, [Header0|Acc]);
decode_headers(0, <<>>, Acc) ->
	lists:reverse(Acc).


decode_integer(<<X, Bin/binary>>) when X < 253 ->
	{X, Bin};
decode_integer(<<253, X:16/little, Bin/binary>>) ->
	{X, Bin};
decode_integer(<<254, X:32/little, Bin/binary>>) ->
	{X, Bin};
decode_integer(<<255, X:64/little, Bin/binary>>) ->
	{X, Bin}.


encode(#btc_version{id = Version, timestamp = Timestamp, services = Services,
		addr_recv = AddrRecv, addr_from = AddrFrom, nonce = Nonce, user_agent = UserAgent,
		start_height = StartHeight, relay = Relay}) ->
	AddrRecv0 = encode_netaddr(AddrRecv),
	AddrFrom0 = encode_netaddr(AddrFrom),
	UserAgent0 = encode_string(UserAgent),
	Relay0 =
		case Relay of
		true ->
			1;
		false ->
			0
		end,
	Payload = <<Version:32/little, Services:64/little, Timestamp:64/little,
		AddrRecv0:26/binary, AddrFrom0:26/binary, Nonce:64/little, UserAgent0/binary, StartHeight:32/little, Relay0:8>>,
	encode_message(<<"version">>, Payload);
encode(#btc_verack{}) ->
	encode_message(<<"verack">>, <<>>);
encode(#btc_addr{addr_list = AddrList}) ->
	Payload = encode_netaddrs(AddrList),
	encode_message(<<"addr">>, Payload);
encode(#btc_inv{vectors = Inv}) ->
	Payload = encode_inventory_vectors(Inv),
	encode_message(<<"inv">>, Payload);
encode(#btc_getdata{vectors = Inv}) ->
	Payload = encode_inventory_vectors(Inv),
	encode_message(<<"getdata">>, Payload);
encode(#btc_notfound{vectors = Inv}) ->
	Payload = encode_inventory_vectors(Inv),
	encode_message(<<"notfound">>, Payload);
encode(#btc_getblocks{version = Version, block_locator_hashes = BlockLocatorHashes,
		hash_stop = HashStop}) ->
	HashList = encode_hashlist(BlockLocatorHashes),
	Payload = <<Version:32/little, HashList/binary, HashStop:256/little>>,
	encode_message(<<"getblocks">>, Payload);
encode(#btc_getheaders{version = Version, block_locator_hashes = BlockLocatorHashes,
		hash_stop = HashStop}) ->
	HashList = encode_hashlist(BlockLocatorHashes),
	Payload = <<Version:32/little, HashList/binary, HashStop:256/little>>,
	encode_message(<<"getheaders">>, Payload);
% tx
% block
% headers
encode(#btc_getaddr{}) ->
	encode_message(<<"getaddr">>, <<>>);
encode(#btc_mempool{}) ->
	encode_message(<<"mempool">>, <<>>);
% checkorder
% submitorder
% reply
encode(#btc_ping{nonce = Nonce}) ->
	encode_message(<<"ping">>, <<Nonce:64/little>>);
encode(#btc_pong{nonce = Nonce}) ->
	encode_message(<<"pong">>, <<Nonce:64/little>>);
% reject
% filterload, filteradd, filterclear, merkleblock
% alert
encode(_) ->
	error.


encode_message(Command, Payload) ->
	Command0 = encode_command(Command),
	Length = byte_size(Payload),
	Checksum = btc_crypto:checksum(Payload),
	<<?BITCOIN_MAGIC:32, Command0:12/binary, Length:32/little, Checksum:32, Payload:Length/binary>>.


encode_command(C) when byte_size(C) =< 12 ->
	Bits = (12 - byte_size(C)) bsl 3,
	<<C/binary, 0:Bits>>.


encode_netaddrs(AddrList) ->
	N = encode_integer(length(AddrList)),
	encode_netaddrs(AddrList, N).

encode_netaddrs([H|T], Acc) ->
	Bin = encode_netaddr(H),
	encode_netaddrs(T, <<Acc/binary, Bin/binary>>);
encode_netaddrs([], Acc) ->
	Acc.


encode_netaddr(#btc_netaddr{time = undefined, services = Services, ip = IP, port = Port}) ->
	IP0 = encode_ip_address(IP),
	<<Services:64/little, IP0:16/binary, Port:16>>;
encode_netaddr(#btc_netaddr{time = Time, services = Services, ip = IP, port = Port}) ->
	IP0 = encode_ip_address(IP),
	<<Time:32/little, Services:64/little, IP0:16/binary, Port:16>>.


encode_ip_address({A0, A1, A2, A3}) ->
	<<?IPV4_FLAG:96, A0, A1, A2, A3>>;
encode_ip_address({A0, A1, A2, A3, A4, A5, A6, A7}) ->
	<<A0:16, A1:16, A2:16, A3:16, A4:16, A5:16, A6:16, A7:16>>.


encode_inventory_vectors(Inv) ->
	N = encode_integer(length(Inv)),
	encode_inventory_vectors(Inv, N).

encode_inventory_vectors([{Type, Hash}|T], Acc) ->
	InventoryType = encode_inventory_type(Type),
	Acc0 = <<Acc/binary, InventoryType:32/little, Hash:256>>,
	encode_inventory_vectors(T, Acc0);
encode_inventory_vectors([], Acc) ->
	Acc.


encode_inventory_type(error) ->
	?INV_ERROR;
encode_inventory_type(tx) ->
	?INV_TX;
encode_inventory_type(block) ->
	?INV_BLOCK.


encode_hashlist(Hashes) ->
	N = encode_integer(length(Hashes)),
	encode_hashlist(Hashes, N).
encode_hashlist([H|T], Acc) ->
	encode_hashlist(T, <<Acc/binary, H:256/little>>);
encode_hashlist([], Acc) ->
	Acc.


encode_string(Bin) ->
	Size = encode_integer(byte_size(Bin)),
	<<Size/binary, Bin/binary>>.


encode_integer(X) when X < 253 ->
	<<X>>;
encode_integer(X) when X < 16#FFFF ->
	<<253, X:16/little>>;
encode_integer(X) when X < 16#FFFFFFFF ->
	<<254, X:32/little>>;
encode_integer(X) ->
	<<255, X:64/little>>.



test() ->
	decode(<<249,190,180,217,118,101,114,115,105,111,110,0,0,0,0,
		0,101,0,0,0,230,205,14,38,114,17,1,0,1,0,0,0,0,0,0,0,
		238,61,82,84,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,255,255,24,207,244,56,130,1,1,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,255,255,78,8,58,249,32,141,222,142,
		62,24,171,52,176,237,15,47,83,97,116,111,115,104,105,
		58,48,46,57,46,51,47,0,0,5,0,1>>).
