-module(sbt_pool_srv).

-include("solarbit.hrl").
-include("bitcoin_script.hrl").


-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


send(#miner{ip = IP}, Message) ->
	send(IP, Message);
send(IP, Message) when is_tuple(IP) ->
	gen_server:call(?MODULE, {send, IP, Message}).


miners() ->
	gen_server:call(?MODULE, miners).


state() ->
	gen_server:call(?MODULE, state).


init(Opts) ->
	Key = proplists:get_value(key, Opts, <<"SolarBitSolarBit">>),
	Port = proplists:get_value(port, Opts, ?UDP_PORT),
	{ok, Socket} = gen_udp:open(Port, [binary]),
	IP = get_local_ip(),
	{ok, #{key => Key, socket => Socket, local => IP, port => Port, miners => #{}}}.


handle_call({send, IP, Message}, _From, State = #{socket := Socket, port := Port, key := Key}) ->
	Message0 = Message#message{nonce = epoch()},
	?LOG(Message0),
	Packet = encode(Key, Message0),
	Reply = gen_udp:send(Socket, IP, Port, Packet),
	{reply, Reply, State};
handle_call(miners, _From, State = #{miners := Miners}) ->
	{reply, {ok, maps:values(Miners)}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := Socket}) ->
	gen_udp:close(Socket),
	{stop, normal, State}.


handle_info({udp, _Socket, Host, _Port, _Packet}, State = #{local := Host}) ->
	?TTY(loopback), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State = #{key := Key, miners := Miners}) ->
	case maps:is_key(Host, Miners) of
	true ->
		Miner = maps:get(Host, Miners),
		Miner0 = Miner#miner{port = Port, time = epoch()},
		Miners0 = Miners;
	false ->
		Miner0 = #miner{ip = Host, port = Port, time = epoch()},
		Miners0 = maps:put(Host, Miner0, Miners)
	end,
	Request = decode(Key, Packet),
	?LOG(Request),
	{Miner1, Response} = handle_message(Miner0, Request),
	case Response of
	#message{} ->
		?LOG(Response),
		Reply = encode(Key, Response),
		ok = gen_udp:send(Socket, Host, Port, Reply);
	ok ->
		ok;
	Err ->
		?TTY(Err)
	end,
	Miners1 = maps:update(Host, Miner1, Miners0),
	{noreply, State#{miners => Miners1}};
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(Reason, _State) ->
	?TTY({terminate, Reason}),
	ok.


decode(Key, <<?SBT_MAGIC:32, Version:4/binary, Number:32/little, Type:4/binary, Size:32/little, Bin/binary>>) ->
	Payload = decode_payload(Key, Size, Bin),
	#message{magic = ?SBT_MAGIC, version = Version, nonce = Number, type = Type, payload = Payload};
decode(_, Bin) ->
	Bin.


decode_payload(_, 0, <<>>) ->
	<<>>;
decode_payload(Key, Size, Bin) when byte_size(Bin) == Size ->
	?TTY({encrypted, hex:encode(Bin)}),
	Value = xxtea:decode(Key, Bin),
	?TTY({decrypted, hex:encode(Value)}),
	Pad = binary:last(Value),
	PayloadSize = byte_size(Value) - Pad,
	<<Payload:PayloadSize/binary, _:Pad/binary>> = Value,
	Payload.


encode(Key, #message{version = Version, nonce = Number, type = Type, payload = Payload}) ->
	Encrypted = encode_payload(Key, Payload),
	Size = byte_size(Encrypted),
%	Size = byte_size(Payload),
%	Encrypted = Payload,
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


% DONE = 80b34dd2.
handle_message(Miner, #message{type = <<"HELO">>, nonce = _Nonce}) ->
	Reply = #message{type = <<"SYNC">>, nonce = epoch()},
	{Miner, Reply};
handle_message(Miner, #message{type = <<"NODE">>, nonce = Nonce, payload = Payload}) ->
	Address = Payload,
	?TTY(Address),
	Miner0 = Miner#miner{address = Address},
	Coinbase = coinbase(Miner0),
	?TTY(hex:encode(Coinbase)),
	Reply = #message{type = <<"POOL">>, nonce = Nonce, payload = Coinbase},
	{Miner0, Reply};
handle_message(Miner, #message{type = <<"OKAY">>, nonce = Nonce}) ->
	Reply = #message{type = <<"WAIT">>, nonce = Nonce},
	{Miner, Reply};
handle_message(Miner, M = #message{type = <<"INFO">>}) ->
	?TTY(M),
	{Miner, ok};
handle_message(Miner, #message{type = <<"BEST">>}) ->
	{Miner, ok};
handle_message(Miner, #message{type = <<"DONE">>}) ->
	{Miner, ok};
handle_message(Miner, M = #message{type = <<"NACK">>}) ->
	?TTY(M),
	{Miner, ok};
handle_message(Miner, M = #message{}) ->
	?TTY({unknown, M}),
	{Miner, ok}.


epoch() ->
	{M, S, _} = erlang:timestamp(),
	M * 1000000 + S.


coinbase(#miner{address = undefined}) ->
	<<>>;
coinbase(#miner{address = Address}) ->
	FakeHeight = epoch(),
	String = <<"//SolarBit/SMM/A/">>,
	Hash = crypto:hash(sha, Address),
	<<3, FakeHeight:24, ?OP_DROP, (byte_size(String)), String/binary, ?OP_DROP,
		(byte_size(Hash)), Hash/binary, ?OP_DROP, ?OP_RETURN, 0:64>>.


get_local_ip() ->
	{ok, L} = inet:getif(),
	[IP] = [X || {X, _, _} <- L, X =/= {127, 0, 0, 1}],
	IP.
