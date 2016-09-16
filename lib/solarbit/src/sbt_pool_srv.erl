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
	Port = proplists:get_value(port, Opts, ?UDP_PORT),
	{ok, Socket} = gen_udp:open(Port, [binary]),
	{ok, #{socket => Socket, port => Port, miners => #{}}}.


handle_call({send, IP, Message}, _From, State = #{socket := Socket, port := Port}) ->
	Message0 = Message#message{nonce = epoch()},
	?LOG(Message0),
	Packet = encode(Message0),
	Reply = gen_udp:send(Socket, IP, Port, Packet),
	{reply, Reply, State};
handle_call(miners, _From, State = #{miners := Miners}) ->
	{reply, {ok, maps:values(Miners)}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := Socket}) ->
	gen_udp:close(Socket),
	{stop, normal, State}.


handle_info({udp, Socket, Host, Port, Packet}, State = #{miners := Miners}) ->
	case maps:is_key(Host, Miners) of
	true ->
		Miner = maps:get(Host, Miners),
		Miner0 = Miner#miner{port = Port, time = epoch()},
		Miners0 = Miners;
	false ->
		Miner0 = #miner{ip = Host, port = Port, time = epoch()},
		Miners0 = maps:put(Host, Miner0, Miners)
	end,
	Request = decode(Packet),
	?LOG(Request),
	{Miner1, Response} = handle_message(Miner0, Request),
	case Response of
	#message{} ->
		?LOG(Response),
		Reply = encode(Response),
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


decode(<<?SBT_MAGIC:32, Version:4/binary, Number:32/little, Type:4/binary, Size:32/little, Bin/binary>>) ->
	% ?TTY({type, Size, Bin}),
	case Bin of
	<<Payload:Size/binary>> ->
	 	ok;
	<<Payload:Size/binary, Bin0/binary>>  ->
		?TTY({unparsed, Bin0})
	end,
	#message{magic = ?SBT_MAGIC, version = Version, nonce = Number, type = Type, payload = Payload};
decode(Bin) ->
	Bin.


encode(#message{version = Version, nonce = Number, type = Type, payload = Payload}) ->
	Size = byte_size(Payload),
	<<?SBT_MAGIC:32, Version/binary, Number:32/little, Type/binary, Size:32/little, Payload/binary>>.


handle_message(Miner, M = #message{type = <<"HELO">>}) ->
	{Miner, M};
handle_message(Miner, #message{type = <<"INFO">>, nonce = Nonce, payload = Payload}) ->
	Address = get_address(Payload),
	Miner0 = Miner#miner{address = Address},
	Coinbase = coinbase(Miner0),
	?LOG(Miner0),
	Reply = #message{type = <<"POOL">>, nonce = Nonce, payload = Coinbase},
	{Miner0, Reply};
handle_message(Miner, M = #message{}) ->
	{Miner, ?LOG(M)}.


get_address(Bin) ->
	get_address(Bin, <<>>).
get_address(<<0, _/binary>>, Acc) ->
	Acc;
get_address(<<X, Bin/binary>>, Acc) ->
	get_address(Bin, <<Acc/binary, X>>).


epoch() ->
	{M, S, _} = erlang:timestamp(),
	M * 1000000 + S.


coinbase(#miner{address = undefined}) ->
	<<>>;
coinbase(#miner{address = Address}) ->
	FakeHeight = epoch(),
	String = <<"//solarbit/SMMA/">>,
	Hash = crypto:hash(sha, Address),
	<<3, FakeHeight:24, ?OP_DROP, (byte_size(String)), String/binary, ?OP_DROP,
		(byte_size(Hash)), Hash/binary, ?OP_DROP, ?OP_RETURN, 0:64>>.
