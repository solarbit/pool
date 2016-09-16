-module(sbt_pool_srv).

-include("solarbit.hrl").


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
	Bin = encode(Message),
	?LOG(Message),
	gen_server:call(?MODULE, {send, IP, Bin}).


miners() ->
	gen_server:call(?MODULE, miners).


state() ->
	gen_server:call(?MODULE, state).


init(Opts) ->
	Port = proplists:get_value(port, Opts, ?UDP_PORT),
	{ok, Socket} = gen_udp:open(Port, [binary]),
	{ok, #{socket => Socket, port => Port, miners => #{}}}.


handle_call({send, IP, Message}, _From, State = #{socket := Socket, port := Port}) ->
	Reply = gen_udp:send(Socket, IP, Port, Message),
	{reply, Reply, State};
handle_call(miners, _From, State = #{miners := Miners}) ->
	{reply, {ok, Miners}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := Socket}) ->
	gen_udp:close(Socket),
	{stop, normal, State}.


handle_info({udp, Socket, Address, Port, Packet}, State = #{miners := Miners}) ->
	Miner = #miner{ip = Address, port = Port, time = epoch()},
	Request = decode(Packet),
	{Miner0, Response} = handle_message(Miner, Request),
	case Response of
	#message{} ->
		Reply = encode(Response),
		ok = gen_udp:send(Socket, Address, Port, Reply);
	_ ->
		ok
	end,
	case maps:is_key(Address, Miners) of
	false ->
		?LOG(Miner0),
		Miners0 = maps:put(Address, Miner0, Miners);
	true ->
		% ?LOG({known, Miner0})
		Miners0 = maps:update(Address, Miner0, Miners)
	end,
	State0 = State#{miners => Miners0},
	{noreply, State0};
handle_info(Message, State) ->
	?TTY({info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(Reason, _State) ->
	?TTY({terminate, Reason}),
	ok.


decode(<<?SBT_MAGIC:32, Version:4/binary, Number:32/little, Command:4/binary, Size:32/little, Bin/binary>>) ->
	% ?TTY({Command, Size, Bin}),
	case Bin of
	<<Payload:Size/binary>> ->
	 	ok;
	<<Payload:Size/binary, Bin0/binary>>  ->
		?TTY({unparsed, Bin0})
	end,
	#message{magic = ?SBT_MAGIC, version = Version, nonce = Number, command = Command, payload = Payload};
decode(Bin) ->
	Bin.


encode(#message{version = Version, nonce = Number, command = Command, payload = Payload}) ->
	Size = byte_size(Payload),
	<<?SBT_MAGIC:32, Version/binary, Number:32/little, Command/binary, Size:32/little, Payload/binary>>.


handle_message(Miner, M = #message{command = <<"HELO">>}) ->
	{Miner, M};
handle_message(Miner, M = #message{command = <<"INFO">>, payload = Payload}) ->
	Info = get_address(Payload),
	Reply = #message{command = <<"POOL">>, payload = <<>>},
	?LOG(M),
	{Miner#miner{info = Info}, Reply};
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
