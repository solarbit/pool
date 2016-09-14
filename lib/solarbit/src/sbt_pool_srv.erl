-module(sbt_pool_srv).

-include("solarbit.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


send(#miner{ip = IP}, Message) ->
	Bin = encode(Message),
	?LOG(Message),
	gen_server:call(?MODULE, {send, IP, Bin}).


miners() ->
	gen_server:call(?MODULE, miners).


state() ->
	gen_server:call(?MODULE, state).


init(Opts) ->
	Port = proplists:get_value(port, Opts, ?UDP_PORT),
	?TTY(Opts),
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
	case maps:is_key(Address, Miners) of
	false ->
		?LOG({new, Miner});
	true ->
		?LOG({known, Miner})
	end,
	Miners0 = maps:update(Address, Miner, Miners),
	try
		Request = decode(Packet),
		Response = handle_message(Request),
		case Response of
		#message{} ->
			Reply = encode(Response),
			ok = gen_udp:send(Socket, Address, Port, Reply);
		_ ->
			ok
		end
	catch _:_ ->
		?TTY({error, Packet})
	end,
	{noreply, State#{miners => Miners0}};
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


handle_message(M = #message{command = <<"HELO">>}) ->
	M;
handle_message(M = #message{nonce = N}) ->
	?LOG(M).


-define(UNIX_EPOCH_ZERO, 62167219200).

epoch() ->
	calendar:universal_time() - ?UNIX_EPOCH_ZERO.

timestamp() ->
	timestamp(calendar:universal_time()).
timestamp({Y, Mo, D, H, M, S}) ->
	timestamp({{Y, Mo, D}, {H, M, S}});
timestamp(Seconds) when is_integer(Seconds) ->
	timestamp(calendar:gregorian_seconds_to_datetime(Seconds + ?UNIX_EPOCH_ZERO));
timestamp(DateTime = {_, _}) ->
	list_to_binary(to_iso8601(DateTime, "Z")).

to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
	ISO_8601 = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
	io_lib:format(ISO_8601, [Year, Month, Day, Hour, Min, Sec, Zone]).
