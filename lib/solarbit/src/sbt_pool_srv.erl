% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(sbt_pool_srv).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


send(#sbt_miner{ip = IP}, Message) ->
	send(IP, Message);
send(IP, Message) when is_tuple(IP) ->
	gen_server:call(?MODULE, {send, IP, Message}).


miners() ->
	gen_server:call(?MODULE, miners).


init([]) ->
	{ok, Socket} = gen_udp:open(?UDP_PORT, [binary]),
	ok = sbt_btc_srv:notify([block]),
	log(<<"Started">>),
	{ok, #{socket => Socket, miners => #{}}}.


handle_call({send, IP, Message}, _From, State = #{socket := Socket, miners := Miners}) ->
	Message0 = Message#sbt_message{nonce = dttm:now()},
	log([out, Message0]),
	Miner = maps:get(IP, Miners, #sbt_miner{ip = IP, port = ?UDP_PORT, key = ?NULL_XXTEA_KEY}),
	Packet = sbt_codec:encode(Miner#sbt_miner.key, Message0),
	Reply = gen_udp:send(Socket, IP, ?UDP_PORT, Packet),
	{reply, Reply, State};
handle_call(miners, _From, State = #{miners := Miners}) ->
	{ok, Miners0} = db:all(sbt_miner),
	{reply, {ok, Miners0}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := _Socket}) ->
	log(<<"Stopping">>),
	{stop, normal, State}.


handle_info({udp, _Socket, Host, _Port, _Packet}, State = #{local := Host}) ->
	?TTY(loopback), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State = #{miners := Miners}) ->
	Default = #sbt_miner{ip = Host, port = Port, key = ?NULL_XXTEA_KEY},
	_MinerDb = db:ensure(Default),
%	?TTY({db_ensure, Result}),
	Miner = maps:get(Host, Miners, Default),
	Key = Miner#sbt_miner.key,
	Request = sbt_codec:decode(Key, Packet),
	Request0 = Request#sbt_message{host = Host},
	log([in, Request0]),
	case handle_message(Request0, Miner) of
	{reply, Response, Miner0} ->
		log([out, Response]),
		Reply = sbt_codec:encode(Key, Response),
		ok = gen_udp:send(Socket, Host, Port, Reply);
	{noreply, Miner0} ->
		ok
	end,
	Miner1 = Miner0#sbt_miner{time = dttm:now()},
	db:save(Miner1),
	Miners0 = maps:put(Host, Miner1, Miners),
	{noreply, State#{miners => Miners0}};
handle_info({block_found, PreviousBlockHeader, Txns}, State) ->
	send_mining_instruction({block, PreviousBlockHeader, Txns}, State),
	{noreply, State};
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(Reason, _State = #{socket := Socket}) ->
	gen_udp:close(Socket),
	log(<<"Stopped">>),
	?TTY({terminate, Reason}),
	ok.


handle_message(#sbt_message{type = <<"HELO">>, nonce = _Nonce}, Miner) ->
	Reply = #sbt_message{type = <<"SYNC">>, nonce = dttm:now()},
	{reply, Reply, Miner};
handle_message(#sbt_message{type = <<"NODE">>, nonce = Nonce, payload = Address}, Miner) ->
	Miner0 = Miner#sbt_miner{address = Address},
	Coinbase = sbt_codec:coinbase(Address),
	Reply = #sbt_message{type = <<"POOL">>, nonce = Nonce, payload = Coinbase},
	{reply, Reply, Miner0};
handle_message(#sbt_message{type = <<"OKAY">>, nonce = Nonce}, Miner) ->
	Reply = #sbt_message{type = <<"WAIT">>, nonce = Nonce},
	{reply, Reply, Miner};
handle_message(#sbt_message{type = <<"INFO">>}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = <<"BEST">>}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = <<"DONE">>}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = <<"NACK">>}, Miner) ->
	{noreply, Miner};
handle_message(M = #sbt_message{}, Miner) ->
	?TTY({unknown, M}),
	{noreply, Miner}.


% TODO: Check versioning and do correct calculation of bits/difficulty
send_mining_instruction({block, Last = #btc_block{}, Txns}, #{socket := Socket, miners := Miners}) ->
	Next = #btc_block{
		version = Last#btc_block.version,
		prev_block = Last#btc_block.id,
		merkle_root = 0,
		timestamp = dttm:datetime(dttm:now()),
		bits = Last#btc_block.bits,
		nonce = 0,
		coinbase = undefined,
		txns = []
	},
	BlockHeight = <<(Last#btc_block.height + 1):32/little>>,
	BlockHeader = btc_codec:encode(Next),
	% TODO: Better mempool selection
	NextTxns = lists:sublist(lists:reverse(Txns), 1000),
	TxHashes = [<<X:256>> || X <- NextTxns],
	{Root, Path} = btc_crypto:merkle_root([<<0:256>>|TxHashes]),
	PathLength = length(Path),
	Path0 = list_to_binary(Path),
	MiningPayload = <<BlockHeight/binary, BlockHeader/binary, PathLength, Path0/binary>>,
	Message = #sbt_message{type = <<"MINE">>, nonce = dttm:now(), payload = MiningPayload},
	log([out, Message]),
	[gen_udp:send(Socket, IP, Port, sbt_codec:encode(Key, Message))
		|| #sbt_miner{ip = IP, port = Port, key = Key}
		<- maps:values(Miners)],
	ok.


log(Message) when is_binary(Message) ->
	?LOG(Message);
log([Prefix, Message]) ->
	?LOG([prefix(Prefix), logf(Message)]).


prefix(none) -> "";
prefix(in) -> "<= ";
prefix(out) -> "=> ";
prefix(_) -> "== ".


logf(#sbt_message{host = Host, type = Type, nonce = Nonce, payload = <<>>}) ->
	[Type, " from:", io_lib:format("~p", [Host]), " nonce:", integer_to_list(Nonce)];
logf(#sbt_message{host = Host, type = <<"NODE">>, nonce = Nonce, payload = Payload}) ->
	[<<"NODE">>, " nonce:", integer_to_list(Nonce), " from:", io_lib:format("~p", [Host]), " address:", Payload];
logf(#sbt_message{host = Host, type = <<"INFO">>, nonce = MessageId, payload = Payload}) ->
	<<Mode, Status, Tethered, Paused, Height:32/little, Nonce:32/little, Nonce2:32/little,
		Best:32/binary, Time:32/little, Rate:64/float-little>> = Payload,
	Info = io_lib:format("~p", [{{Mode, Status, Tethered, Paused}, Height, Nonce, Nonce2, hex:encode(Best), Time, Rate}]),
	[<<"INFO">>, " nonce:", integer_to_list(MessageId), " from:", io_lib:format("~p", [Host]), " detail:", Info];
logf(#sbt_message{type = Type, nonce = Nonce, payload = Payload}) ->
	[Type, " nonce:", integer_to_list(Nonce), " payload:", hex:encode(Payload)];
logf(Message) when is_binary(Message); is_list(Message) ->
	Message;
logf(Message) ->
	io_lib:format("~p", [Message]).
