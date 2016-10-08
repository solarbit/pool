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

% TEMP
state() ->
	gen_server:call(?MODULE, state).


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
	{reply, {ok, maps:values(Miners)}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := _Socket}) ->
	{stop, normal, State}.


handle_info({udp, _Socket, Host, _Port, _Packet}, State = #{local := Host}) ->
	?TTY(loopback), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State = #{miners := Miners}) ->
	Miner = maps:get(Host, Miners, #sbt_miner{ip = Host, port = Port, key = ?NULL_XXTEA_KEY}),
	Key = Miner#sbt_miner.key,
	Request = sbt_codec:decode(Key, Packet),
	log([in, Request]),
	case handle_message(Request, Miner) of
	{reply, Response, Miner0} ->
		?LOG([out, Response]),
		Reply = sbt_codec:encode(Key, Response),
		ok = gen_udp:send(Socket, Host, Port, Reply);
	{noreply, Miner0} ->
		ok
	end,
	Miners0 = maps:put(Host, Miner0, Miners),
	{noreply, State#{miners => Miners0}};
handle_info({block, PreviousBlockHeader, Txns}, State = #{miners := Miners}) ->
	?TTY({block_signal, PreviousBlockHeader, length(Txns)}),
	send_mining_instruction({block, PreviousBlockHeader, Txns}, Miners),
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


% DONE = 80b34dd2.
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


send_mining_instruction({block, Last = #btc_block{}, _Txns}, _Miners) ->
	Next = #btc_block{
		height = Last#btc_block.height + 1,
		version = Last#btc_block.version,
		prev_block = Last#btc_block.id,
		merkle_root = 0,
		timestamp = dttm:datetime(dttm:now()),
		bits = Last#btc_block.bits,
		nonce = 0,
		coinbase = undefined,
		txns = []
	},
	?TTY(hex:encode(btc_codec:encode(Next))),
	ok.

log(Message) when is_binary(Message) ->
	log([none, Message]);
log([Prefix, Message]) ->
	Bin = iolist_to_binary([prefix(Prefix), logf(Message)]),
	?LOG(Bin).

prefix(none) -> "";
prefix(in) -> "<= ";
prefix(out) -> "=> ";
prefix(_) -> "== ".

logf(#sbt_message{type = Type, nonce = Nonce, payload = <<>>}) ->
	[Type, " nonce:", integer_to_list(Nonce)];
logf(#sbt_message{type = Type, nonce = Nonce, payload = Payload}) ->
	[Type, " nonce:", integer_to_list(Nonce), " payload:", hex:encode(Payload)];
logf(Message) when is_binary(Message) orelse ?is_string(Message) ->
	Message;
logf(Message) ->
	io_lib:format("~p", [Message]).
