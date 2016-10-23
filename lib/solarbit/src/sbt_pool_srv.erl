% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

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



send(Host, Message) when is_tuple(Host) ->
	gen_server:call(?MODULE, {send, Host, Message}).


init([]) ->
	{sbt_config, sbt_pool_address, #btc_address{id = Address}} = db:ensure({sbt_config, sbt_pool_address, bitcoin:address()}),
	{ok, Socket} = gen_udp:open(?UDP_PORT, [binary]),
	{ok, Host} = inetx:ip(),
	{ok, Port} = inet:port(Socket),
	%?TTY({pool, Host, Port}),
	ok = sbt_btc_srv:notify([block]),
	?LOG(<<"Started">>),
	{ok, #{socket => Socket, host => Host, port => Port, address => Address}}.


handle_call({send, {IP, Port}, Message}, _From, State = #{socket := Socket}) ->
	?LOG([out, Message]),
	Packet = sbt_codec:encode(?NULL_XXTEA_KEY, Message),
	Reply = gen_udp:send(Socket, IP, Port, Packet),
	{reply, Reply, State};
handle_call(miners, _From, State) ->
	{ok, Miners} = db:all(sbt_miner),
	{reply, {ok, Miners}, State}.


handle_cast(stop, State = #{socket := _Socket}) ->
	?LOG(<<"Stopping">>),
	{stop, normal, State}.


handle_info({udp, _Socket, Host, Port, _Packet}, State = #{host := Host, port := Port}) ->
	?TTY({loopback, Host, Port}), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State) ->
	case db:lookup(sbt_miner, Host) of
	M = #sbt_miner{} ->
		Miner = M#sbt_miner{port = Port, time = dttm:now()};
	undefined ->
		Miner = #sbt_miner{host = Host, port = Port, key = ?NULL_XXTEA_KEY}
	end,
	Key = Miner#sbt_miner.key,
	Message = sbt_codec:decode(Key, Packet),
	?LOG([in, Message]),
	case handle_message(Message, Miner) of
	{reply, Reply, Miner0} ->
		?LOG([out, Reply]),
		Packet0 = sbt_codec:encode(Key, Reply),
		ok = gen_udp:send(Socket, Host, Port, Packet0);
	{noreply, Miner0} ->
		ok
	end,
	db:save(Miner0),
	{noreply, State};
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
	?LOG(<<"Stopped">>),
	?TTY({terminate, Reason}),
	ok.


handle_message(#sbt_message{type = 'HELO', nonce = _Nonce}, Miner) ->
	Reply = #sbt_message{type = 'SYNC', nonce = dttm:now()},
	{reply, Reply, Miner};
handle_message(#sbt_message{type = 'NODE', nonce = Nonce, payload = Map}, Miner) ->
	Flags = maps:get(flags, Map, []),
	Address = maps:get(address, Map, <<>>),
	Coinbase = sbt_codec:coinbase(Address),
	Payload = #{template => bitcoin:encode(Coinbase)},
	Reply = #sbt_message{type = 'POOL', nonce = Nonce, payload = Payload},
	{reply, Reply, Miner#sbt_miner{flags = Flags, address = Address}};
handle_message(#sbt_message{type = 'OKAY'}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = 'INFO'}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = 'BEST'}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = 'DONE'}, Miner) ->
	{noreply, Miner};
handle_message(#sbt_message{type = 'NACK'}, Miner) ->
	{noreply, Miner};
handle_message(M = #sbt_message{}, Miner) ->
	?TTY({unknown, M}),
	{noreply, Miner}.


% TODO: Check versioning, block reward, and do correct calculation of difficulty bits
send_mining_instruction({block, Last = #btc_block{id = Previous}, TxPool}, #{socket := Socket, address := Address}) ->
	BlockHeight = Last#btc_block.height + 1,
	{ok, _Version, Reward, _Bits} = bitcoin:rules(BlockHeight),
	Version = Last#btc_block.version, % TODO: correct this
	Bits = Last#btc_block.bits, % TODO: correct calculation
	{ok, Fees, TxList} = select_transactions(TxPool),
	Coinbase = sbt_codec:coinbase(BlockHeight, Reward + Fees, Address),
	{ok, <<MerkleRoot:256>>} = bitcoin:get_merkle_root(Coinbase, TxList),
	Next = #btc_block{
		version = Version,
		prev_block = Previous,
		merkle_root = MerkleRoot,
		timestamp = dttm:datetime(dttm:now()),
		bits = Bits,
		nonce = 0,
		coinbase = undefined,
		txns = []
	},
	MiningPayload = #{height => BlockHeight, header => Next},
 	Message = #sbt_message{type = 'MINE', nonce = dttm:now(), payload = MiningPayload},
	?LOG([out, Message]),
	{ok, Miners} = db:all(sbt_miner),
	[gen_udp:send(Socket, Host, Port, sbt_codec:encode(Key, Message))
		|| #sbt_miner{host = Host, port = Port, key = Key}
		<- Miners],
	ok.


select_transactions(TxPool) ->
	% TODO: proper mempool selection and fee values
	NextTxns = lists:sublist(lists:reverse(TxPool), 1000),
	Fees = 0,
	{ok, Fees, NextTxns}.


%% questionable
logf(#sbt_message{type = Type, nonce = Nonce, payload = Payload}) ->
	[atom_to_list(Type), " nonce:", integer_to_list(Nonce), " payload:", io_lib:format("~p", [Payload])];
logf(Message) ->
	io_lib:format("~p", [Message]).
