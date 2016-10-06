% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

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


send(#sbt_miner{ip = IP}, Message) ->
	send(IP, Message);
send(IP, Message) when is_tuple(IP) ->
	gen_server:call(?MODULE, {send, IP, Message}).


miners() ->
	gen_server:call(?MODULE, miners).


% TEMP
set_key(Key) when byte_size(Key) == 16 ->
	gen_server:call(?MODULE, {key, Key}).

% TEMP
state() ->
	gen_server:call(?MODULE, state).


init(Opts) ->
	Key = proplists:get_value(key, Opts, ?NULL_XXTEA_KEY),
	Port = proplists:get_value(port, Opts, ?UDP_PORT),
	{ok, Socket} = gen_udp:open(Port, [binary]),
	ok = sbt_btc_srv:notify([block]),
	IP = netutil:get_local_ip(),
	log(<<"Started">>),
	{ok, #{key => Key, socket => Socket, local => IP, port => Port, miners => #{}}}.


handle_call({send, IP, Message}, _From, State = #{socket := Socket, port := Port, key := Key}) ->
	Message0 = Message#sbt_message{nonce = dttm:now()},
	log("=> ", Message0),
	Packet = sbt_codec:encode(Key, Message0),
	Reply = gen_udp:send(Socket, IP, Port, Packet),
	{reply, Reply, State};
handle_call(miners, _From, State = #{miners := Miners}) ->
	{reply, {ok, maps:values(Miners)}, State};
handle_call({key, Key}, _From, State) ->
	{reply, ok, State#{key => Key}};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast(stop, State = #{socket := Socket}) ->
	gen_udp:close(Socket),
	log(<<"Stopped">>),
	{stop, normal, State}.


handle_info({udp, _Socket, Host, _Port, _Packet}, State = #{local := Host}) ->
	?TTY(loopback), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State = #{key := Key, miners := Miners}) ->
	case maps:is_key(Host, Miners) of
	true ->
		Miner = maps:get(Host, Miners),
		Miner0 = Miner#sbt_miner{port = Port, time = dttm:now()},
		Miners0 = Miners;
	false ->
		Miner0 = #sbt_miner{ip = Host, port = Port, time = dttm:now()},
		Miners0 = maps:put(Host, Miner0, Miners)
	end,
	Request = sbt_codec:decode(Key, Packet),
	log("<= ", Request),
	{Miner1, Response} = handle_message(Miner0, Request),
	case Response of
	#sbt_message{} ->
		log("=> ", Response),
		Reply = sbt_codec:encode(Key, Response),
		ok = gen_udp:send(Socket, Host, Port, Reply);
	ok ->
		ok;
	Err ->
		?TTY(Err)
	end,
	Miners1 = maps:update(Host, Miner1, Miners0),
	{noreply, State#{miners => Miners1}};
handle_info({block, Height, PrevHash, Txns}, State = #{miners := _Miners}) ->
	% TODO: Send miners instructions
	?TTY({block, Height, hex:encode(<<PrevHash:256>>), length(Txns)}),
	{noreply, State};
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(Reason, _State) ->
	?TTY({terminate, Reason}),
	ok.


% DONE = 80b34dd2.
handle_message(Miner, #sbt_message{type = <<"HELO">>, nonce = _Nonce}) ->
	Reply = #sbt_message{type = <<"SYNC">>, nonce = dttm:now()},
	{Miner, Reply};
handle_message(Miner, #sbt_message{type = <<"NODE">>, nonce = Nonce, payload = Address}) ->
	Miner0 = Miner#sbt_miner{address = Address},
	Coinbase = sbt_codec:coinbase(Address),
	Reply = #sbt_message{type = <<"POOL">>, nonce = Nonce, payload = Coinbase},
	{Miner0, Reply};
handle_message(Miner, #sbt_message{type = <<"OKAY">>, nonce = Nonce}) ->
	Reply = #sbt_message{type = <<"WAIT">>, nonce = Nonce},
	{Miner, Reply};
handle_message(Miner, M = #sbt_message{type = <<"INFO">>}) ->
	?TTY(M),
	{Miner, ok};
handle_message(Miner, #sbt_message{type = <<"BEST">>}) ->
	{Miner, ok};
handle_message(Miner, #sbt_message{type = <<"DONE">>}) ->
	{Miner, ok};
handle_message(Miner, M = #sbt_message{type = <<"NACK">>}) ->
	?TTY(M),
	{Miner, ok};
handle_message(Miner, M = #sbt_message{}) ->
	?TTY({unknown, M}),
	{Miner, ok}.


log(Message) ->
	log(<<>>, Message).

log(Prefix, #sbt_message{type = Type, nonce = Nonce, payload = <<>>}) ->
	M = [Prefix, Type, " nonce:", integer_to_list(Nonce)],
	Bin = iolist_to_binary(M),
	?LOG(Bin);
log(Prefix, #sbt_message{type = Type, nonce = Nonce, payload = Payload}) ->
	M = [Prefix, Type, " nonce:", integer_to_list(Nonce), ", payload:", hex:encode(Payload)],
	Bin = iolist_to_binary(M),
	?LOG(Bin);
log(_, Message) ->
	?LOG(Message).
