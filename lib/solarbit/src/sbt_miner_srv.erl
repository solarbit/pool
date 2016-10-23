% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(sbt_miner_srv).

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


init([]) ->
	% TODO: Use a BTC address from a cold store, don't do this in a prod environment!
	{sbt_config, sbt_miner_address, #btc_address{id = Address}} = db:ensure({sbt_config, sbt_miner_address, bitcoin:address()}),
	{ok, Socket} = gen_udp:open(0, [binary]),
	{ok, Port} = inet:port(Socket),
	Key = ?NULL_XXTEA_KEY,
	?LOG(<<"CPU Miner Started on port ", (integer_to_binary(Port))/binary>>),
	Packet = sbt_codec:encode(Key, #sbt_message{type = 'HELO', nonce = dttm:now()}),
	ok = gen_udp:send(Socket, localhost, ?UDP_PORT, Packet),
	{ok, #{socket => Socket, port => Port, address => Address, key => Key, paused => false}}.


handle_call(Message, _From, State) ->
	{reply, {ok, Message}, State}.


handle_cast(stop, State = #{socket := _Socket}) ->
	?LOG(<<"CPU Miner Stopping">>),
	{stop, normal, State}.


handle_info({udp, _Socket, Host, Port, _Packet}, State = #{port := Port}) ->
	?TTY({loopback, Host, Port}), % ignore messages from self
	{noreply, State};
handle_info({udp, Socket, Host, Port, Packet}, State = #{key := Key}) ->
	Message = sbt_codec:decode(Key, Packet),
	Message0 = Message#sbt_message{host = Host},
	%?TTY({in, Message0}),
	case handle_message(Message0, State) of
	{reply, Reply, State0} ->
		%?TTY({out, Reply}),
		Packet0 = sbt_codec:encode(Key, Reply),
		ok = gen_udp:send(Socket, Host, Port, Packet0);
	{noreply, State0} ->
		ok
	end,
	{noreply, State0}.


code_change(_OldVsn, State, _Extra) ->
	State.


terminate(_Reason, _State = #{socket := Socket}) ->
	gen_udp:close(Socket).


handle_message(#sbt_message{type = 'PING', nonce = Nonce}, State) ->
	Reply = #sbt_message{type = 'HELO', nonce = Nonce},
	{reply, Reply, State};
handle_message(#sbt_message{type = 'SYNC', nonce = Nonce}, State = #{address := Address}) ->
	case maps:get(paused, State) of
	true ->
		Flags = [ready, compact, paused];
	false ->
		Flags = [ready, compact]
	end,
	Payload = #{flags => Flags, address => Address},
	Reply = #sbt_message{type = 'NODE', nonce = Nonce, payload = Payload},
	{reply, Reply, State};
handle_message(#sbt_message{type = 'POOL', nonce = Nonce, payload = Payload}, State) ->
	Coinbase = maps:get(template, Payload, <<>>),
	Reply = #sbt_message{type = 'OKAY', nonce = Nonce},
	{reply, Reply, State#{coinbase => Coinbase}};
handle_message(#sbt_message{type = 'WAIT'}, State) ->
	{noreply, State#{paused => true}};
handle_message(M = #sbt_message{}, State) ->
	?TTY(M),
	{noreply, State}.
