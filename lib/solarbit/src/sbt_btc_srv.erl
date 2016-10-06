% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(sbt_btc_srv).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").
-include_lib("bitcoin/include/bitcoin_seed.hrl").

-export([start_link/1, stop/0]).
-export([connect/1, notify/1, send/1, info/0, state/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_TIMER, 600000). % 10 minute


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


connect(Type) ->
	gen_server:cast(?MODULE, {connect, Type}).


send(Message) ->
	gen_server:call(?MODULE, {send, Message}).


notify(Types) ->
	gen_server:call(?MODULE, {notify, self(), Types}).


info() ->
	gen_server:call(?MODULE, info).


state() ->
	gen_server:call(?MODULE, state).


init(_Opts) ->
	{ok, Decoder} = btc_protocol:init(),
	State = #{
		socket => undefined,
		codec => Decoder,
		timer => undefined,
		host => undefined,
		unconfirmed => [],
		block_height => undefined,
		notify => []
	},
	case do_connect(local, State) of
	{ok, State0} ->
		{ok, State0};
	{error, Error, State0} ->
		?TTY({local, Error}),
		{ok, State0}
	end.


handle_call({send, Message}, _From, State = #{socket := Socket}) ->
	?LOG(io_lib:format("=> ~p", [Message])),
	Packet = btc_protocol:encode(Message),
	Reply = gen_tcp:send(Socket, Packet),
	{reply, Reply, State};
handle_call({notify, Pid, _Types}, _From, State = #{notify := Notify}) when is_pid(Pid) ->
	{reply, ok, State#{notify => [Pid|Notify]}};
handle_call(info, _From, State) ->
	#{host := Host, unconfirmed := Unconfirmed, block_height := Height} = State,
	Info = [{btc_host, Host}, {block_height, Height}, {unconfirmed_tx, length(Unconfirmed)}],
	{reply, {ok, Info}, State};
handle_call(state, _From, State) ->
	{reply, {ok, State}, State}.


handle_cast({connect, Type}, State) ->
	case do_connect(Type, State) of
	{ok, State0} ->
		{noreply, State0};
	{error, _Reason, State0} ->
		{noreply, State0}
	end;
handle_cast(stop, State = #{socket := Socket}) ->
	case Socket of
	undefined ->
		ignore;
	_ ->
		gen_tcp:close(Socket)
	end,
	?LOG(<<"Stopped">>),
	{stop, normal, State}.


handle_info({tcp, _Port, Packet}, State = #{socket := Socket, codec := Decoder}) ->
	{ok, Messages, Decoder0} = Decoder(Packet),
	State0 = handle_messages(Messages, State#{codec => Decoder0}),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State0};
handle_info({tcp_closed, Port}, State = #{host := Host}) ->
	?LOG(io_lib:format("closed: ~p", [Host])),
	ok = gen_tcp:close(Port),
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{noreply, State#{socket => undefined, host => undefined, timer => TimerRef} };
handle_info(timeout, State = #{socket := Socket}) ->
	?TTY({handle_info, timeout}),
	ok = gen_tcp:close(Socket),
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{noreply, State#{socket => undefined, host => undefined, timer => TimerRef} };
handle_info({timeout, TimerRef, reconnect}, State) ->
	?TTY({do_reconnect, TimerRef}),
	case do_connect(local, State) of
	{ok, State0} ->
		ok;
	{error, Reason, State0} ->
		?TTY({reconnect_failed, Reason})
	end,
	{noreply, State0};
handle_info(Message, State) ->
	?TTY({handle_info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(normal, _State) ->
	ok;
terminate(Reason, _State) ->
	?TTY(Reason),
	ok.


do_connect(Type, State = #{socket := OldSocket, timer := TimerRef}) ->
	maybe_cancel_timer(TimerRef),
	maybe_close_socket(OldSocket),
	HostList = get_host_list(Type),
	case do_connect(HostList) of
	{ok, Host, Socket} ->
		{ok, State#{socket => Socket, host => Host, timer => undefined}};
	{error, Reason, TimerRef0} ->
		{error, Reason, State#{socket => undefined, host => undefined,  timer => TimerRef0}}
	end.


do_connect([IP|T]) ->
	Port = ?BITCOIN_PORT,
	case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, once}], ?CONNECT_TIMEOUT) of
	{ok, Socket} ->
		?LOG(io_lib:format("connected: ~p", [{IP, Port}])),
		{ok, LocalPort} = inet:port(Socket),
		Version = #btc_version{
			services = ?NODE_NONE,
			addr_from = #btc_netaddr{services = ?NODE_NONE, port = LocalPort},
			timestamp = dttm:now(),
			user_agent = ?SBT_BTC_CLIENT,
			nonce = btc_crypto:nonce()
		},
		Packet = btc_protocol:encode(Version),
		?LOG(io_lib:format("=> ~p", [Version])),
		ok = gen_tcp:send(Socket, Packet),
		{ok, {Host, Port}} = inet:peername(Socket),
		{ok, {Host, Port}, Socket};
	{error, Reason} ->
		?LOG(io_lib:format("~p ~p", [{IP, Port}, Reason])),
		do_connect(T)
	end;
do_connect([]) ->
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{error, unavailable, TimerRef}.


maybe_cancel_timer(undefined) ->
	ok;
maybe_cancel_timer(TimerRef) ->
	?TTY({cancelling_timer, TimerRef}),
	erlang:cancel_timer(TimerRef).


maybe_close_socket(undefined) ->
	ok;
maybe_close_socket(Socket) ->
	?TTY({closing_socket, inet:peername(Socket)}),
	gen_tcp:close(Socket).


get_host_list(local) ->
	[loopback, {127, 0, 0, 1}, {0, 0, 0, 0}, netutil:get_local_ip()];
get_host_list(remote) ->
	List = [{rand:uniform(), parse_ip(N)} || N <- ?SEED_BITNODES_IO],
	[X || {_, X} <- lists:sort(List)];
get_host_list(_) ->
	[].


parse_ip(String) when is_list(String) ->
	list_to_tuple([binary_to_integer(X) || X <- re:split(String, <<"\\.">>)]).


handle_messages([Message|T], State = #{socket := Socket}) ->
	case handle_message(Message, State) of
	{noreply, State0} ->
		ok;
	{reply, Reply, State0} ->
		?LOG(io_lib:format("=> ~p", [Reply])),
		Packet0 = btc_protocol:encode(Reply),
		gen_tcp:send(Socket, Packet0)
	end,
	handle_messages(T, State0);
handle_messages([], State) ->
	State.


handle_message(M = #btc_version{}, State) ->
	?LOG(io_lib:format("<= VERSION ~p", [M])),
	{reply, #btc_verack{}, State};
handle_message(#btc_ping{nonce = Nonce}, State) ->
	?LOG(io_lib:format("<= PING nonce: ~p", [Nonce])),
	{reply, #btc_pong{nonce = Nonce}, State};
handle_message(#btc_inv{vectors = VectorList}, State = #{unconfirmed := TxList}) ->
	TxList0 = [X || {tx, X} <- VectorList] ++ TxList,
	State0 = State#{unconfirmed => TxList0},
	NewBlocks = [X || X = {block, _} <- VectorList],
	case NewBlocks of
	[] ->
		{noreply, State0};
	_ ->
		NewBlockHashes = [hex:encode(<<X:256/little>>) || {block, X} <- NewBlocks],
		?LOG(io_lib:format("<= INV blocks:~p, txns:~p", [NewBlockHashes, length(TxList0)])),
		{reply, #btc_getdata{vectors = NewBlocks}, State0}
	end;
handle_message(Block = #btc_block{id = Hash, txns = Txns}, State = #{unconfirmed := TxList, notify := Notify}) ->
	BlockHeight = btc_codec:get_block_height(Block),
	ConfirmedTx = [X || #btc_tx{id = X} <- Txns],
	TxList0 = TxList -- ConfirmedTx,
	Log = io_lib:format("<= BLOCK height: ~p, confirmed: ~p, unconfirmed: [was: ~p, now: ~p]",
		[BlockHeight, length(ConfirmedTx), length(TxList), length(TxList0)]),
	?TTY({new_block, BlockHeight, dttm:timestamp()}),
	?LOG(Log),
	[Pid ! {block, BlockHeight + 1, Hash, TxList0} || Pid <- Notify],
	{noreply, State#{unconfirmed => TxList0, block_height => BlockHeight}};
handle_message(#btc_addr{addr_list = List}, State) ->
	?LOG(io_lib:format("<= ADDR ~p", [List])),
	{noreply, State};
handle_message(Message, State) ->
	?LOG(io_lib:format("<= ~p", [Message])),
	{noreply, State}.
