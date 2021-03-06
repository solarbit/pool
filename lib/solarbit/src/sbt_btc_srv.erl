% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(sbt_btc_srv).

-include("solarbit.hrl").
-include_lib("bitcoin/include/bitcoin.hrl").
-include_lib("bitcoin/include/bitcoin_seed.hrl").

-export([start_link/1, stop/0]).
-export([connect/1, notify/1, ping/0, set_bitcoind/2, send/1, info/0, state/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_TIMER, 600000). % 10 minutes


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


connect(Type) ->
	gen_server:cast(?MODULE, {connect, Type}).


ping() ->
	send(#btc_ping{nonce = btc_crypto:nonce()}).


set_bitcoind(Host, Port) ->
	db:save({sbt_config, sbt_btc_address, {Host, Port}}).


send(Message) ->
	gen_server:call(?MODULE, {send, Message}).


notify(Types) ->
	gen_server:call(?MODULE, {notify, self(), Types}).


info() ->
	{ok, Info} = gen_server:call(?MODULE, info),
	Info.

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
	case do_connect(pool, State) of
	{ok, State0} ->
		{ok, State0};
	{error, _Error, State0} ->
		?LOG([none, <<"Local BTC Daemon Offline">>]),
		{ok, State0}
	end.


handle_call({send, Message}, _From, State = #{socket := Socket}) ->
	?LOG([out, Message]),
	Packet = btc_protocol:encode(Message),
	Reply = gen_tcp:send(Socket, Packet),
	{reply, Reply, State};
handle_call({notify, Pid, _Types}, _From, State = #{notify := Notify}) when is_pid(Pid) ->
	{reply, ok, State#{notify => [Pid|Notify]}};
handle_call(info, _From, State) ->
	#{host := Host, unconfirmed := Unconfirmed, block_height := Height} = State,
	Info = [{btc_host, Host}, {block_height, Height}, {mempool, length(Unconfirmed)}],
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
	?LOG(io_lib:format("Closed: ~p", [Host])),
	ok = gen_tcp:close(Port),
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{noreply, State#{socket => undefined, host => undefined, timer => TimerRef} };
handle_info(timeout, State = #{socket := Socket}) ->
	?TTY({handle_info, timeout}),
	ok = gen_tcp:close(Socket),
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{noreply, State#{socket => undefined, host => undefined, timer => TimerRef} };
handle_info({timeout, _TimerRef, reconnect}, State) ->
	% ?TTY({do_reconnect, TimerRef}),
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
	?TTY({terminate, Reason}),
	ok.


do_connect(Type, State = #{socket := OldSocket, timer := TimerRef}) ->
	maybe_cancel_timer(TimerRef),
	maybe_close_socket(OldSocket),
	[sbt_db_srv:clear(X) || X <- [btc_tx, btc_block, btc_height]],
	{ok, Decoder} = btc_protocol:init(),
	State0 = State#{codec => Decoder, unconfirmed => []},
	HostList = get_host_list(Type),
	case do_connect(HostList) of
	{ok, Host, Socket} ->
		{ok, State0#{socket => Socket, host => Host, timer => undefined}};
	{error, Reason, TimerRef0} ->
		{error, Reason, State0#{socket => undefined, host => undefined,  timer => TimerRef0}}
	end.

do_connect([IP|T]) ->
	Port = ?BITCOIN_PORT,
	case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active, once}], ?CONNECT_TIMEOUT) of
	{ok, Socket} ->
		?LOG(io_lib:format("Connected: ~p", [{IP, Port}])),
		{ok, LocalPort} = inet:port(Socket),
		Version = #btc_version{
			services = ?NODE_NONE,
			addr_from = #btc_netaddr{services = ?NODE_NONE, port = LocalPort},
			timestamp = dttm:now(),
			user_agent = ?SBT_BTC_CLIENT,
			nonce = btc_crypto:nonce()
		},
		Packet = btc_protocol:encode(Version),
		?LOG([out, Version]),
		ok = gen_tcp:send(Socket, Packet),
		{ok, {Host, Port}} = inet:peername(Socket),
		{ok, {Host, Port}, Socket};
	{error, Reason} ->
		?LOG([local, io_lib:format("{~p, ~p} ~p", [IP, Port, Reason])]),
		do_connect(T)
	end;
do_connect([]) ->
	TimerRef = erlang:start_timer(?RECONNECT_TIMER, self(), reconnect),
	{error, unavailable, TimerRef}.


maybe_cancel_timer(undefined) ->
	ok;
maybe_cancel_timer(TimerRef) ->
	% ?TTY({cancelling_timer, TimerRef}),
	erlang:cancel_timer(TimerRef).


maybe_close_socket(undefined) ->
	ok;
maybe_close_socket(Socket) ->
	Host = inet:peername(Socket),
	?TTY({closing_socket, Host}),
	gen_tcp:close(Socket).


get_host_list(pool) ->
	case db:lookup(sbt_config, sbt_btc_address) of
	{sbt_config, sbt_btc_address, {Host, ?BITCOIN_PORT}} ->
		[Host];
	undefined ->
		get_host_list(local)
	end;
get_host_list(local) ->
	[loopback, {127, 0, 0, 1}, {0, 0, 0, 0}];
get_host_list(remote) ->
	List = [{rand:uniform(), parse_ip(N)} || N <- ?SEED_BITNODES_IO],
	[X || {_, X} <- lists:sort(List)].


parse_ip(String) when is_list(String) ->
	list_to_tuple([binary_to_integer(X) || X <- re:split(String, <<"\\.">>)]).


handle_messages([Message|T], State = #{socket := Socket}) ->
	case handle_message(Message, State) of
	{noreply, State0} ->
		ok;
	{reply, Reply, State0} ->
		?LOG([out, Reply]),
		Packet0 = btc_protocol:encode(Reply),
		gen_tcp:send(Socket, Packet0)
	end,
	handle_messages(T, State0);
handle_messages([], State) ->
	State.


handle_message(M = #btc_version{}, State) ->
	?LOG([in, M]),
	{reply, #btc_verack{}, State};
handle_message(M = #btc_verack{}, State) ->
	?LOG([in, M]),
	{noreply, State};
handle_message(M = #btc_ping{nonce = Nonce}, State = #{unconfirmed := TxList}) ->
	?LOG(io_lib:format("MEMPOOL unconfirmed:~p", [length(TxList)])),
	?LOG([in, M]),
	{reply, #btc_pong{nonce = Nonce}, State};
handle_message(M = #btc_pong{nonce = _Nonce}, State) ->
	?LOG([in, M]),
	{noreply, State};
handle_message(#btc_inv{vectors = VectorList}, State = #{unconfirmed := TxList}) ->
	NewTx = [X || {tx, X} <- VectorList],
	TxList0 = NewTx ++ TxList,
	State0 = State#{unconfirmed => TxList0},
	NewBlocks = [X || X = {block, _} <- VectorList],
	case NewBlocks of
	[] ->
		{noreply, State0};
	_ ->
		?LOG([in, io_lib:format("INV blocks:~p, txns:~p", [length(NewBlocks), length(TxList0)])]),
		{reply, #btc_getdata{vectors = NewBlocks}, State0}
	end;
handle_message(Block = #btc_block{id = BlockId, height = BlockHeight, txns = Txns}, State = #{unconfirmed := TxList, notify := Notify}) ->
	db:save({btc_block, BlockId, Block#btc_block{txns = []}}),
	db:save({btc_height, BlockHeight, BlockId}),
	ConfirmedTx = [X || #btc_tx{id = X} <- Txns],
	TxList0 = TxList -- ConfirmedTx,
	?LOG([none, io_lib:format("BLOCK height:~p confirmed:~p unconfirmed[was:~p now:~p]",
		[BlockHeight, length(ConfirmedTx), length(TxList), length(TxList0)]) ]),
	[Pid ! {block_found, Block#btc_block{txns = []}, TxList0} || Pid <- Notify],
	{noreply, State#{unconfirmed => TxList0, block_height => BlockHeight}};

handle_message(Tx = #btc_tx{id = TxId, tx_in = TxIn}, State = #{unconfirmed := TxList}) ->
	db:save({btc_tx, TxId, Tx}),
	case lists:member(TxId, TxList) of
	true ->
		PreviousTxs = #btc_getdata{vectors = [{tx, X} || #btc_tx_in{outpoint_ref = X} <- TxIn]},
		{reply, PreviousTxs, State};
	false ->
		{noreply, State}
	end;
handle_message(#btc_addr{addr_list = List}, State) ->
	?LOG([in, io_lib:format("ADDR ~p", [List])]),
	{noreply, State};
handle_message(Message, State) ->
	?LOG([in, Message]),
	{noreply, State}.


logf(#btc_verack{}) ->
	["VERACK"];
logf(#btc_ping{nonce = Nonce}) ->
	["PING nonce:", integer_to_list(Nonce)];
logf(#btc_pong{nonce = Nonce}) ->
	["PONG nonce:", integer_to_list(Nonce)];
logf(Temp = #btc_version{}) ->
	["VERSION version:", io_lib:format("~p", [Temp])];
logf(#btc_getdata{vectors = Vectors}) ->
	Vectors0 = [{Type, hex:encode(<<Number:256/little>>)} || {Type, Number} <- Vectors],
	Message = io_lib:format("~p", [Vectors0]),
	["GETDATA ", Message];
logf(#btc_notfound{vectors = Vectors}) ->
	Message = io_lib:format("~p", [length(Vectors)]),
	["NOTFOUND number:", Message];
logf(Message) ->
	Message.
