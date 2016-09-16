-module(sbt_log_srv).

-include("solarbit.hrl").

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


write(#message{nonce = Nonce, type = Type, payload = Payload}) when byte_size(Payload) > 0 ->
	ID = io_lib:format("~w", [Nonce]),
	Message = [timestamp(), " [", Type, "] id:", ID, " payload:", hex:encode(Payload), "\n"],
	gen_server:cast(?MODULE, {log, Message});
write(#message{nonce = Nonce, type = Type}) ->
	ID = io_lib:format("~w", [Nonce]),
	Message = [timestamp(), " [", Type, "] id:", ID, "\n"],
	gen_server:cast(?MODULE, {log, Message});
write(#miner{ip = {A, B, C, D}, port = Port, time = _Time, address = Address}) when is_binary(Address) ->
	Message = io_lib:format("host:~p.~p.~p.~p:~p address:", [A, B, C, D, Port]),
	gen_server:cast(?MODULE, {log, [timestamp(), " [NODE] ", Message, Address, "\n"]});
write(#miner{ip = {A, B, C, D}, port = Port, time = _Time}) ->
	Message = io_lib:format("host:~p.~p.~p.~p:~p", [A, B, C, D, Port]),
	gen_server:cast(?MODULE, {log, [timestamp(), " [NODE] ", Message, "\n"]});
write(Message) ->
	Bin = io_lib:format("[???] ~p~n", [Message]),
	gen_server:cast(?MODULE, {log, [timestamp(), Bin]}).


init([]) ->
	FileName = code:priv_dir(solarbit) ++ "/pool.log",
	{ok, File} = file:open(FileName, [append, raw]),
	{ok, #{file => File}}.


handle_call(M, _From, State) ->
	?TTY({call, M}),
	{reply, ok, State}.


handle_cast({log, Message}, State = #{file := File}) ->
	ok = file:write(File, Message),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State}.


handle_info(Message, State = #{file := _File}) ->
	% file:write(File, Message),
	?TTY({info, Message}),
	{noreply, State}.


code_change(OldVsn, State, Extra) ->
	?TTY({code_change, OldVsn, Extra}),
	State.


terminate(Reason, _State = #{file := File}) ->
	% be sure to close!
	?TTY(close_file),
	file:close(File),
	?TTY({terminate, Reason}),
	ok.


-define(UNIX_EPOCH_ZERO, 62167219200).

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
