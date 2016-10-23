% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(sbt_log_srv).

-include("solarbit.hrl").

-export([start_link/1, stop/0, write/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link([]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


write(Module, [Prefix, Message]) when is_atom(Prefix) ->
	gen_server:cast(?MODULE, {log, Module, [prefix(Prefix), format(Message)]});
write(Module, Message) when is_atom(Module) ->
	gen_server:cast(?MODULE, {log, Module, format(Message)}).


init([]) ->
	FileName = code:lib_dir(solarbit) ++ "/../../log/pool.log",
	{ok, File} = file:open(FileName, [write, raw]),
	{ok, #{file => File}}.


handle_call(_Message, _From, State) ->
	{reply, ok, State}.


handle_cast({log, Module, Message}, State = #{file := File}) ->
	Out = [dttm:timestamp(), " [", atom_to_list(Module), "] ", Message, $\n],
	ok = file:write(File, Out),
	{noreply, State};
handle_cast(stop, State = #{file := File}) ->
	file:close(File),
	{stop, normal, State}.


handle_info(_Message, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	State.


terminate(_Reason, _State = #{file := File}) ->
	file:close(File),
	ok.


format(X) when is_binary(X) ->
	X;
format(X) when is_list(X) ->
	case is_iolist(X) of
	true ->
		X;
	false ->
		io_lib:format("~w", [X])
	end;
format(X) ->
	io_lib:format("~w", [X]).


is_iolist([]) ->
	true;
is_iolist([H|T]) when is_binary(H) ->
	is_iolist(T);
is_iolist([H|T]) when is_integer(H), H >= 0, H =< 255 ->
	is_iolist(T);
is_iolist([H|T]) when is_list(H) ->
	case is_iolist(H) of
	true ->
		is_iolist(T);
	false ->
		false
	end;
is_iolist(_) ->
	false.


prefix(local) -> "== ";
prefix(in) -> "<= ";
prefix(out) -> "=> ";
prefix(_) -> "".
