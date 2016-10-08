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


write(Module, Message) when is_atom(Module) ->
	gen_server:cast(?MODULE, {log, Module, Message}).


init([]) ->
	FileName = code:lib_dir(solarbit) ++ "/../../log/pool.log",
	{ok, File} = file:open(FileName, [append, raw]),
	{ok, #{file => File}}.


handle_call(_, _From, State) ->
	{reply, ok, State}.


handle_cast({log, Module, Message}, State = #{file := File}) ->
	Bin = format(Module, Message),
	ok = file:write(File, Bin),
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


format(Prefix, Message) when is_binary(Message) ->
	String = atom_to_list(Prefix),
	[dttm:timestamp(), " [", String, "] ", Message, $\n];
format(Prefix, Message) ->
	Bin = io_lib:format("~p", [Message]),
	format(Prefix, Bin).
