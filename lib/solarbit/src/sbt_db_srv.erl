% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(sbt_db_srv).

-include("solarbit.hrl").

-behaviour(supervisor).
-export([start_link/1, init/1]).

-compile(export_all).

-define(DB_TIMEOUT, 10000).


start() ->
	start_link([]).


start_link([]) ->
	DataPath = code:priv_dir(solarbit) ++ "/db." ++ atom_to_list(node()),
	?LOG("Database Path: " ++ DataPath),
	case mnesia:system_info(is_running) of
	no ->
		% NOTE: Mnesia MUST NOT be running for this call to be safe...
		ok = application:set_env(mnesia, dir, DataPath),
		case mnesia:system_info(use_dir) of
		true ->
			ok;
		false ->
			ok = mnesia:create_schema([node()])
		end,
		case supervisor:start_link({local, ?MODULE}, mnesia_sup, []) of
		{ok, Pid} ->
			mnesia:wait_for_tables(mnesia:system_info(tables), ?DB_TIMEOUT),
			Result = ensure_tables(?DB_TABLES),
			case Result of
			{ok, Tables, []} when is_list(Tables) ->
				?LOG(io_lib:format("Tables: ~p", [lists:sort(Tables)]));
			_ ->
				?LOG(Result)
			end,
			{ok, Pid};
		Error ->
			Error
		end;
	_ -> % yes, starting, stopping
		{error, {mnesia, already_in_use}}
	end.


init([]) ->
	ignore.


clear(Table) when is_atom(Table) ->
	{atomic, ok} = mnesia:clear_table(Table).


ensure_tables(Tables) ->
	Existing = mnesia:system_info(tables),
	Missing = [Table || Table = {Name, _Fields} <- Tables, not lists:member(Name, Existing)],
	[create_table(Name, Fields) || {Name, Fields} <- Missing],
	{ok, Existing, Missing}.


create_table(Table, Fields) when is_atom(Table), is_list(Fields) ->
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, [node()]}, {attributes, Fields}]),
    ok = mnesia:wait_for_tables([Table], ?DB_TIMEOUT).


drop(Table) when is_atom(Table) ->
	drop_tables([Table]).


drop_tables(Tables) when is_list(Tables) ->
	drop_tables(Tables, []).

drop_tables([H|T], Acc) ->
	{atomic, ok} = mnesia:delete_table(H),
	drop_tables(T, [H|Acc]);
drop_tables([], Acc) ->
	{ok, Acc}.
