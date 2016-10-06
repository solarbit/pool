% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-module(db).

% Convenience API for the Mnesia Database

-include("base.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([info/0, info/1]).
-export([all/1, lookup/2, save/1, ensure/1]).
-export([create/1, read/2, update/1, delete/1]).
-export([select/1, select/2, select/3, run/2]).
-export([increment/2, dump_tables/1]).


info() ->
	[{X, mnesia:table_info(X, attributes), mnesia:table_info(X, size)} || X <- mnesia:system_info(tables), X =/= schema].

info(Table) when is_atom(Table) ->
	case lists:member(Table, mnesia:system_info(tables)) of
	true ->
		io:format(user, "~p~n", [mnesia:table_info(Table, all)]),
		ok;
	false ->
		undefined
	end.


all(Table) when is_atom(Table) ->
	T = fun () ->
			Pattern = mnesia:table_info(Table, wild_pattern),
			mnesia:match_object(Pattern)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.


lookup(Table, Key) ->
	case mnesia:dirty_read({Table, Key}) of
	[Result] ->
		Result;
	[] ->
		undefined
	end.


create(Record) when ?is_record(Record) ->
	Table = element(1, Record),
	Key = element(2, Record),
    T = fun () ->
			case mnesia:wread({Table, Key}) of
			[] ->
				mnesia:write(Record);
			_ ->
				{error, exists}
			end
		end,
    {atomic, Result} = mnesia:transaction(T),
    Result.


read(Table, Key) when is_atom(Table) ->
	T = fun () ->
			mnesia:read({Table, Key})
		end,
    case mnesia:transaction(T) of
    {atomic, [Result]} ->
		Result;
	_ ->
		undefined
	end.


update(Record) when ?is_record(Record) ->
	Table = element(1, Record),
	Key = element(2, Record),
    T = fun () ->
			case mnesia:wread({Table, Key}) of
			[] ->
				{error, undefined};
			_ ->
				mnesia:write(Record)
			end
		end,
    {atomic, Result} = mnesia:transaction(T),
    Result.


delete(Record) when ?is_record(Record) ->
	delete(element(1, Record), element(2, Record)).
delete(Table, Key) when is_atom(Table) ->
	T = fun () ->
			mnesia:delete({Table, Key})
		end,
	{atomic, Result} = mnesia:transaction(T),
	Result.


save(Record) when ?is_record(Record) ->
	save([Record]);
save(Records) when is_list(Records) ->
    T = fun () ->
			[mnesia:write(Record) || Record <- Records]
		end,
    {atomic, Result} = mnesia:transaction(T),
    Result.


ensure(List) when is_list(List) ->
	[ensure(X) || X <- List];

ensure(Record) when ?is_record(Record) ->
	Table = element(1, Record),
	Key = element(2, Record),
    T = fun () ->
			case mnesia:wread({Table, Key}) of
			[] ->
				mnesia:write(Record);
			_ ->
				ok
			end
		end,
    {atomic, Result} = mnesia:transaction(T),
    Result.


%% TODO review use of match_object -> mnesia:select
select(Match) when is_tuple(Match) ->
	T = fun() ->
			mnesia:match_object(Match)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.

% cf. all(Table)
select(Table, _Num) when is_atom(Table) ->
	T = fun () ->
		qlc:e(qlc:q([X || X <- mnesia:table(Table)]))
	end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.

select(Table, Field, Value) when is_atom(Field) ->
	Spec = mnesia:table_info(Table, attributes),
	Match = create_match({Table}, {Field, Value}, Spec),
    T = fun () ->
			mnesia:match_object(Match)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.


create_match(Record, {Field, Value}, [Field|T]) ->
	create_match(erlang:append_element(Record, Value), {Field, Value}, T);
create_match(Record, Match, [_|T]) ->
	create_match(erlang:append_element(Record, '_'), Match, T);
create_match(Record, _, []) ->
	Record.


increment(Table, Key) ->
	mnesia:dirty_update_counter(Table, Key, 1).


run(Table, Predicate) when is_atom(Table), is_function(Predicate) ->
	QH = qlc:q([X || X <- mnesia:table(Table), Predicate(X) =:= true]),
	{atomic, Result} = mnesia:transaction(fun() -> qlc:eval(QH) end),
	Result.


dump_tables(File) ->
	ok = mnesia:dump_to_textfile(File).
