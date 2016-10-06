% Copyright 2014-2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-module(json).

% @ref RFC-4627
% @ref EEP-18 <http://www.erlang.org/eeps/eep-0018.html>

-include("base.hrl").

% -behaviour(codec).
-export([init/1, encode/1, decode/1]).

-export([parse_records/3]).

-define(JSON_REGEX, <<"[ \t\r\n]+|[,]|([\\[\\]{}:])|(\"[^\"]*\")">>).

-compile(export_all).


init(_Opts) ->
	{codec, fun(X) -> encode(X, []) end, fun (X) -> decode(X, <<>>) end}.


encode(X, _Acc) ->
	encode(X).


decode(X, _Bin) ->
	decode(X).


encode(Term) ->
	List = encode_value(Term),
	iolist_to_binary(List).


encode_value(true) ->
	<<"true">>;
encode_value(false) ->
	<<"false">>;
encode_value(null) ->
	<<"null">>;
encode_value(V) when is_integer(V); is_float(V) ->
	text:encode(V);
encode_value(V) when is_atom(V); is_binary(V) ->
	encode_string(V);
encode_value(V) when is_map(V) ->
	encode_object(V);
encode_value(V) when is_list(V) ->
	encode_array(V).


encode_string(K) when is_atom(K) ->
	[$", atom_to_binary(K, utf8), $"];
encode_string(K) when is_list(K) ->
	[$", K, $"];
encode_string(K) when is_binary(K) ->
	K0 = text:replace(K, <<"\"">>, <<"\\\\\"">>),
	[$", K0, $"].


encode_object(Map) ->
	KV = maps:to_list(Map),
	Pairs = [[encode_string(K), $:, encode_value(V)] || {K, V} <- KV],
	Pairs0 = interleave(Pairs, $,),
	[${, Pairs0, $}].


encode_array(List) ->
	Array = [encode_value(X) || X <- List],
	Array0 = interleave(Array, $,),
	[$[, Array0, $]].


interleave(Items, Separator) ->
	interleave(Items, Separator, []).

interleave([], _, Acc) ->
	lists:reverse(Acc);
interleave([H], _, Acc) ->
	lists:reverse([H|Acc]);
interleave([H|T], S, Acc) ->
	interleave(T, S, [S, H|Acc]).


decode(Bin) when is_binary(Bin) ->
	Tokens = text:split(Bin, ?JSON_REGEX),
	{Term, []} = decode_value(Tokens),
	Term.


decode_value([<<"true">>|T]) ->
	{true, T};
decode_value([<<"false">>|T]) ->
	{false, T};
decode_value([<<"null">>|T]) ->
	{null, T};
decode_value([<<$[>>|T]) ->
	decode_array(T, []);
decode_value(V = [<<${>>|_]) ->
	decode_object(V);
decode_value([H = <<$", _/binary>>|T]) ->
	{text:unquote(H), T};
decode_value([H|T]) ->
	{text:to_number(H), T}.


decode_array([<<$]>>|T], Acc) ->
	{lists:reverse(Acc), T};
decode_array(L, Acc) ->
	{V, T0} = decode_value(L),
	decode_array(T0, [V|Acc]).


decode_object([<<${>>|L]) ->
	decode_object(L, #{}).

decode_object([<<$}>>|T], Map) ->
	{Map, T};
decode_object([K, <<$:>>|T], Map) ->
	K0 = text:unquote(K),
	{V, T0} = decode_value(T),
	decode_object(T0, maps:put(K0, V, Map)).


parse_records(RecordName, Keys, List) ->
	parse_records(RecordName, Keys, List, []).

parse_records(RecordName, Keys, [H|T], Acc) when is_tuple(H) ->
	[RecordName|Values] = tuple_to_list(H),
	Object = parse_record(Keys, Values, []),
	parse_records(RecordName, Keys, T, [Object|Acc]);
parse_records(_, _, [], Acc) ->
	lists:reverse(Acc).

parse_record([K|KT], [V|VT], Acc) when ?is_string(V) ->
	parse_record(KT, VT, [{K, list_to_binary(V)}|Acc]);
parse_record([K|KT], [V|VT], Acc) when is_tuple(V) ->
	parse_record(KT, VT, [{K, tuple_to_list(V)}|Acc]);
parse_record([K|KT], [V|VT], Acc) ->
	parse_record(KT, VT, [{K, V}|Acc]);
parse_record([], [], Acc) ->
	lists:reverse(Acc).


from_xml(X) ->
	from_xml(X, []).

from_xml([{E, A, C}|T], Acc) ->
	A0 = maps:from_list(A),
	M = #{element => E, attributes => A0, content => C},
	from_xml(T, [M|Acc]);
from_xml([], Acc) ->
	encode(lists:reverse(Acc)).
