% Copyright 2012-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(uuid).

% @ref RFC-4122
% e.g. urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6
% UUID =
%	time-low(4) "-"
%	time-mid(2) "-"
%	time-high-and-version(2) "-"
%	clock-seq-and-reserved(1) clock-seq-low(1) "-"
%	node(6)

-define(VERSION, 1).
-define(VARIANT, 2).

-export([null/0, random/0, init/0, init/1, next/0]).
-export([to_integer/1, from_integer/1, network_interfaces/0, format/1]).


null() ->
	<<0:128>>.


random() ->
	crypto:strong_rand_bytes(16).


init() ->
	case get(uuid_node) of
	undefined ->
		case network_interfaces() of
		[{_, Node}|_] ->
			init(Node);
		[] ->
			<<X, Bin/binary>> = crypto:strong_rand_bytes(6),
			% NOTE: set multicast bit
			Node = <<(X bor 1), Bin>>,
			init(Node)
		end;
	Node ->
		{ok, Node}
	end.

init(Node) ->
	case get(uuid_node) of
	Node ->
		ok;
	_ ->
		put(uuid_node, Node),
		<<Seq:16>> = crypto:strong_rand_bytes(2),
		put(uuid_clock_seq, Seq)
	end,
	{ok, Node}.


next() ->
	<<HiAndVersion:2/binary, Mid:2/binary, Low:4/binary>> = get_time(),
	X = put(uuid_clock_seq, get(uuid_clock_seq) + 1),
	Seq = <<?VARIANT:2, X:14>>,
	Node = get(uuid_node),
	list_to_binary([Low, Mid, HiAndVersion, Seq, Node]).


network_interfaces() ->
	{ok, IfAddrs} = inet:getifaddrs(),
	NetworkAddrs = [X || X <- IfAddrs, is_network_node(X)],
	[format_network_node(X) || X <- NetworkAddrs].


is_network_node({_, Opts}) ->
	proplists:get_value(netmask, Opts) =/= undefined
		andalso proplists:get_value(hwaddr, Opts) =/= undefined.


format_network_node({N, L}) ->
	{io_lib:format("~w", [N]), list_to_binary(proplists:get_value(hwaddr, L))}.


get_time() ->
	{M, S, U} = os:timestamp(),
	TS = ((((M * 1000000) + S) * 1000000) + U) * 10,
	<<?VERSION:4, TS:60>>.


to_integer(<<X:128>>) ->
	X.


from_integer(X) when is_integer(X) ->
	<<X:128>>.


format(X) when is_integer(X) ->
	format(<<X:128>>);
format(<<A:4/binary, B:2/binary, C:2/binary, D:2/binary, E:6/binary>>) ->
	format([A, B, C, D, E]);
format([H|T]) when length(T) == 4 ->
	iolist_to_binary([hex:encode(H), [[$-, hex:encode(X)] || X <- T]]).
