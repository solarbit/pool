
-module(inet_ext).

-include("base.hrl").

-export([local_ip/0, parse_ip/1]).

-compile(export_all).


local_ip() ->
	{ok, Addrs} = inet:getifaddrs(),
	local_ip0(Addrs).

local_ip0([{_Name, H}|T]) ->
	List = [X || X = {K, _} <- H, K == addr andalso lists:keyfind(hwaddr, 1, H) =/= false],
	case [A || {addr, A} <- List, tuple_size(A) == 4] of
	[] ->
		local_ip0(T);
	[Addr] ->
		{ok, Addr}
	end;
local_ip0([]) ->
	{error, enoip}.


parse_ip(String) when is_binary(String) orelse is_list(String) ->
	list_to_tuple([binary_to_integer(X) || X <- re:split(String, <<"\\.">>)]).
