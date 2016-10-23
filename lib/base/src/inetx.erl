-module(inetx).
-include("base.hrl").
-compile(export_all).

ip() ->
	{ok, Addrs} = inet:getifaddrs(),
	filter(Addrs).

filter([{_Name, H}|T]) ->
	List = [X || X = {K, _} <- H, K == addr andalso lists:keyfind(hwaddr, 1, H) =/= false],
	case [A || {addr, A} <- List, tuple_size(A) == 4] of
	[] ->
		filter(T);
	[Addr] ->
		{ok, Addr}
	end;
filter([]) ->
	{error, enoip}.
