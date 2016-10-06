#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    {ok, _} = net_kernel:start([pool_ctrl, shortnames]),
	[_|T] = re:split(atom_to_binary(node(), utf8), <<"(@)">>),
	Pool = binary_to_atom(iolist_to_binary([<<"solarbit">>|T]), utf8),
	rpc:call(Pool, solarbit, stop, []),
    Res = rpc:call(Pool, init, stop, []),
    io:fwrite("==> ~p~n", [Res]).
