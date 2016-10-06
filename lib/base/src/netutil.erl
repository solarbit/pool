% Copyright 2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(netutil).
% net, inet etc taken by OTP

-export([get_local_ip/0]).


get_local_ip() ->
	{ok, L} = inet:getif(),
	[IP] = [X || {X, _, _} <- L, X =/= {127, 0, 0, 1}],
	IP.
