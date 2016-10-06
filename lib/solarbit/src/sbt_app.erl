% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-module(sbt_app).

-include("solarbit.hrl").

-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).


start(normal, []) ->
    sbt_sup:start_link().


prep_stop(State) ->
	% notify miners...?
	State.


stop(_State) ->
    ok.
