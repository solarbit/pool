% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See LICENSE

-module(sbt_sup).

-include("solarbit.hrl").

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SUPERVISOR(I), {I, {I, start_link, [[]]}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I), {I, {I, start_link, [[]]}, permanent, 5000, worker, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{one_for_one, 0, 1}, [
		?WORKER(sbt_log_srv),
		?SUPERVISOR(sbt_db_srv),
		?WORKER(sbt_btc_srv),
		?WORKER(sbt_pool_srv)
	] }}.
