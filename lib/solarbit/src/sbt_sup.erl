-module(sbt_sup).

-include("solarbit.hrl").

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, [[]]}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {{one_for_one, 0, 1}, [
		?CHILD(sbt_pool_srv, worker),
		?CHILD(sbt_log_srv, worker)
	] }}.
