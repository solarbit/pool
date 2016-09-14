% -*- mode:erlang -*-
{application, solarbit, [
	{description, "Solarbit Pool Server"},
	{vsn, "1.0.0-A"},
	{mod, {sbt_app, []}},
	{env, []},
	{modules, [solarbit, sbt_pool_srv, sbt_logger]},
	{applications, [kernel, stdlib]}
]}.
