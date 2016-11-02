% -*- mode:erlang -*-
{application, solarbit, [
	{description, "Solarbit Pool Server"},
	{vsn, "0.5.0-A"},
	{mod, {sbt_app, []}},
	{env, []},
	{modules, [sbt_app, sbt_btc_srv, sbt_codec, sbt_db_srv, sbt_log_srv, sbt_pool_srv, sbt_sup, solarbit]},
	{applications, [kernel, stdlib]}
]}.
