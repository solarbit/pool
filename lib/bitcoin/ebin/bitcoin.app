% -*- mode:erlang -*-
{application, bitcoin, [
	{description, "Bitcoin Support Library"},
	{vsn, "0.5.0"},
	{env, []},
	{modules, [bitcoin, btc_codec, btc_crypto, btc_protocol, btc_script]},
	{applications, [kernel, stdlib]}
]}.
