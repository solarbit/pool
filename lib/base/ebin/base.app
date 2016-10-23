% -*- mode:erlang -*-
{application, base, [
	{description, "Basic Utility Function Library"},
	{vsn, "0.1.0"},
	{env, []},
	{modules, [base58, db, dttm, inetx, hex, json, uuid]},
	{applications, [kernel, stdlib]}
]}.
