% -*- mode:erlang -*-
{application, crypto_ext, [
	{description, "Extensions to the OTP Crypto Library"},
	{vsn, "0.1.0"},
	{env, []},
	{modules, [xxtea, hkdf]},
	{applications, [kernel, stdlib]}
]}.
