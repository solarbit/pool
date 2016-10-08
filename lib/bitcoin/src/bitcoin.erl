% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

-module(bitcoin).

-include("bitcoin.hrl").

-compile(export_all).


address() ->
	btc_crypto:generate_address().

address(Wif) ->
	btc_crypto:generate_address(Wif).


%% NOTE: OSX/MacOS ONLY
-define(BLOCK_DIR, os:getenv("HOME") ++ "/Library/Application Support/Bitcoin/blocks/").

extract(File) ->
	extract(File, 1).

extract(File, Limit) ->
	Path = ?BLOCK_DIR ++ File,
	case filelib:is_regular(Path) of
	true ->
		{ok, Fd} = file:open(Path, [binary, read]),
		{ok, Blocks} = extract_blocks(Fd, Limit, []),
		ok = file:close(Fd),
		{ok, Blocks};
	false ->
		{invalid_path, Path}
	end.

extract_blocks(Fd, Count, Acc) when Count > 0 ->
	case file:read(Fd, 8) of
	{ok, <<?BITCOIN_MAGIC:32, BlockSize:32/little>>} ->
		{ok, Bin} = file:read(Fd, BlockSize),
		Block = btc_codec:decode_block(Bin),
		extract_blocks(Fd, Count - 1, [Block|Acc]);
	{ok, <<0:64>>} ->
		{ok, lists:reverse(Acc)};
	{ok, Other} ->
		?TTY({unexpected, Other}),
		{ok, lists:reverse(Acc)};
	eof ->
		{ok, lists:reverse(Acc)};
	Error ->
		Error
	end;
extract_blocks(_, _, Acc) ->
	{ok, lists:reverse(Acc)}.
