% Copyright 2016 solarbit.cc <steve@solarbit.cc>
% See MIT LICENSE

-module(bitcoin).

-include("bitcoin.hrl").

-compile(export_all).


address() ->
	btc_crypto:generate_address().

address(Wif) ->
	btc_crypto:generate_address(Wif).


-define(BLOCK_DIR, "/Users/stevedavis/Library/Application Support/Bitcoin/blocks/").


extract(File) ->
	extract(File, 1).

extract(File, Limit) ->
	Path = ?BLOCK_DIR ++ File,
	?TTY(Path),
	{ok, Fd} = file:open(Path, [binary, read]),
	{ok, Blocks} = extract_blocks(Fd, ?BITCOIN_MAGIC, 1, Limit, []),
	ok = file:close(Fd),
	{ok, Blocks}.


extract_blocks(Fd, Magic, Count, Max, Acc) when Count =< Max ->
	case file:read(Fd, 8) of
	{ok, <<Magic:4/binary, BlockSize:32/little>>} ->
		{ok, Bin} = file:read(Fd, BlockSize),
		Block = btc_codec:decode_block(Bin),
		extract_blocks(Fd, Magic, Count + 1, Max, [Block|Acc]);
	{ok, <<0:64>>} ->
		{ok, lists:reverse(Acc)};
	eof ->
		{ok, lists:reverse(Acc)};
	Error ->
		{error, Error}
	end;
extract_blocks(_, _, _, _, Acc) ->
	{ok, lists:reverse(Acc)}.
