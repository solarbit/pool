% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

% bitcoin.hrl

-ifndef(TTY).
-define(TTY(Term), io:format(user, "[~p:~p] ~p~n", [?MODULE, ?LINE, Term])).
-endif.


-define(BITCOIN_HOST, {127, 0, 0, 1}).
-define(BITCOIN_PORT, 8333).

-define(BITCOIN_MAGIC, 4190024921). % <<249, 190, 180, 217>>).
-define(BITCOIN_VERSION, 70002).

% Node Services Bit Flags
-define(NODE_NONE, 0).
-define(NODE_NETWORK, 1).
-define(NODE_GETUTXO, 2).
-define(NODE_BLOOM, 4).
-define(NODE_WITNESS, 8).
-define(NODE_XTHIN, 16).

-define(DEFAULT_TX_INPUT_SEQUENCE, 16#FFFFFFFF).

-define(INV_ERROR, 0).
-define(INV_TX, 1).
-define(INV_BLOCK, 2).
-define(INV_FILTERED_BLOCK, 3).

-define(REJECT_MALFORMED, 1).
-define(REJECT_INVALID, 16).
-define(REJECT_OBSOLETE, 17).
-define(REJECT_DUPLICATE, 18).
-define(REJECT_NONSTANDARD, 64).
-define(REJECT_DUST, 65).
-define(REJECT_INSUFFICIENTFEE, 66).
-define(REJECT_CHECKPOINT, 67).

-define(VERSION_PUBKEY_HASH, 0).
-define(VERSION_SCRIPT_HASH, 5).
-define(VERSION_PRIVATE_KEY, 128).

-define(SIG_HASH_ALL, 1).

-define(IPV4_FLAG, 65535).

% base difficulty hex: 0x00000000FFFF0000000000000000000000000000000000000000000000000000
-define(BASE_DIFFICULTY, 26959535291011309493156476344723991336010898738574164086137773096960).

% 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
-define(GENESIS_BLOCK, 10628944869218562084050143519444549580389464591454674019345556079).

% DATA
-record(btc_address, {id, public_key, wif, private_key}).
-record(btc_netaddr, {time, services = ?NODE_NONE, ip = {0, 0, 0, 0}, port = ?BITCOIN_PORT}).

-record(btc_block, {id, height, version, prev_block, merkle_root, timestamp, bits, nonce, coinbase, txns}).
-record(btc_tx, {id, block_id, version, tx_in = [], tx_out = [], lock_time = 0}).
-record(btc_tx_in, {outpoint_ref, outpoint_index, sig_script, sequence = ?DEFAULT_TX_INPUT_SEQUENCE}).
-record(btc_tx_out, {value, pk_script}).

-record(btc_utxo_key, {outpoint_ref, outpoint_index}).
-record(btc_utxo, {key, value}).

% PROTOCOL
-record(btc_version, {
	id = ?BITCOIN_VERSION,
	services = ?NODE_NETWORK,
	timestamp = dttm:now(),
	addr_recv = #btc_netaddr{},
	addr_from = #btc_netaddr{},
	nonce = 0,
	user_agent = <<>>,
	start_height = 0,
	relay = true
}).
-record(btc_verack, {}).
-record(btc_addr, {addr_list = []}).
-record(btc_inv, {vectors = []}).
-record(btc_getdata, {vectors = []}).
-record(btc_notfound, {vectors = []}).
-record(btc_getblocks, {version = ?BITCOIN_VERSION, block_locator_hashes = [], hash_stop = 0}).
-record(btc_getheaders, {version = ?BITCOIN_VERSION, block_locator_hashes = [], hash_stop = 0}).
% btc_tx - already defined
% btc_block - already defined
-record(btc_headers, {headers = []}).
-record(btc_getaddr, {}).
-record(btc_mempool, {}).
-record(btc_ping, {nonce}).
-record(btc_pong, {nonce}).
-record(btc_reject, {message, ccode, reason, data}).
-record(btc_alert, {
	version,
	relay_until,
	expiration,
	id,
	cancel,
	set_cancel,
	min_ver,
	max_ver,
	set_sub_ver,
	priority,
	comment,
	status_bar,
	reserved
}).
% BIP 0037 -> filterload, filteradd, filterclear, merkleblock
-record(btc_filterload, {}).
-record(btc_filteradd, {}).
-record(btc_filterclear, {}).
-record(btc_merkleblock, {}).
