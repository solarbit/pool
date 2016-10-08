% Copyright 2014-2016 Steve Davis <steve@solarbit.cc>
% See MIT LICENSE

% bitcoin_script.hrl
%@ref: https://en.bitcoin.it/wiki/Script

% True=1 and False=0.

% --Constants--
% When talking about scripts, these value-pushing words are usually omitted.
% Word	Opcode	Hex	Input	Output	Description
% OP_0, OP_FALSE	0	0x00	Nothing.	(empty value)	An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
-define(OP_0, 0).
-define(OP_FALSE, 0).
% N/A	1-75	0x01-0x4b	(special)	data	The next opcode bytes is data to be pushed onto the stack
% OP_PUSHDATA1	76	0x4c	(special)	data	The next byte contains the number of bytes to be pushed onto the stack.
-define(OP_PUSHDATA, 76).
% OP_PUSHDATA2	77	0x4d	(special)	data	The next two bytes contain the number of bytes to be pushed onto the stack.
-define(OP_PUSHDATA2, 77).
% OP_PUSHDATA4	78	0x4e	(special)	data	The next four bytes contain the number of bytes to be pushed onto the stack.
-define(OP_PUSHDATA4, 78).
% OP_1NEGATE	79	0x4f	Nothing.	 -1	The number -1 is pushed onto the stack.
-define(OP_1NEGATE, 79).
% OP_1, OP_TRUE	81	0x51	Nothing.	1	The number 1 is pushed onto the stack.
-define(OP_1, 81).
-define(OP_TRUE, 81).
% OP_2-OP_16	82-96	0x52-0x60	Nothing.	2-16	The number in the word name (2-16) is pushed onto the stack.
-define(OP_2, 82).
-define(OP_3, 83).
-define(OP_4, 84).
-define(OP_5, 85).
-define(OP_6, 86).
-define(OP_7, 87).
-define(OP_8, 88).
-define(OP_9, 89).
-define(OP_10, 90).
-define(OP_11, 91).
-define(OP_12, 92).
-define(OP_13, 93).
-define(OP_14, 94).
-define(OP_15, 95).
-define(OP_16, 96).

% --Flow control--
% Word	Opcode	Hex	Input	Output	Description
% OP_NOP	97	0x61	Nothing	Nothing	Does nothing.
-define(OP_NOP, 97).
% OP_IF	99	0x63	<expression> if [statements] [else [statements]]* endif	If the top stack value is not 0, the statements are executed. The top stack value is removed.
-define(OP_IF, 99).
% OP_NOTIF	100	0x64	<expression> if [statements] [else [statements]]* endif	If the top stack value is 0, the statements are executed. The top stack value is removed.
-define(OP_NOTIF, 100).
% OP_ELSE	103	0x67	<expression> if [statements] [else [statements]]* endif	If the preceding OP_IF or OP_NOTIF or OP_ELSE was not executed then these statements are and if the preceding OP_IF or OP_NOTIF or OP_ELSE was executed then these statements are not.
-define(OP_ELSE, 103).
% OP_ENDIF	104	0x68	<expression> if [statements] [else [statements]]* endif	Ends an if/else block.
-define(OP_ENDIF, 104).
% OP_VERIFY	105	0x69	True / false	Nothing / False	Marks transaction as invalid if top stack value is not true. True is removed, but false is not.
-define(OP_VERIFY, 105).
% OP_RETURN	106	0x6a	Nothing	Nothing	Marks transaction as invalid.
-define(OP_RETURN, 106).

% --Stack--
% Word	Opcode	Hex	Input	Output	Description
% OP_TOALTSTACK	107	0x6b	x1	(alt)x1	Puts the input onto the top of the alt stack. Removes it from the main stack.
-define(OP_TOALTSTACK, 107).
% OP_FROMALTSTACK	108	0x6c	(alt)x1	x1	Puts the input onto the top of the main stack. Removes it from the alt stack.
-define(OP_FROMALTSTACK, 108).
% OP_2DROP	109	0x6d	x1 x2	Nothing	Removes the top two stack items.
-define(OP_2DROP, 109).
% OP_2DUP	110	0x6e	x1 x2	x1 x2 x1 x2	Duplicates the top two stack items.
-define(OP_2DUP, 110).
% OP_3DUP	111	0x6f	x1 x2 x3	x1 x2 x3 x1 x2 x3	Duplicates the top three stack items.
-define(OP_3DUP, 111).
% OP_2OVER	112	0x70	x1 x2 x3 x4	x1 x2 x3 x4 x1 x2	Copies the pair of items two spaces back in the stack to the front.
-define(OP_2OVER, 112).
% OP_2ROT	113	0x71	x1 x2 x3 x4 x5 x6	x3 x4 x5 x6 x1 x2	The fifth and sixth items back are moved to the top of the stack.
-define(OP_2ROT, 113).
% OP_2SWAP	114	0x72	x1 x2 x3 x4	x3 x4 x1 x2	Swaps the top two pairs of items.
-define(OP_2SWAP, 114).
% OP_IFDUP	115	0x73	x	x / x x	If the top stack value is not 0, duplicate it.
-define(OP_IFDUP, 115).
% OP_DEPTH	116	0x74	Nothing	<Stack size>	Puts the number of stack items onto the stack.
-define(OP_DEPTH, 116).
% OP_DROP	117	0x75	x	Nothing	Removes the top stack item.
-define(OP_DROP, 117).
% OP_DUP	118	0x76	x	x x	Duplicates the top stack item.
-define(OP_DUP, 118).
% OP_NIP	119	0x77	x1 x2	x2	Removes the second-to-top stack item.
-define(OP_NIP, 119).
% OP_OVER	120	0x78	x1 x2	x1 x2 x1	Copies the second-to-top stack item to the top.
-define(OP_OVER, 120).
% OP_PICK	121	0x79	xn ... x2 x1 x0 <n>	xn ... x2 x1 x0 xn	The item n back in the stack is copied to the top.
-define(OP_PICK, 121).
% OP_ROLL	122	0x7a	xn ... x2 x1 x0 <n>	... x2 x1 x0 xn	The item n back in the stack is moved to the top.
-define(OP_ROLL, 122).
% OP_ROT	123	0x7b	x1 x2 x3	x2 x3 x1	The top three items on the stack are rotated to the left.
-define(OP_ROT, 123).
% OP_SWAP	124	0x7c	x1 x2	x2 x1	The top two items on the stack are swapped.
-define(OP_SWAP, 124).
% OP_TUCK	125	0x7d	x1 x2	x2 x1 x2	The item at the top of the stack is copied and inserted before the second-to-top item.
-define(OP_TUCK, 125).


% --Splice--
% If any opcode marked as disabled is present in a script, it must abort and fail.
% Word	Opcode	Hex	Input	Output	Description
% OP_CAT	126	0x7e	x1 x2	out	Concatenates two strings. disabled.
-define(OP_CAT, 126).
% OP_SUBSTR	127	0x7f	in begin size	out	Returns a section of a string. disabled.
-define(OP_SUBSTR, 127).
% OP_LEFT	128	0x80	in size	out	Keeps only characters left of the specified point in a string. disabled.
-define(OP_LEFT, 128).
% OP_RIGHT	129	0x81	in size	out	Keeps only characters right of the specified point in a string. disabled.
-define(OP_RIGHT, 129).
% OP_SIZE	130	0x82	in	in size	Pushes the string length of the top element of the stack (without popping it).
-define(OP_SIZE, 130).

% --Bitwise logic--
% If any opcode marked as disabled is present in a script, it must abort and fail.
% Word	Opcode	Hex	Input	Output	Description
% OP_INVERT	131	0x83	in	out	Flips all of the bits in the input. disabled.
-define(OP_INVERT, 131).
% OP_AND	132	0x84	x1 x2	out	Boolean and between each bit in the inputs. disabled.
-define(OP_AND, 132).
% OP_OR	133	0x85	x1 x2	out	Boolean or between each bit in the inputs. disabled.
-define(OP_OR, 133).
% OP_XOR	134	0x86	x1 x2	out	Boolean exclusive or between each bit in the inputs. disabled.
-define(OP_XOR, 134).
% OP_EQUAL	135	0x87	x1 x2	True / false	Returns 1 if the inputs are exactly equal, 0 otherwise.
-define(OP_EQUAL, 135).
% OP_EQUALVERIFY	136	0x88	x1 x2	True / false	Same as OP_EQUAL, but runs OP_VERIFY afterward.
-define(OP_EQUALVERIFY, 136).

% --Arithmetic--
% Note: Arithmetic inputs are limited to signed 32-bit integers, but may overflow their output.
% If any input value for any of these commands is longer than 4 bytes, the script must abort and fail. If any opcode marked as disabled is present in a script - it must also abort and fail.
% Word	Opcode	Hex	Input	Output	Description
% OP_1ADD	139	0x8b	in	out	1 is added to the input.
-define(OP_1ADD, 139).
% OP_1SUB	140	0x8c	in	out	1 is subtracted from the input.
-define(OP_1SUB, 140).
% OP_2MUL	141	0x8d	in	out	The input is multiplied by 2. disabled.
-define(OP_2MUL, 141).
% OP_2DIV	142	0x8e	in	out	The input is divided by 2. disabled.
-define(OP_2DIV, 142).
% OP_NEGATE	143	0x8f	in	out	The sign of the input is flipped.
-define(OP_NEGATE, 143).
% OP_ABS	144	0x90	in	out	The input is made positive.
-define(OP_ABS, 144).
% OP_NOT	145	0x91	in	out	If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
-define(OP_NOT, 145).
% OP_0NOTEQUAL	146	0x92	in	out	Returns 0 if the input is 0. 1 otherwise.
-define(OP_0NOTEQUAL, 146).
% OP_ADD	147	0x93	a b	out	a is added to b.
-define(OP_ADD, 147).
% OP_SUB	148	0x94	a b	out	b is subtracted from a.
-define(OP_SUB, 148).
% OP_MUL	149	0x95	a b	out	a is multiplied by b. disabled.
-define(OP_MUL, 149).
% OP_DIV	150	0x96	a b	out	a is divided by b. disabled.
-define(OP_DIV, 150).
% OP_MOD	151	0x97	a b	out	Returns the remainder after dividing a by b. disabled.
-define(OP_MOD, 151).
% OP_LSHIFT	152	0x98	a b	out	Shifts a left b bits, preserving sign. disabled.
-define(OP_LSHIFT, 152).
% OP_RSHIFT	153	0x99	a b	out	Shifts a right b bits, preserving sign. disabled.
-define(OP_RSHIFT, 153).
% OP_BOOLAND	154	0x9a	a b	out	If both a and b are not 0, the output is 1. Otherwise 0.
-define(OP_BOOLAND, 154).
% OP_BOOLOR	155	0x9b	a b	out	If a or b is not 0, the output is 1. Otherwise 0.
-define(OP_BOOLOR, 155).
% OP_NUMEQUAL	156	0x9c	a b	out	Returns 1 if the numbers are equal, 0 otherwise.
-define(OP_NUMEQUAL, 156).
% OP_NUMEQUALVERIFY	157	0x9d	a b	out	Same as OP_NUMEQUAL, but runs OP_VERIFY afterward.
-define(OP_NUMEQUALVERIFY, 157).
% OP_NUMNOTEQUAL	158	0x9e	a b	out	Returns 1 if the numbers are not equal, 0 otherwise.
-define(OP_NUMNOTEQUAL, 158).
% OP_LESSTHAN	159	0x9f	a b	out	Returns 1 if a is less than b, 0 otherwise.
-define(OP_LESSTHAN, 159).
% OP_GREATERTHAN	160	0xa0	a b	out	Returns 1 if a is greater than b, 0 otherwise.
-define(OP_GREATERTHAN, 160).
% OP_LESSTHANOREQUAL	161	0xa1	a b	out	Returns 1 if a is less than or equal to b, 0 otherwise.
-define(OP_LESSTHANOREQUAL, 161).
% OP_GREATERTHANOREQUAL	162	0xa2	a b	out	Returns 1 if a is greater than or equal to b, 0 otherwise.
-define(OP_GREATERTHANOREQUAL, 162).
% OP_MIN	163	0xa3	a b	out	Returns the smaller of a and b.
-define(OP_MIN, 163).
% OP_MAX	164	0xa4	a b	out	Returns the larger of a and b.
-define(OP_MAX, 164).
% OP_WITHIN	165	0xa5	x min max	out	Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
-define(OP_WITHIN, 165).

% --Crypto--
% Word	Opcode	Hex	Input	Output	Description
% OP_RIPEMD160	166	0xa6	in	hash	The input is hashed using RIPEMD-160.
-define(OP_RIPEMD160, 166).
% OP_SHA1	167	0xa7	in	hash	The input is hashed using SHA-1.
-define(OP_SHA1, 167).
% OP_SHA256	168	0xa8	in	hash	The input is hashed using SHA-256.
-define(OP_SHA256, 168).
% OP_HASH160	169	0xa9	in	hash	The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
-define(OP_HASH160, 169).
% OP_HASH256	170	0xaa	in	hash	The input is hashed two times with SHA-256.
-define(OP_HASH256, 170).
% OP_CODESEPARATOR	171	0xab	Nothing	Nothing	All of the signature checking words will only match signatures to the data after the most recently-executed OP_CODESEPARATOR.
-define(OP_CODESEPARATOR, 171).
% OP_CHECKSIG	172	0xac	sig pubkey	True / false	The entire transaction's outputs, inputs, and script (from the most recently-executed OP_CODESEPARATOR to the end) are hashed. The signature used by OP_CHECKSIG must be a valid signature for this hash and public key. If it is, 1 is returned, 0 otherwise.
-define(OP_CHECKSIG, 172).
% OP_CHECKSIGVERIFY	173	0xad	sig pubkey	True / false	Same as OP_CHECKSIG, but OP_VERIFY is executed afterward.
-define(OP_CHECKSIGVERIFY, 173).
% OP_CHECKMULTISIG	174	0xae	x sig1 sig2 ... <number of signatures> pub1 pub2 <number of public keys>	True / False	For each signature and public key pair, OP_CHECKSIG is executed. If more public keys than signatures are listed, some key/sig pairs can fail. All signatures need to match a public key. If all signatures are valid, 1 is returned, 0 otherwise. Due to a bug, one extra unused value is removed from the stack.
-define(OP_CHECKMULTISIG, 174).
% OP_CHECKMULTISIGVERIFY	175	0xaf	x sig1 sig2 ... <number of signatures> pub1 pub2 ... <number of public keys>	True / False	Same as OP_CHECKMULTISIG, but OP_VERIFY is executed afterward.
-define(OP_CHECKMULTISIGVERIFY, 175).

% --Pseudo-words--
% These words are used internally for assisting with transaction matching. They are invalid if used in actual scripts.
% Word	Opcode	Hex	Description
% OP_PUBKEYHASH	253	0xfd	Represents a public key hashed with OP_HASH160.
-define(OP_PUBKEYHASH, 253).
% OP_PUBKEY	254	0xfe	Represents a public key compatible with OP_CHECKSIG.
-define(OP_PUBKEY, 254).
% OP_INVALIDOPCODE	255	0xff	Matches any opcode that is not yet assigned.
-define(OP_INVALIDOPCODE, 255).

% --Reserved words--
% Any opcode not assigned is also reserved. Using an unassigned opcode makes the transaction invalid.
% Word	Opcode	Hex	When used...
% OP_RESERVED	80	0x50	Transaction is invalid unless occuring in an unexecuted OP_IF branch
-define(OP_RESERVED, 80).
% OP_VER	98	0x62	Transaction is invalid unless occuring in an unexecuted OP_IF branch
-define(OP_VER, 98).
% OP_VERIF	101	0x65	Transaction is invalid even when occuring in an unexecuted OP_IF branch
-define(OP_VERIF, 101).
% OP_VERNOTIF	102	0x66	Transaction is invalid even when occuring in an unexecuted OP_IF branch
-define(OP_VERNOTIF, 102).
% OP_RESERVED1	137	0x89	Transaction is invalid unless occuring in an unexecuted OP_IF branch
-define(OP_RESERVED1, 137).
% OP_RESERVED2	138	0x8a	Transaction is invalid unless occuring in an unexecuted OP_IF branch
-define(OP_RESERVED2, 138).
% OP_NOP1-OP_NOP10	176-185	0xb0-0xb9	The word is ignored.
-define(OP_NOP1, 176).
-define(OP_NOP2, 177).
-define(OP_NOP3, 178).
-define(OP_NOP4, 179).
-define(OP_NOP5, 180).
-define(OP_NOP6, 181).
-define(OP_NOP7, 182).
-define(OP_NOP8, 183).
-define(OP_NOP9, 184).
-define(OP_NOP10, 185).

-define(OPCODES, [
		{0, op_false},
		% size = 1-75 -> PUSHDATA(size)
		{76, op_pushdata}, % size in next byte
		{77, op_pushdata2}, % size in next two bytes
		{78, op_pushdata4}, % size in next four bytes
		{79, op_1negate},
		{80, op_reserved},
		{81, op_true},
		{82, op_2}, % PUSHDATA(2)...
		{83, op_3},
		{84, op_4},
		{85, op_5},
		{86, op_6},
		{87, op_7},
		{88, op_8},
		{89, op_9},
		{90, op_10},
		{91, op_11},
		{92, op_12},
		{93, op_13},
		{94, op_14},
		{95, op_15},
		{96, op_16}, % ...PUSHDATA(16)
		{97, op_nop},
		{98, op_ver},
		{99, op_if},
		{100, op_notif},
		{101, op_verif},
		{102, op_vernotif},
		{103, op_else},
		{104, op_endif},
		{105, op_verify},
		{106, op_return},
		{107, op_toaltstack},
		{108, op_fromaltstack},
		{109, op_2drop},
		{110, op_2dup},
		{111, op_3dup},
		{112, op_2over},
		{113, op_2rot},
		{114, op_2swap},
		{115, op_ifdup},
		{116, op_depth},
		{117, op_drop},
		{118, op_dup},
		{119, op_nip},
		{120, op_over},
		{121, op_pick},
		{122, op_roll},
		{123, op_rot},
		{124, op_swap},
		{125, op_tuck},
		{126, op_cat},
		{127, op_substr},
		{128, op_left},
		{129, op_right},
		{130, op_size},
		{131, op_invert},
		{132, op_and},
		{133, op_or},
		{134, op_xor},
		{135, op_equal},
		{136, op_equalverify},
		{137, op_reserved1},
		{138, op_reserved2},
		{139, op_1add},
		{140, op_1sub},
		{141, op_2mul},
		{142, op_2div},
		{143, op_negate},
		{144, op_abs},
		{145, op_not},
		{146, op_0notequal},
		{147, op_add},
		{148, op_sub},
		{149, op_mul},
		{150, op_div},
		{151, op_mod},
		{152, op_lshift},
		{153, op_rshift},
		{154, op_booland},
		{155, op_boolor},
		{156, op_numequal},
		{157, op_numequalverify},
		{158, op_numnotequal},
		{159, op_lessthan},
		{160, op_greaterthan},
		{161, op_lessthanorequal},
		{162, op_greaterthanorequal},
		{163, op_min},
		{164, op_max},
		{165, op_within},
		{166, op_ripemd160},
		{167, op_sha1},
		{168, op_sha256},
		{169, op_hash160},
		{170, op_hash256},
		{171, op_codeseparator},
		{172, op_checksig},
		{173, op_checksigverify},
		{174, op_checkmultisig},
		{175, op_checkmultisigverify},
		{176, op_nop1},
		{177, op_nop2},
		{178, op_nop3},
		{179, op_nop4},
		{180, op_nop5},
		{181, op_nop6},
		{182, op_nop7},
		{183, op_nop8},
		{184, op_nop9},
		{185, op_nop10}
	]).
