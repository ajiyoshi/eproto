-module(eproto_codec).

-export([get_value/3,
		 get_msg_bin/3,
	     get_id_wt/1,
		 gen_varint/1,
		 gen_basic_value/2,
		 gen_id_wt/1,
		 make_id_wt/2,
		 get_wt_by_type/1
		]).

-include("eproto.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================

%%get basic type value except messages 
get_value(?WT_LEN, FD = #field_desc{label = ?LABEL_REPEATED}, Bin) ->
	case (FD#field_desc.type) of
		?TYPE_BYTES		-> get_value2(?WT_LEN, FD#field_desc.type, Bin);
		?TYPE_STRING	-> get_value2(?WT_LEN, FD#field_desc.type, Bin);
		?TYPE_MESSAGE	-> get_value2(?WT_LEN, FD#field_desc.type, Bin);
		_ -> % packed repeated 
			{Size, Rest} = get_varint(Bin),
			WT = get_wt_by_type(FD#field_desc.type),
			get_value1(WT, FD, Rest, Size, [])
	end;
get_value(WT, FD, Bin) ->
	get_value2(WT, FD#field_desc.type, Bin).

%%get repeated values from bytes, return list of values
get_value1(_WT, _FD, Bin, 0, Acc) ->
	{lists:reverse(Acc), Bin};
get_value1(WT, FD, Bin, Size, Acc) ->
	S1 = byte_size(Bin),
	{V, Rest} = get_value2(WT, FD#field_desc.type, Bin),
	S2 = byte_size(Rest),	
	get_value1(WT, FD, Rest, Size - (S1 - S2), [V|Acc]).


get_value2(?WT_64BIT, ?TYPE_DOUBLE, Bin) ->
	<<V:64/float, Rest/binary>> = Bin,
	{V, Rest};
get_value2(?WT_32BIT, ?TYPE_FLOAT, Bin) ->
	<<V:32/float, Rest/binary>> = Bin,
	{V, Rest};
%%TODO: process standard negtive int value
get_value2(?WT_VARINT, ?TYPE_INT64, Bin) ->
	get_varint(Bin);
get_value2(?WT_VARINT, ?TYPE_UINT64, Bin) ->
	get_varint(Bin);
get_value2(?WT_VARINT, ?TYPE_INT32, Bin) ->
	get_varint(Bin);
get_value2(?WT_64BIT, ?TYPE_FIXED64, Bin) ->
	<<V:64/integer, Rest/binary>> = Bin,
	{V, Rest};
get_value2(?WT_32BIT, ?TYPE_FIXED32, Bin) ->
	<<V:32/integer, Rest/binary>> = Bin,
	{V, Rest};
get_value2(?WT_VARINT, ?TYPE_BOOL, Bin) ->
	{V, Rest} = get_varint(Bin),
	case V of
		1 -> {true, Rest};
		0 -> {fasle, Rest}
	end;
%%just return bytes, when get string type
%%let application do character conversion
get_value2(?WT_LEN, ?TYPE_STRING, Bin) ->
	get_value2(?WT_LEN, ?TYPE_BYTES, Bin);
get_value2(?WT_LEN, ?TYPE_BYTES, Bin) ->
	{Len, Rest} = get_varint(Bin),
	<<B:Len/binary, Rest1/binary>> = Rest,
	{B, Rest1};
get_value2(?WT_VARINT, ?TYPE_UINT32, Bin) ->
	get_varint(Bin);
get_value2(?WT_VARINT, ?TYPE_ENUM, Bin) ->
	get_varint(Bin);
get_value2(?WT_32BIT, ?TYPE_SFIXED32, Bin) ->
	<<V:32/signed-integer, Rest/binary>> = Bin,
	{V, Rest};
get_value2(?WT_64BIT, ?TYPE_SFIXED64, Bin) ->
	<<V:64/signed-integer, Rest/binary>> = Bin,
	{V, Rest};
get_value2(?WT_VARINT, Type, Bin) 
  when Type == ?TYPE_SINT32 orelse Type == ?TYPE_SINT64 ->
	{V, Rest} = get_varint(Bin),
	V1 = case V rem 2 of
			 0 -> V div 2;
			 1 -> 0 - (V + 1) div 2
		 end,
	{V1, Rest}.


%%get message body, return bytes and rest bytes
get_msg_bin(_WT, _FD, Bin) ->
	get_value2(?WT_LEN, ?TYPE_BYTES, Bin).

%%get field index and wire type, return rest bytes
get_id_wt(Bin) ->
	{Bits, Rest} = get_varint_bits(Bin),
	S1 = bit_size(Bits) - 3,
	<<V1:S1, V2:3>> = Bits,
	{V1, V2,  Rest}.

get_varint(Bin) ->
	get_varint_1(Bin, <<>>, int).

get_varint_bits(Bin) ->
	get_varint_1(Bin, <<>>, bits).

get_varint_1(<<0:1, B:7, Rest/binary>>, Acc, int) ->	
	B1 = <<B:7, Acc/bits>>,
	S1 = bit_size(B1),
	<<V:S1>> = B1,
	{V, <<Rest/binary>>};	
get_varint_1(<<0:1, B:7/bits, Rest/binary>>, Acc, bits) ->
	{<<B:7/bits, Acc/bits>>, <<Rest/binary>>};
get_varint_1(<<1:1, B:7/bits, Rest/binary>>, Acc, RT) ->	
	get_varint_1(<<Rest/binary>>, <<B:7/bits, Acc/bits>>, RT).

get_wt_by_type(?TYPE_BOOL) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_BYTES) ->
	?WT_LEN;
get_wt_by_type(?TYPE_DOUBLE) ->
	?WT_64BIT;
get_wt_by_type(?TYPE_ENUM) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_FIXED32) ->
	?WT_32BIT;
get_wt_by_type(?TYPE_FIXED64) ->
	?WT_64BIT;
get_wt_by_type(?TYPE_FLOAT) ->
	?WT_32BIT;
get_wt_by_type(?TYPE_INT32) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_INT64) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_MESSAGE) ->
	?WT_LEN;
get_wt_by_type(?TYPE_SFIXED32) ->
	?WT_32BIT;
get_wt_by_type(?TYPE_SFIXED64) ->
	?WT_64BIT;
get_wt_by_type(?TYPE_SINT32) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_SINT64) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_STRING) ->
	?WT_LEN;
get_wt_by_type(?TYPE_UINT32) ->
	?WT_VARINT;
get_wt_by_type(?TYPE_UINT64) ->
	?WT_VARINT.


%%-----------------------------------
%%    Generate binary value
%%-----------------------------------
%%generate id and wire_type varint
gen_id_wt(FD) ->
	ID = FD#field_desc.number,
	WT = get_wt_by_type(FD#field_desc.type),
	make_id_wt(ID, WT).

make_id_wt(ID, WT) ->
	gen_varint(ID * 8 + WT).

%%generate varint from input value
-define(GPB_BASE, 2#10000000).
gen_varint(Num) when Num < ?GPB_BASE ->
   <<0:1, Num:7>>;
gen_varint(Num) ->
	Rem = Num rem ?GPB_BASE,
	join_bits(<<1:1, Rem:7>>, gen_varint(Num div ?GPB_BASE)).

% tail recursive version. i wonder this version is easy to understand or not.
% i think i will NEVER use very very large var int.
-ifdef(gen_varint_1_tail_recursive).
gen_varint_1(Num) when Num < ?GPB_BASE ->
	<<0:1, Num:7>>;
gen_varint_1(Num) ->
	[H|T] = radix_list(Num, [], ?GPB_BASE),
	Tails = list_to_binary( lists:reverse([X+?GPB_BASE || X <- T]) ),
	join_bits( Tails, <<H>> ).

radix_list(Num, [], Base) ->
	radix_list(Num div Base, [ Num rem Base ]);
radix_list(0, Acc, Base) ->
	Acc;
radix_list(Num, Acc, Base) ->
	radix_list(Num div Base, [ Num rem Base | Acc ]).
-endif.

-undef(GPB_BASE).

%%generate bytes according to the field type
%%when label is repeated, return packed bytes
gen_basic_value(FD, V) ->
	gen_value(FD#field_desc.number, FD#field_desc.type, V).

gen_packed_repeated(_ID, _Type, []) ->
	<<>>;
gen_packed_repeated(ID, Type, L) when is_list(L) ->
	Bin = list_to_binary( [ make_body(Type, X) || X <- L ] ),
	gen_value(ID, ?TYPE_BYTES, Bin);
gen_packed_repeated(ID, Type, V) ->
	gen_value(ID, Type, V).

gen_value(ID, Type, V) when is_list(V) ->
	list_to_binary( [ gen_value(ID, Type, X) || X <- V ] );
gen_value(ID, Type, V) ->
	Head = make_id_wt(ID, get_wt_by_type(Type)),
	Body = make_body(Type, V),
	join_bits(Head, Body).

make_body(?TYPE_INT64, V) ->
	gen_varint(V);
make_body(?TYPE_UINT64, V) ->
	gen_varint(V);
make_body(?TYPE_INT32, V) ->
	gen_varint(V);
make_body(?TYPE_UINT32, V) ->
	gen_varint(V);
make_body(?TYPE_ENUM, V) ->
	gen_varint(V);
make_body(?TYPE_BOOL, V) ->
	gen_varint(V);
make_body(?TYPE_BYTES, V) ->
	gen_length_delimited(V);
make_body(?TYPE_STRING, V) ->
	gen_length_delimited(V);
make_body(?TYPE_FIXED64, V) ->
	<<V:64/integer>>;
make_body(?TYPE_FIXED32, V) ->
	<<V:32/integer>>;
make_body(?TYPE_SFIXED64, V) ->
	<<V:64/signed-integer>>;
make_body(?TYPE_SFIXED32, V) ->
	<<V:32/signed-integer>>;
make_body(?TYPE_DOUBLE, V) ->
	<<V:64/float>>;
make_body(?TYPE_FLOAT, V) ->
	<<V:32/float>>;
make_body(?TYPE_SINT64, V) ->
	gen_zig_zag(V);
make_body(?TYPE_SINT32, V) ->
	gen_zig_zag(V);
make_body(_Type, _V) ->
	<<>>.

gen_zig_zag(V) ->
	V1 = if
		V >= 0 -> V * 2;
		V < 0 -> (0 - V) * 2 - 1
	end,
	gen_varint(V1).

gen_length_delimited(V) ->
	BLen = gen_varint(byte_size(V)),
	join_bits(BLen, V).

join_bits(L, R) ->
	<<L/bits, R/bits>>.
%%==================================================================
%% Test Functions
%%==================================================================

get_varint_test() ->
	?assertEqual(get_varint(<<150, 1, 67, 69>>), {150, <<67, 69>>}),
	?assertEqual(get_varint(<<1, 67, 69>>), {1, <<67, 69>>}),
	?assertEqual(get_varint(<<16#AC, 16#02>>), {300, <<>>}),
	ok.

gen_varint_test() ->
	?assertEqual(gen_varint(150), <<16#96, 16#01>>),
	?assertEqual(gen_varint(300), <<16#AC, 16#02>>),
	?assertEqual(gen_varint(0), <<0>>),
	?assertEqual(gen_varint(1), <<1>>),
	?assertEqual(gen_varint(2), <<2>>),
	?assertEqual(gen_varint(127), <<127>>),
	?assertEqual(gen_varint(128), <<128, 1>>),
	?assertEqual(gen_varint(129), <<129, 1>>),
	?assertEqual(gen_varint(16383), <<255, 127>>),
	?assertEqual(gen_varint(16384), <<128, 128, 1>>),
	?assertEqual(gen_varint(16385), <<129, 128, 1>>),
	?assertEqual(get_varint(gen_varint(150)), {150, <<>>}),
	?assertEqual(get_varint(gen_varint(300)), {300, <<>>}),
	?assertEqual(get_varint(gen_varint(1)), {1, <<>>}),
	?assertEqual(get_varint(gen_varint(2)), {2, <<>>}),
	?assertEqual(get_varint(gen_varint(127)), {127, <<>>}),
	?assertEqual(get_varint(gen_varint(128)), {128, <<>>}),
	?assertEqual(get_varint(gen_varint(129)), {129, <<>>}),
	?assertEqual(get_varint(gen_varint(16383)), {16383, <<>>}),
	?assertEqual(get_varint(gen_varint(16384)), {16384, <<>>}),
	?assertEqual(get_varint(gen_varint(16385)), {16385, <<>>}),

	?assertEqual(get_value2(?WT_VARINT, ?TYPE_SINT32, gen_zig_zag(-1)), {-1, <<>>}),
	?assertEqual(get_value2(?WT_VARINT, ?TYPE_SINT32, gen_zig_zag(-128)), {-128, <<>>}),
	ok.

gen_value_test() ->
	Half = 0.5,
	?assertEqual( gen_value(1, ?TYPE_DOUBLE, Half), <<1:5, ?WT_64BIT:3, Half:64/float>> ),
	?assertEqual( gen_value(2, ?TYPE_DOUBLE, Half), <<2:5, ?WT_64BIT:3, Half:64/float>> ),
	?assertEqual( gen_value(3, ?TYPE_FLOAT, Half), <<3:5, ?WT_32BIT:3, Half:32/float>> ),
	?assertEqual( gen_value(4, ?TYPE_FLOAT, Half), <<4:5, ?WT_32BIT:3, Half:32/float>> ),
	?assertEqual( gen_value(5, ?TYPE_BOOL, 0), <<5:5, ?WT_VARINT:3, 0:8>> ),
	?assertEqual( gen_value(6, ?TYPE_BOOL, 1), <<6:5, ?WT_VARINT:3, 1:8>> ),
	?assertEqual( gen_value(7, ?TYPE_UINT32, 150), <<7:5, ?WT_VARINT:3, 16#96:8, 16#01:8>> ),
	Str = <<"123">>,
	Len = byte_size(Str),
	?assertEqual( gen_value(8, ?TYPE_STRING, Str), join_bits(<<8:5, ?WT_LEN:3, Len:8>>, Str) ),
	?assertEqual( gen_value(9, ?TYPE_BYTES, Str), join_bits(<<9:5, ?WT_LEN:3, Len:8>>, Str) ),

	%repeated varint
	?assertEqual( gen_value(10, ?TYPE_UINT32, [1, 3, 150]),
		<<
		10:5, ?WT_VARINT:3, 1,
		10:5, ?WT_VARINT:3, 3,
		10:5, ?WT_VARINT:3, 16#96:8, 16#01:8
		>>
	),

	%repeated bytes
	?assertEqual( gen_value(11, ?TYPE_BYTES, [<<1, 2, 3>>, <<2, 3>>, <<4>>]),
		<<
		11:5, ?WT_LEN:3, 3:8, 1:8, 2:8, 3:8,
		11:5, ?WT_LEN:3, 2:8, 2:8, 3:8,
		11:5, ?WT_LEN:3, 1:8, 4:8
		>>
	),

	ok.

gen_repeated_test() ->
	?assertEqual( gen_packed_repeated(1, ?TYPE_INT32, []), <<>> ),
	?assertEqual( gen_packed_repeated(1, ?TYPE_INT32, [1]), <<16#0a, 1, 1>> ),
	?assertEqual( gen_packed_repeated(1, ?TYPE_INT32, [1, 2, 3, 4, 150]), <<16#0a, 6, 1, 2, 3, 4, 16#96, 16#01>> ),
	ok.

get_id_wt_test() ->
	?assertEqual(get_id_wt(<<8, 8>>), {1, 0, <<8>>}),
	?assertEqual(get_id_wt(<<129, 1, 8>>), {16, 1, <<8>>}),
	ok.

make_id_wt_test() ->
	?assertEqual(get_id_wt(make_id_wt(1, ?WT_VARINT)), {1, ?WT_VARINT, <<>>}),
	?assertEqual(get_id_wt(make_id_wt(16, ?WT_64BIT)), {16, ?WT_64BIT, <<>>}),
	ok.

get_value_test() -> 
	{V1, Rest1} = get_value(?WT_VARINT, #field_desc{type = ?TYPE_UINT32, label = ?LABEL_REPEATED}, 
		<<150, 1, 67, 69>>),
	?assertEqual({V1, Rest1}, {150, <<67, 69>>}),

	{V2, Rest2} = get_value(?WT_VARINT, #field_desc{type = ?TYPE_SINT32},
		<<150, 1, 67, 69>>),	
	?assertEqual({V2, Rest2}, {75, <<67, 69>>}),

	{V3, Rest3} = get_value1(?WT_LEN, #field_desc{type = ?TYPE_BYTES}, <<1, 2, 1, 5, 2, 4, 128>>, 7, []),
	?assertEqual({V3, Rest3}, {[<<2>>, <<5>>, <<4, 128>>], <<>>}),

	%repeated uint32 [packed=true]
	{V4, Rest4} = get_value(?WT_LEN, #field_desc{type = ?TYPE_UINT32, label = ?LABEL_REPEATED},
		<<4, 1, 2, 150, 1, 2>>),	
	?assertEqual({V4, Rest4}, {[1, 2, 150], <<2>>}),

	{V5, Rest5} = get_value(?WT_LEN, #field_desc{type = ?TYPE_BYTES},
		<<4, 1, 2, 150, 1, 2>>),	
	?assertEqual({V5, Rest5}, {<<1, 2, 150, 1>>, <<2>>}),

	%repeated fixed32 [packed=true]
	{V6, Rest6} = get_value(?WT_LEN, #field_desc{type = ?TYPE_FIXED32, label = ?LABEL_REPEATED},
		<<8, 0, 0, 0, 1, 0, 0, 0, 2>>),	
	?assertEqual({V6, Rest6}, {[1, 2], <<>>}),
	ok.

get_msg_bin_test() ->	
	{V, Rest} = get_msg_bin(?WT_LEN, #field_desc{type = ?TYPE_MESSAGE}, <<2, 1, 2, 67, 69>>),
	?assertEqual({V, Rest}, {<<1, 2>>, <<67, 69>>}),
	ok.
	
