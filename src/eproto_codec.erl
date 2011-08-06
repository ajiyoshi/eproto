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
get_value(?WT_LEN, FD, Bin) ->
	get_value2(?WT_LEN, FD#field_desc.type, Bin);
get_value(WT, FD = #field_desc{label = repeated}, Bin) ->
	{Size, Rest} = get_varint(Bin),
	get_value1(WT, FD, Rest, Size, []);	
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
gen_varint(V) ->
	gen_varint_1(V rem 128, V div 128, <<>>).

gen_varint_1(Rem, 0, <<>>) ->
	<<0:1, Rem:7>>;
gen_varint_1(Rem, Div, <<>>) ->
	gen_varint_1(Rem, Div, <<0:1, Rem:7>>);
gen_varint_1(Rem, 0, Acc) ->
	<<1:1, Rem:7, Acc>>;
gen_varint_1(Rem, Div, Acc) ->
	gen_varint_1(Rem, Div, <<1:1, Rem:7, Acc>>).

%%generate bytes according to the field type
%%when label is repeated, return packed bytes
gen_basic_value(FD, V) ->
	gen_value(FD#field_desc.number, FD#field_desc.type, V).

gen_value(ID, ?TYPE_DOUBLE, V) ->
	B = make_id_wt(ID, ?WT_64BIT),
	join_bits(B, <<V:64/float>>);	
gen_value(ID, ?TYPE_FLOAT, V) ->
	B = make_id_wt(ID, ?WT_32BIT),
	join_bits(B, <<V:32/float>>);
gen_value(ID, ?TYPE_INT64, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_UINT64, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_INT32, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_UINT32, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_ENUM, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_FIXED64, V) ->
	B = make_id_wt(ID, ?WT_64BIT),
	join_bits(B, <<V:64/integer>>);	
gen_value(ID, ?TYPE_FIXED32, V) ->
	B = make_id_wt(ID, ?WT_32BIT),
	join_bits(B, <<V:32/integer>>);	
gen_value(ID, ?TYPE_SFIXED64, V) ->
	B = make_id_wt(ID, ?WT_64BIT),
	join_bits(B, <<V:64/signed-integer>>);	
gen_value(ID, ?TYPE_SFIXED32, V) ->
	B = make_id_wt(ID, ?WT_32BIT),
	join_bits(B, <<V:32/signed-integer>>);	
gen_value(ID, ?TYPE_BOOL, V) ->
	gen_varint_value(ID, V);
gen_value(ID, ?TYPE_STRING, V) ->
	B = make_id_wt(ID, ?WT_LEN),
	BLen = gen_varint(byte_size(V)),
	join_bits(B, join_bits(BLen, V));	
gen_value(ID, ?TYPE_BYTES, V) ->
	B = make_id_wt(ID, ?WT_LEN),
	join_bits(B, V);
gen_value(ID, Type, V) 
  when Type == ?TYPE_SINT32 orelse Type == ?TYPE_SINT64 ->
	gen_zig_zag_value(ID, V).

gen_zig_zag_value(ID, V) ->
	V1 = if
			V >= 0 -> V * 2;
			V < 0 -> (0 - V) * 2 - 1
		end,
	gen_varint_value(ID, V1).

gen_varint_value(ID, V) ->
	B = make_id_wt(ID, ?WT_VARINT),
	join_bits(B, gen_varint(V)).

join_bits(L, R) ->
	<<L/bits, R/bits>>.
%%==================================================================
%% Test Functions
%%==================================================================

get_varint_test() ->
	?assert(get_varint(<<150, 1, 67, 69>>) == {150, <<67, 69>>}),
	?assert(get_varint(<<1, 67, 69>>) == {1, <<67, 69>>}),
	io:format("get_varint_test -- testing done~n").

get_id_wt_test() ->
	?assert(get_id_wt(<<8, 8>>) == {1, 0, <<8>>}),
	?assert(get_id_wt(<<129, 1, 8>>) == {16, 1, <<8>>}),
	io:format("get_id_wt_test -- testing done~n").

get_value_test() -> 
	{V, Rest} = get_value(?WT_VARINT, #field_desc{label = repeated, type = ?TYPE_UINT32}, 
						  <<5, 25, 26, 150, 1, 27, 67, 69>>),
	?assert({V, Rest} == {[25, 26, 150, 27], <<67, 69>>}),
	{V1, Rest1} = get_value(?WT_VARINT, #field_desc{type = ?TYPE_SINT32}, <<150, 1, 67, 69>>),	
	?assert({V1, Rest1} == {75, <<67, 69>>}),
	io:format("get_value_test -- testing done~n").

get_msg_bin_test() ->	
	{V, Rest} = get_msg_bin(?WT_LEN, #field_desc{type = ?TYPE_MESSAGE}, <<2, 1, 2, 67, 69>>),
	?assert({V, Rest} == {<<1, 2>>, <<67, 69>>}),
	io:format("get_msg_bin_test -- testing done~n").
	