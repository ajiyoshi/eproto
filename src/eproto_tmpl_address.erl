-module(eproto_tmpl_address).

-export([encode/2,
		 decode/2]).

-record(person, {name, id, email, phone}).
-record(person_phonenumber,{number, type}).

-include("eproto.hrl").
-include_lib("eunit/include/eunit.hrl").
%%====================================================================
%% API
%%====================================================================

%%encode tuple into binary
encode(MT, Rec) ->
	[_|Values] = tuple_to_list(Rec),
	Fields = get_record_info(MT),
	encode_fd(<<>>, person, Values, Fields, <<>>).

%%decode binary into tuple
decode(MT, Bin) ->
	{FD, WT, RBin} = get_next_fd(MT, Bin),
	preserve_order(decode_fd(person, FD, WT, RBin, get_init_msg(MT))).

%%====================================================================
%% Specialized Functions
%%====================================================================

decode_fd(MT = person, F = #field_desc{name=name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#person{name = V} end);

decode_fd(MT = person, F = #field_desc{name=id}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#person{id = V} end);

decode_fd(MT = person, F = #field_desc{name=email}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#person{email = V} end);

decode_fd(MT = person, F = #field_desc{name=phone}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#person{phone = update_repeated_fd(R#person.phone, V)} end);

decode_fd(MT = person_phonenumber, F = #field_desc{name=number}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#person_phonenumber{number = V} end);

decode_fd(MT = person_phonenumber, F = #field_desc{name=type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#person_phonenumber{type = V} end).

%%get field description by id

get_fd(person, N) when N==1 orelse N==name ->
	#field_desc{number=1, name=name, label = ?LABEL_REQUIRED, type=?TYPE_STRING};
get_fd(person, N) when N==2 orelse N==id ->
	#field_desc{number=2, name=id, label = ?LABEL_REQUIRED, type=?TYPE_INT32};
get_fd(person, N) when N==3 orelse N==email ->
	#field_desc{number=3, name=email, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(person, N) when N==4 orelse N==phone ->
	#field_desc{number=4, name=phone, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=person_phonenumber};
get_fd(person_phonenumber, N) when N==1 orelse N==number ->
	#field_desc{number=1, name=number, label = ?LABEL_REQUIRED, type=?TYPE_STRING};
get_fd(person_phonenumber, N) when N==2 orelse N== type ->
	#field_desc{number=2, name=type, label = ?LABEL_OPTIONAL, type=?TYPE_ENUM}.

get_init_msg(person) ->
	#person{};
get_init_msg(person_phonenumber) ->
	#person_phonenumber{}.

%%list was reversed during decode process, so reverse
%%to preserv order
preserve_order(R= #person{phone = V}) when is_list(V)->
	R#person{phone = lists:reverse(V)};
preserve_order(R) ->
	R.

get_record_info(person) ->
	 record_info(fields, person);
get_record_info(person_phonenumber) ->
	 record_info(fields, person_phonenumber).
%%====================================================================
%% Common Codec Functions
%%====================================================================
	
update_repeated_fd(undefined, V) ->
	[V];
update_repeated_fd(Orig, V) ->
	[V|Orig].	

%%get current field value from bytes,
%%recursively to decode next field
get_fd_value(MT, WT, FD, Bin, Fun) ->		
	%%when field type is Message, change the MT para
	case FD#field_desc.type of
		?TYPE_MESSAGE -> 
			{V, Bin1} = get_msg_value(FD#field_desc.type_name, WT, FD, Bin);			
		_ ->
			{V, Bin1} = eproto_codec:get_value(WT, FD, Bin)
	end,
	%%preserve order when each message decoded
	R2 = Fun(preserve_order(V)),
	get_next_fd_value(MT, Bin1, R2).
	
get_next_fd_value(MT, Bin, R) ->
	case get_next_fd(MT, Bin) of 
		undefined -> 
			R;
		{FD, WT, Bin2} ->
			decode_fd(MT, FD, WT, Bin2, R)
	end.
	
get_msg_value(MT, WT, FD, Bin) ->
	{Bin1, RBin} = eproto_codec:get_msg_bin(WT, FD, Bin),
	{get_next_fd_value(MT, Bin1, get_init_msg(MT)), RBin}.
		

%%get next field description
get_next_fd(_, <<>>) ->
	undefined;
get_next_fd(MT, Bin) ->
	{ID, WT, RBin} = eproto_codec:get_id_wt(Bin),
	{get_fd(MT, ID), WT, RBin}.

%%-----------------------------------------------------------------------

encode_fd(Head, _MT, [], [], Acc) ->
	BSize = eproto_codec:gen_varint(erlang:size(Acc)),	
	<<Head/binary, BSize/binary, Acc/binary>>;
encode_fd(<<>>, _MT, [], [], Acc) ->
	Acc;
encode_fd(Head, MT, [undefined|TV], [_HF|TF], Acc) ->
	%%should add label check here
	encode_fd(Head, MT, TV, TF, Acc);
encode_fd(Head, MT, [HV|TV], [HF|TF], Acc) ->
	FD = get_fd(MT, HF),
	Bin = gen_fd(FD, HV, <<>>),
	encode_fd(Head, MT, TV, TF, <<Acc/binary, Bin/binary>>).

gen_fd(#field_desc{type=?TYPE_MESSAGE}, [], Acc) ->
	Acc;
gen_fd(FD = #field_desc{type=?TYPE_MESSAGE}, [HV|TV], Acc) ->
	Bin = gen_fd(FD = #field_desc{type=?TYPE_MESSAGE}, HV, Acc),
	gen_fd(FD = #field_desc{type=?TYPE_MESSAGE}, TV, <<Acc/binary, Bin/binary>>);
gen_fd(FD = #field_desc{type=?TYPE_MESSAGE}, V, _Acc) ->
	BWI = eproto_codec:gen_id_wt(FD),
	[_|Values] = tuple_to_list(V),
	Fields = record_info(fields, person),
	encode_fd(BWI, FD#field_desc.type_name, Values, Fields, []);
gen_fd(FD = #field_desc{}, V, _Acc) ->
	eproto_codec:gen_basic_value(FD, V).	
