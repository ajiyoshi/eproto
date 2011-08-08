%%Generated Protobuf Codec File 
-module(address).

-export([encode/2,
	decode/2]).

-record(tour_addressbook,{person}).
-record(tour_person,{name, id, email, phone}).
-record(tour_person_phonenumber,{number, type}).

-include("eproto.hrl").
-include_lib("eunit/include/eunit.hrl").
%%====================================================================
%% API
%%====================================================================

%%encode tuple into binary
encode(MT, Rec) ->
	[_|Values] = tuple_to_list(Rec),
	Fields = get_record_info(MT),
	encode_fd(<<>>, MT, Values, Fields, <<>>).

%%decode binary into tuple
decode(MT, Bin) ->
	{FD, WT, RBin} = get_next_fd(MT, Bin),
	V = decode_fd(MT, FD, WT, RBin, []),
	make_record(MT, V).

%%====================================================================
%% Common Codec Functions
%%====================================================================
compare_element({ID1, _, _}, {ID2, _}) ->
	ID1 > ID2;
compare_element({ID1, _}, {ID2, _, _}) ->
	ID1 > ID2;
compare_element({ID1, _}, {ID2, _}) ->
	ID1 > ID2;
compare_element({ID1, SID1, _}, {ID2, SID2, _}) ->
	if 
		ID1 == ID2 -> SID1 > SID2;
		true -> ID1 > ID2
	end.

prepare_tuple_list(E = {_, _}, Acc) ->
	[E|Acc];
prepare_tuple_list({ID, _, V}, []) ->
	[{ID, [V]}];
prepare_tuple_list({ID, _, V}, [ {ID0, V0} | T]) ->
	if
		ID == ID0 -> [ {ID0, [V|V0]}|T];
		true -> [{ID, [V]}|T]
	end.

make_record(MT, L) ->
	L1 = lists:sort(fun(E1, E2) -> compare_element(E1, E2) end, L),
	L2 = lists:foldl(fun(E, Acc) -> prepare_tuple_list(E, Acc) end, [], L1),
	erlang:make_tuple(get_record_size(MT), undefind, [{1, MT} | L2]).	
	
update_repeated_fd(undefined, V) ->
	[V];
update_repeated_fd(Orig, V) ->
	[V|Orig].	

%%get current field value from bytes,
%%recursively to decode next field
get_fd_value(MT, WT, FD, Bin, Fun) ->		
	%%when field type is Message, change the MT para
	{RV, RBin} = case FD#field_desc.type of
		?TYPE_MESSAGE -> 
			{V, Bin1} = get_msg_value(FD#field_desc.type_name, WT, FD, Bin),
			{make_record(FD#field_desc.type_name, V), Bin1};
		_ ->
			eproto_codec:get_value(WT, FD, Bin)
	end,
	%%preserve order when each message decoded
	R2 = Fun(RV),
	get_next_fd_value(MT, RBin, R2).
	
get_next_fd_value(MT, Bin, R) ->
	case get_next_fd(MT, Bin) of 
		undefined -> 
			R;
		{FD, WT, Bin2} ->
			decode_fd(MT, FD, WT, Bin2, R)
	end.
	
get_msg_value(MT, WT, FD, Bin) ->
	{Bin1, RBin} = eproto_codec:get_msg_bin(WT, FD, Bin),
	{get_next_fd_value(MT, Bin1, []), RBin}.
		

%%get next field description
get_next_fd(_, <<>>) ->
	undefined;
get_next_fd(MT, Bin) ->
	{ID, WT, RBin} = eproto_codec:get_id_wt(Bin),
	{get_fd(MT, ID), WT, RBin}.

%%-----------------------------------------------------------------------

encode_fd(<<>>, _MT, [], [], Acc) ->
	Acc;
encode_fd(Head, _MT, [], [], Acc) ->
	BSize = eproto_codec:gen_varint(erlang:size(Acc)),	
	<<Head/binary, BSize/binary, Acc/binary>>;
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
	Bin = gen_fd(FD, HV, Acc),
	gen_fd(FD, TV, <<Acc/binary, Bin/binary>>);
gen_fd(FD = #field_desc{type=?TYPE_MESSAGE}, V, _Acc) ->
	BWI = eproto_codec:gen_id_wt(FD),
	[_|Values] = tuple_to_list(V),
	Fields = get_record_info(FD#field_desc.type_name),
	encode_fd(BWI, FD#field_desc.type_name, Values, Fields, <<>>);
gen_fd(FD = #field_desc{}, V, _Acc) ->
	eproto_codec:gen_basic_value(FD, V).	
	
%%====================================================================
%% Specialized Functions
%%====================================================================
get_record_size(tour_addressbook) ->
	 record_info(size, tour_addressbook);
get_record_size(tour_person) ->
	 record_info(size, tour_person);
get_record_size(tour_person_phonenumber) ->
	 record_info(size, tour_person_phonenumber).

get_record_info(tour_addressbook) ->
	 record_info(fields, tour_addressbook);
get_record_info(tour_person) ->
	 record_info(fields, tour_person);
get_record_info(tour_person_phonenumber) ->
	 record_info(fields, tour_person_phonenumber).
%------------------------------------------
get_init_msg(tour_addressbook) ->
	#tour_addressbook{};
get_init_msg(tour_person) ->
	#tour_person{};
get_init_msg(tour_person_phonenumber) ->
	#tour_person_phonenumber{}.
%------------------------------------------
preserve_order(R= #tour_addressbook{person = V}) when is_list(V)->
	R#tour_addressbook{person = lists:reverse(V)};
preserve_order(R= #tour_person{phone = V}) when is_list(V)->
	R#tour_person{phone = lists:reverse(V)};
preserve_order(R) ->
	R.
%------------------------------------------
get_fd(tour_addressbook, N) when N==1 orelse N==person ->
	#field_desc{number=1, name=person, label = 3, type=11, type_name=tour_person};
get_fd(tour_person, N) when N==4 orelse N==phone ->
	#field_desc{number=4, name=phone, label = 3, type=11, type_name=tour_person_phonenumber};
get_fd(tour_person, N) when N==3 orelse N==email ->
	#field_desc{number=3, name=email, label = 1, type=9, type_name=undefined};
get_fd(tour_person, N) when N==2 orelse N==id ->
	#field_desc{number=2, name=id, label = 2, type=5, type_name=undefined};
get_fd(tour_person, N) when N==1 orelse N==name ->
	#field_desc{number=1, name=name, label = 2, type=9, type_name=undefined};
get_fd(tour_person_phonenumber, N) when N==2 orelse N==type ->
	#field_desc{number=2, name=type, label = 1, type=14, type_name=tour_person_phonetype};
get_fd(tour_person_phonenumber, N) when N==1 orelse N==number ->
	#field_desc{number=1, name=number, label = 2, type=9, type_name=undefined}.
%------------------------------------------
decode_fd(MT = tour_addressbook, F = #field_desc{name = person}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_addressbook.person, length(R), V} | R] end);
decode_fd(MT = tour_person, F = #field_desc{name = phone}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person.phone, length(R), V} | R] end);
decode_fd(MT = tour_person, F = #field_desc{name = email}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person.email, V} | R] end);
decode_fd(MT = tour_person, F = #field_desc{name = id}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person.id, V} | R] end);
decode_fd(MT = tour_person, F = #field_desc{name = name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person.name, V} | R] end);
decode_fd(MT = tour_person_phonenumber, F = #field_desc{name = type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person_phonenumber.type, V} | R] end);
decode_fd(MT = tour_person_phonenumber, F = #field_desc{name = number}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> [{#tour_person_phonenumber.number, V} | R] end).
%------------------------------------------
