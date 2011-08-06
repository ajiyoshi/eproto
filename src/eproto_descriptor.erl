%% Author: Radiumce
%% Created: 2010-9-25
%% Description: TODO: Add description to eproto_descriptor
-module(eproto_descriptor).

-export([decode/1,
		 decode/2]).
%%
%% Include files
%%
-include("eproto.hrl").
-include("descriptor.hrl").
%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
decode(FilePath) ->	
	{ok, IODev} = file:open(FilePath, [read, binary, {encoding, latin1}]),
	Data = get_proto_binary(IODev, <<>>),
	decode(filedescriptorset, Data).

get_proto_binary(IODev, Acc) ->	
	case file:read(IODev, 64) of
		{ok, Data} ->
			get_proto_binary(IODev, <<Acc/binary, Data/binary>>);
		eof ->
			Acc
	end.	

%%decode binary into tuple
decode(MT, Bin) ->
	{FD, WT, RBin} = get_next_fd(MT, Bin),
	decode_fd(filedescriptorset, FD, WT, RBin, get_init_msg(MT)).

%%
%% Local Functions
%%
decode_fd(MT = filedescriptorset, F = #field_desc{name=file}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#filedescriptorset{file = V} end);

decode_fd(MT = filedescriptorproto, F = #field_desc{name=name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#filedescriptorproto{name = V} end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=package}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#filedescriptorproto{package = V} end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=dependency}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#filedescriptorproto{dependency = update_repeated_fd(R#filedescriptorproto.dependency, V)} 
				 end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=message_type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#filedescriptorproto{message_type = update_repeated_fd(R#filedescriptorproto.message_type, V)}
				 end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=enum_type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#filedescriptorproto{enum_type = update_repeated_fd(R#filedescriptorproto.enum_type, V)}
				 end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=service}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#filedescriptorproto{service = update_repeated_fd(R#filedescriptorproto.service, V)}
				 end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=extension}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#filedescriptorproto{extension = update_repeated_fd(R#filedescriptorproto.extension, V)}
				 end);
decode_fd(MT = filedescriptorproto, F = #field_desc{name=options}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#filedescriptorproto{options = V} end);

decode_fd(MT = descriptorproto, F = #field_desc{name=name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#descriptorproto{name = V} end);
decode_fd(MT = descriptorproto, F = #field_desc{name=field}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{field = update_repeated_fd(R#descriptorproto.field, V)}
				 end);
decode_fd(MT = descriptorproto, F = #field_desc{name=nested_type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{nested_type = update_repeated_fd(R#descriptorproto.nested_type, V)}
				 end);
decode_fd(MT = descriptorproto, F = #field_desc{name=enum_type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{enum_type = update_repeated_fd(R#descriptorproto.enum_type, V)}
				 end);
decode_fd(MT = descriptorproto, F = #field_desc{name=extension_range}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{extension_range = update_repeated_fd(R#descriptorproto.extension_range, V)}
				 end);
decode_fd(MT = descriptorproto, F = #field_desc{name=extension}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{extension = update_repeated_fd(R#descriptorproto.extension, V)}
				 end);
decode_fd(MT = descriptorproto, F = #field_desc{name=options}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, 
				 fun(V)-> R#descriptorproto{options = update_repeated_fd(R#descriptorproto.options, V)}
				 end);

decode_fd(MT = fielddescriptorproto, F = #field_desc{name=name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{name = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=number}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{number = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=extendee}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{extendee = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=label}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{label = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=type}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{type = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=type_name}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{type_name = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=default_value}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{default_value = V} end);
decode_fd(MT = fielddescriptorproto, F = #field_desc{name=options}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> R#fielddescriptorproto{options = V} end).


%%--------------------------
get_fd(filedescriptorset, N) when N==1 orelse N==file ->
	#field_desc{number=1, name=file, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=filedescriptorproto};

get_fd(filedescriptorproto, N) when N==1 orelse N==name ->
	#field_desc{number=1, name=name, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(filedescriptorproto, N) when N==2 orelse N==package ->
	#field_desc{number=2, name=package, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(filedescriptorproto, N) when N==3 orelse N==dependency ->
	#field_desc{number=3, name=dependency, label = ?LABEL_REPEATED, type=?TYPE_STRING};
get_fd(filedescriptorproto, N) when N==4 orelse N==message_type ->
	#field_desc{number=4, name=message_type, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=descriptorproto};
get_fd(filedescriptorproto, N) when N==5 orelse N==enum_type ->
	#field_desc{number=5, name=enum_type, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=enumdescriptorproto};
get_fd(filedescriptorproto, N) when N==6 orelse N==service ->
	#field_desc{number=6, name=service, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=servicedescriptorproto};
get_fd(filedescriptorproto, N) when N==7 orelse N==extension ->
	#field_desc{number=7, name=extension, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=fielddescriptorproto};
get_fd(filedescriptorproto, N) when N==8 orelse N==options ->
	#field_desc{number=8, name=options, label = ?LABEL_OPTIONAL, type=?TYPE_MESSAGE, type_name=fileoptions};


get_fd(descriptorproto, N) when N==1 orelse N==name ->
	#field_desc{number=1, name=name, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(descriptorproto, N) when N==2 orelse N==field ->
	#field_desc{number=2, name=field, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=fielddescriptorproto };
get_fd(descriptorproto, N) when N==3 orelse N==nested_type ->
	#field_desc{number=3, name=nested_type, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=descriptorproto};
get_fd(descriptorproto, N) when N==4 orelse N==enum_type ->
	#field_desc{number=4, name=enum_type, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=enumdescriptorproto};
get_fd(descriptorproto, N) when N==5 orelse N==extension_range ->
	#field_desc{number=5, name=extension_range, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=extensionrange};
get_fd(descriptorproto, N) when N==6 orelse N==extension ->
	#field_desc{number=6, name=extension, label = ?LABEL_REPEATED, type=?TYPE_MESSAGE, type_name=fielddescriptorproto};
get_fd(filedescriptorproto, N) when N==7 orelse N==options ->
	#field_desc{number=7, name=options, label = ?LABEL_OPTIONAL, type=?TYPE_MESSAGE, type_name=messageoptions};

get_fd(fielddescriptorproto, N) when N==1 orelse N==name ->
	#field_desc{number=1, name=name, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(fielddescriptorproto, N) when N==2 orelse N==extendee ->
	#field_desc{number=2, name=extendee, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(fielddescriptorproto, N) when N==3 orelse N==number ->
	#field_desc{number=3, name=number, label = ?LABEL_OPTIONAL, type=?TYPE_INT32};
get_fd(fielddescriptorproto, N) when N==4 orelse N==label ->
	#field_desc{number=4, name=label, label = ?LABEL_OPTIONAL, type=?TYPE_ENUM};
get_fd(fielddescriptorproto, N) when N==5 orelse N==type ->
	#field_desc{number=5, name=type, label = ?LABEL_OPTIONAL, type=?TYPE_ENUM};
get_fd(fielddescriptorproto, N) when N==6 orelse N==type_name ->
	#field_desc{number=6, name=type_name, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(fielddescriptorproto, N) when N==7 orelse N==default_value ->
	#field_desc{number=7, name=default_value, label = ?LABEL_OPTIONAL, type=?TYPE_STRING};
get_fd(filedescriptorproto, N) when N==8 orelse N==options ->
	#field_desc{number=8, name=options, label = ?LABEL_OPTIONAL, type=?TYPE_MESSAGE, type_name=fieldoptions};

get_fd(_MT, _N) ->
	undefined.

%%---------------------------

get_init_msg(filedescriptorset) ->
	#filedescriptorset{};
get_init_msg(filedescriptorproto) ->
	#filedescriptorproto{};
get_init_msg(descriptorproto) ->
	#descriptorproto{};
get_init_msg(fielddescriptorproto) ->
	#fielddescriptorproto{};
get_init_msg(_MT) ->
	undefined.

preserve_order(R) ->
	R.
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
	case get_fd(MT, ID) of 
		undefined -> undefined;
		FD -> {FD, WT, RBin}
	end.
	
%%==================================================================
%% Test Functions
%%==================================================================
decode_test() ->
	FilePath = "D:\\tmp\\desc.out",
	decode(FilePath).
