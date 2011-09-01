%% Author: CE
%% Created: 2010-9-20
%% Description: TODO: Add description to eproto_code_gen
-module(eproto_code_gen).

%%
%% Include files
%%
-include("eproto.hrl").
-include("descriptor.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([gen/2]).

%%
%% API Functions
%%
gen(Path, MName) ->
	Desc = eproto_descriptor:decode(Path),
	{{statements, STAcc}, {records, RDefAcc}} = visit_file_desc_set(Desc),
	gen_proto_file(STAcc, RDefAcc, MName).

%%
%% Local Functions
%%
visit_file_desc_set(Desc) ->
	FileSet = Desc#filedescriptorset.file,
	FDAcc = visit_file_descs(FileSet, []),
	gen_statements(FDAcc).
	
gen_proto_file(STAcc, RDefAcc, MName) ->
	Funs = [decode_fd, get_fd, preserv_order, get_init_msg, get_record_info],
	FLists = partition_flist(STAcc, Funs, []),
	write_file(test:get_home() ++ "address.erl", FLists, RDefAcc, MName),
	ok.

write_file(FName, FLists, RDefAcc, MName) ->
	{ok, IODev} = file:open(test:get_home() ++ "eproto.tmpl", [read]),
	Codes = read_tmpl(IODev, []),
	file:close(IODev),
	file:delete(FName),
	{ok, IODev2} = file:open(FName, [write]),
	write_file_ctt(Codes, FLists, RDefAcc, IODev2, MName),	
	file:close(IODev2),
	ok.

write_file_ctt([], _, _, _, _) ->
	io:format("INFO:generate codec source succeeded!");
write_file_ctt([Cur|T], FLists, RDefAcc, IODev, MName) ->
	Mdef = case string:str(Cur, "${module_def}") of
		0 -> nomatch;
		_ -> io:fwrite(IODev, "-module(~s).~n", [MName])
	end,
	Rdef = case string:str(Cur, "${generated_record_def}") of
		0 -> nomatch;
		_ -> write_record_def(RDefAcc, IODev)
	end,
	Fdef = case string:str(Cur, "${generated_functions}") of
		0 -> nomatch;
		_ -> write_functions(FLists, IODev)
	end,
	case {Mdef, Rdef, Fdef} of
		{nomatch, nomatch, nomatch} ->
			io:fwrite(IODev, "~s", [Cur]);
		_ ->
			ok
	end,
	write_file_ctt(T, FLists, RDefAcc, IODev, MName).

write_functions([], _) ->
	ok;
write_functions([FL|T], IODev) ->
	write_function_item(FL, IODev),
	write_functions(T, IODev).

write_function_item([], IODev) ->
	io:fwrite(IODev, "%------------------------------------------~n", []);
write_function_item([{FN, FS}|T], IODev) ->
	io:fwrite(IODev, "~s", [FS]),
	case T of
		[] -> 
			write_last_function(FN, IODev), 
			io:fwrite(IODev, ".~n", []);
		_ -> io:fwrite(IODev, ";~n", [])
	end,
	write_function_item(T, IODev).
 
write_last_function(preserv_order, IODev) ->
	io:fwrite(IODev, ";~n~s", [render_preserve_order_fun(last)]);
write_last_function(_, _) ->
	ok.
  
write_record_def([], _) ->
	ok;
write_record_def([{RName, RFDS}|T], IODev) ->
	write_record_def_item({RName, lists:reverse(RFDS)}, IODev),
	write_record_def(T, IODev).

write_record_def_item({RName, RFDS}, IODev) ->
	io:fwrite(IODev, "-record(~s,{", [RName]),
	write_record_def_item(RFDS, IODev);
write_record_def_item([], IODev) ->
	io:fwrite(IODev, "}).~n", []);
write_record_def_item([H|T], IODev) ->
	case H#fielddescriptorproto.default_value of
		undefined ->
			io:fwrite(IODev, "~s", [H#fielddescriptorproto.name]);
		V -> 
			io:fwrite(IODev, "~s = ~s", [H#fielddescriptorproto.name, V])
	end,
	case T of
		[] -> ok;
		_ -> io:fwrite(IODev, ", ", [])
	end,
	write_record_def_item(T, IODev).
	
									

read_tmpl(IODev, Acc) ->
	case io:get_line(IODev, []) of
		eof -> lists:reverse(Acc);
		{error, Reason} -> throw(Reason);
		Data -> read_tmpl(IODev, [Data|Acc])	
	end.

partition_flist(FList, [_], Acc) ->
	[FList|Acc];
partition_flist(FList, [Fun|T], Acc) ->
	{L, Rest} = lists:partition(fun({FName, _FString}) -> FName =:= Fun end, FList),
	partition_flist(Rest, T, [L|Acc]).

visit_file_descs([], FDAcc) ->
	FDAcc;
visit_file_descs([H|T], FDAcc) ->
	P = H#filedescriptorproto.package,
	M = H#filedescriptorproto.message_type,
	Acc = visit_desc([P], M, FDAcc),
	visit_file_descs(T, Acc);
visit_file_descs(D, FDAcc) ->
	visit_file_descs([D], FDAcc).

visit_desc(_,[], FDAcc) ->
	FDAcc;
visit_desc(Track, [M|T], FDAcc) ->
	Track1 = [M#descriptorproto.name|Track],
	Fields = M#descriptorproto.field,
	Acc1 = visit_fields(Track1, Fields, FDAcc),
	ExtFields = M#descriptorproto.extension,
	Acc2 = visit_fields(Track1, ExtFields, Acc1),
	NestSet = M#descriptorproto.nested_type,
	Acc3 = visit_nested_descs(Track1, NestSet, Acc2),
	visit_desc(Track, T, Acc3);
visit_desc(Track, M, FDAcc) ->
	visit_desc(Track,[M], FDAcc).
visit_nested_descs(_, undefined, Acc) ->
	Acc;
visit_nested_descs(Track, [H|T], Acc) ->
	Acc1 = visit_desc(Track, H, Acc),
	visit_nested_descs(Track, T, Acc1);
visit_nested_descs(_Track, [], Acc) ->
	Acc.
		
visit_fields(_, undefined, Acc) ->
	Acc;
visit_fields(Track, [H|T], Acc) ->
	MT = make_messgae_name(Track, []),
	visit_fields(Track, T, [{MT, H}|Acc]);	
visit_fields(_Track, [], Acc) ->
	Acc.

gen_statements(FDSet) ->
	gen_statements(FDSet, [], [], []).

gen_statements([], STAcc, RDefAcc, _Last) ->
	{{statements, STAcc}, {records, RDefAcc}};	
gen_statements([H = {MT, FD}|T], STAcc, RDefAcc, Last) ->	
	MState = if			
			  MT =:= Last -> old;
			  true -> new
			 end,
	{STAcc1, RDefAcc1} = case MState of 
							 new ->
								 ST = render_get_init_msg_fun(MT),
								 STAcc01 = [{get_init_msg, ST}|STAcc],
								 STAcc02 = [{get_record_info, render_get_record_info_fun(MT)}|STAcc01],
								 {STAcc02, [{MT, [FD]}|RDefAcc]};
							 old ->
								 case RDefAcc of
									 [{MT1, FDS}|T1] ->
										 {STAcc, [{MT1, [FD|FDS]}|T1]};
									 [] ->
										 ok
								 end
						 end,
				  
	STAcc2 = [{decode_fd, render_decode_fd_fun(H)}|STAcc1],
	STAcc3 = [{get_fd, render_get_fd_fun(H)}|STAcc2],	
	STAcc4 = case FD#fielddescriptorproto.label of
		?LABEL_REPEATED ->
			[{preserv_order, render_preserve_order_fun(H)}|STAcc3];
				 _	->
					 STAcc3
			 end,			
	gen_statements(T, STAcc4, RDefAcc1, MT).
	
				   
%%
%% Template render Functions
%%
make_messgae_name([Name], Acc) ->
	N = string:to_lower(binary_to_list(Name)),
	make_messgae_name([], [N|Acc]);
make_messgae_name([H|T], Acc) ->
	H1 = binary_to_list(H),
	N = [$_, string:to_lower(H1)],
	make_messgae_name(T, [N|Acc]);
make_messgae_name([], Acc) ->
	erlang:list_to_atom(lists:flatten(Acc)).
				   
render_decode_fd_fun({MT, FD}) ->
	Name = FD#fielddescriptorproto.name,
	io_lib:format(
"decode_fd(MT = ~s, F = #field_desc{name = ~s}, WT, Bin, R) ->
	get_fd_value(MT, WT, F, Bin, fun(V)-> ~s end)",
			 [MT, Name, render_set_expr(MT, FD)]).

render_set_expr(MT, #fielddescriptorproto{name = Name, label = ?LABEL_REPEATED}) ->
	io_lib:format(	  
"R#~s{~s = update_repeated_fd(R#~s.~s, V)}", 
	  [MT, Name, MT, Name]);
render_set_expr(MT, #fielddescriptorproto{name = Name}) ->
	io_lib:format(
	  "R#~s{~s = V}",
	  [MT, Name]).

convert_type_name(<<$.,Rest/bitstring>>, <<>>) ->
	convert_type_name(empty, Rest);
convert_type_name(empty, Rest) ->
	L = binary_to_list(Rest),
	L1 = string:to_lower(L),
	L2 = lists:foldr(fun(I, Acc) -> case I of 
								   46 -> [$_|Acc];
								   _ -> [I|Acc]
							   end
				end, [], L1),
	list_to_atom(L2);
convert_type_name(undefined, _) ->
	undefined.

render_get_fd_fun({MT, FD}) ->
	Name = FD#fielddescriptorproto.name,
	Number = FD#fielddescriptorproto.number,
	Label = FD#fielddescriptorproto.label,
	Type = FD#fielddescriptorproto.type,
	TypeName = convert_type_name(FD#fielddescriptorproto.type_name, <<>>),
	io_lib:format(
"get_fd(~s, N) when N==~p orelse N==~s ->
	#field_desc{number=~p, name=~s, label = ~p, type=~p, type_name=~s}", 
	[MT, Number, Name, Number, Name, Label, Type, TypeName]).

render_get_init_msg_fun(MT) ->
	io_lib:format(
"get_init_msg(~s) ->
	#~s{}", 
	[MT, MT]).
	
render_preserve_order_fun(last) ->
	io_lib:format(
"preserve_order(R) ->
	R", []);
render_preserve_order_fun({MT, FD}) ->
	Name = FD#fielddescriptorproto.name,
	io_lib:format(
"preserve_order(R= #~s{~s = V}) when is_list(V)->
	R#~s{~s = lists:reverse(V)}", 
	[MT, Name, MT, Name]).

render_get_record_info_fun(MT) ->
	io_lib:format(
"get_record_info(~s) ->
	 record_info(fields, ~s)",
	[MT, MT]).

%%==================================================================
%% Test Functions
%%==================================================================
gen_test() ->
	gen(test:get_home() ++ "addr_desc.out", "address"),
	ok.

tname_test() ->
	R = convert_type_name(<<".tour.Person">>, <<>>),
	?assert(R == tour_person).
	
