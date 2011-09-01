%% Author: Radiumce
%% Created: 2011-8-3
%% Description: TODO: Add description to eproto
-module(test).

-include_lib("eunit/include/eunit.hrl").
%%
%% Include files
%%
-record(tour_addressbook,{person}).
-record(tour_person,{name, id, email, phone}).
-record(tour_person_phonenumber,{number, type}).

%%
%% Exported Functions
%%
-export([decode/2,
		 encode/3,
		 ec_test/1,
		 dc_test/1,
		 get_home/0]).

%%
%% API Functions
%%
decode(MT, FilePath) ->
	{ok, IODev} = file:open(FilePath, [read, binary, {encoding, latin1}]),
	Data = get_proto_binary(IODev, <<>>),
	address:decode(MT, Data).

encode(MT, Rec, FilePath) ->
	Bin = address:encode(MT, Rec),
	file:write_file(FilePath, Bin),	
	Bin.

%%
%% Local Functions
%%
get_proto_binary(IODev, Acc) ->	
	case file:read(IODev, 64) of
		{ok, Data} ->
			get_proto_binary(IODev, <<Acc/binary, Data/binary>>);
		eof ->
			Acc
	end.	

get_home() ->
	{ok, Path} = file:get_cwd(),
	Path ++ "/".

make_test_rec() ->
	Phone = #tour_person_phonenumber{number = <<"13777486490">>, type = 0},
	Person = #tour_person{name = <<"CE">>, id = 1, email = <<"radiumce@gmail.com">>, phone = Phone},
	Person1 = #tour_person{name = <<"CE-2">>, id = 1, email = <<"radiumce@gmail.com">>, phone = Phone},
	#tour_addressbook{person = [Person, Person1]}.

calc_time() ->
	{_, T} = statistics(wall_clock),
	io:format("time_cost = ~p~n", [T]).
%%
%% Unit Test
%%
encode_test() ->
	AB = make_test_rec(),
	B = encode(tour_addressbook, AB, get_home() ++ "addressbook.bin"),
	io:format("bin_value = ~p~nsize = ~p~n", [B, byte_size(B)]),
	B.

decode_test() ->
	R = decode(tour_addressbook, get_home() ++ "addressbook.bin"),
	io:format("~p~n", [R]).

codec_test() ->
	B = encode_test(),
	R = address:decode(tour_addressbook, B),
	B2 = address:encode(tour_addressbook, R),
	io:format("~p~n~p~n~p~n", [B, R, B2]).

ec_test(N) ->
	R = make_test_rec(),
	statistics(wall_clock),
	ec_test1(N, R).
ec_test1(0, _) ->
	calc_time();
ec_test1(N, R) ->
	address:encode(tour_addressbook, R),
	ec_test1(N - 1, R).
	
dc_test(N) ->
	R = make_test_rec(),
	B = address:encode(tour_addressbook, R),
	statistics(wall_clock),
	dc_test1(N, B).
dc_test1(0, _) ->
	calc_time();
dc_test1(N, B) ->
	address:decode(tour_addressbook, B),
	dc_test1(N - 1, B).
	
	
	
