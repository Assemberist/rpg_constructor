-module(calculator).
-include("character.hrl").

-export([check_config/1]).

check_config(Objects) ->
	%% search duplicates
	uniq_objects(Objects).
	
uniq_objects() ->
	Class_names = [Obj#class.name || Obj#class{} <- Objects],
	Stat_names = [Obj#stat.name || Obj#stat{} <- Objects],
	Property_names = [Obj#property.name || Obj#property{} <- Objects],
	Action_names = [Obj#action.name || Obj#action{} <- Objects],

	lists:foldl(

	search_duplicates(Class_names, []),
	
search_duplicates([Head | []], Acc) ->
	[];

search_duplicates([Head | Tail], Acc) ->
	case lists:member(Head, Tail) of
		true -> [Head | Acc];
		false -> Acc
	end ++ search_duplicates(Tail).
